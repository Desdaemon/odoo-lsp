use std::borrow::Cow;
use std::fmt::Display;
use std::ops::Deref;

use dashmap::mapref::one::RefMut;
use dashmap::DashMap;
use futures::executor::block_on;
use lasso::{Key, Spur};
use log::{debug, error, trace, warn};
use miette::{diagnostic, IntoDiagnostic};
use qp_trie::{wrapper::BString, Trie};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Range;
use tree_sitter::{Node, Parser, QueryCursor};
use ts_macros::query;

use crate::index::{interner, Interner, Symbol, SymbolMap, SymbolSet};
use crate::str::Text;
use crate::utils::{ts_range_to_lsp_range, ByteOffset, ByteRange, Erase, MinLoc, RangeExt, TryResultExt, Usage};
use crate::{format_loc, ImStr};

#[derive(Clone, Debug)]
pub struct Model {
	pub type_: ModelType,
	pub range: Range,
	pub byte_range: ByteRange,
}

#[derive(Clone, Debug)]
pub enum ModelType {
	Base { name: ImStr, ancestors: Vec<ImStr> },
	Inherit(Vec<ImStr>),
}

#[derive(Default)]
pub struct ModelIndex {
	inner: DashMap<ModelName, ModelEntry>,
	pub by_prefix: RwLock<Trie<BString, ModelName>>,
}

pub type ModelName = Symbol<ModelEntry>;

#[derive(Default)]
pub struct ModelEntry {
	pub base: Option<ModelLocation>,
	pub descendants: Vec<ModelLocation>,
	pub ancestors: SymbolSet<ModelEntry>,
	pub fields: Option<SymbolMap<FieldName, Field>>,
	pub fields_set: qp_trie::Trie<BString, ()>,
	pub docstring: Option<Text>,
}

pub enum FieldName {}

#[derive(Clone, Debug)]
pub enum FieldKind {
	Value,
	Relational(Spur),
	Related(ImStr),
}

#[derive(Clone, Debug)]
pub struct Field {
	pub kind: FieldKind,
	pub type_: Spur,
	pub location: MinLoc,
	pub help: Option<Text>,
}

#[derive(Clone)]
pub struct ModelLocation(pub MinLoc, pub ByteRange);

impl Display for ModelLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.0.range.start.line == self.0.range.end.line {
			write!(
				f,
				"{} [{}:{}..{}]",
				interner().resolve(&self.0.path),
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.character,
			)
		} else {
			write!(
				f,
				"{} [{}:{}..{}:{}]",
				interner().resolve(&self.0.path),
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.line,
				self.0.range.end.character,
			)
		}
	}
}

impl Deref for ModelIndex {
	type Target = DashMap<ModelName, ModelEntry>;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

query! {
	ModelFields(Field, Type, Relation, Arg, Value);
r#"
((class_definition
	(block
		(expression_statement
			(assignment
				(identifier) @FIELD
				(call [
					(identifier) @TYPE
					(attribute (identifier) @_fields (identifier) @TYPE)
				] (argument_list . (string)? @RELATION
					((keyword_argument (identifier) @ARG (_) @VALUE) ","?)*))))))
(#eq? @_fields "fields")
(#match? @TYPE "^[A-Z]"))"#
}

impl ModelIndex {
	pub async fn append(&self, path: Spur, interner: &Interner, replace: bool, items: &[Model]) {
		let mut by_prefix = self.by_prefix.write().await;
		for item in items {
			match &item.type_ {
				ModelType::Base { name: base, ancestors } => {
					let name = interner.get_or_intern(base).into();
					by_prefix.insert_str(base, name);
					let mut entry = self.entry(name).or_default();
					if entry.base.is_none() || replace {
						entry.base = Some(ModelLocation(
							MinLoc {
								path,
								range: item.range,
							},
							item.byte_range.clone(),
						));
						entry.ancestors.extend(
							ancestors
								.into_iter()
								.map(|sym| ModelName::from(interner.get_or_intern(&sym))),
						);
					} else {
						warn!(
							"Conflicting bases:\nfirst={}\n  new={}",
							entry.base.as_ref().unwrap(),
							ModelLocation(
								MinLoc {
									path,
									range: item.range
								},
								item.byte_range.clone()
							)
						)
					}
				}
				ModelType::Inherit(inherits) => {
					if replace {
						for inherit in inherits {
							let Some(inherit) = interner.get(inherit) else { continue };
							if let Some(mut entry) = self.try_get_mut(&inherit.into()).expect(format_loc!("deadlock")) {
								entry.descendants.retain(|loc| loc.0.path != path)
							}
						}
					}
					if let Some((primary, ancestors)) = inherits.split_first() {
						let inherit = interner.get_or_intern(primary).into();
						let mut entry = self.entry(inherit).or_default();
						entry.descendants.push(ModelLocation(
							MinLoc {
								path,
								range: item.range,
							},
							item.byte_range.clone(),
						));
						entry
							.ancestors
							.extend(ancestors.iter().map(|sym| ModelName::from(interner.get_or_intern(sym))));
					}
				}
			}
		}
	}
	pub fn populate_field_names<'model>(
		&'model self,
		model: ModelName,
		locations_filter: &'model [Spur],
	) -> Option<RefMut<'model, ModelName, ModelEntry>> {
		let model_name = interner().resolve(&model);
		let mut entry = self.try_get_mut(&model).expect(format_loc!("deadlock"))?;
		if entry.fields.is_some() && locations_filter.is_empty() {
			return Some(entry);
		}
		let t0 = std::time::Instant::now();
		let locations = entry.base.iter().chain(&entry.descendants).cloned().collect::<Vec<_>>();

		let query = ModelFields::query();
		let fields = locations
			.into_par_iter()
			.filter_map(|ModelLocation(location, byte_range)| {
				if !locations_filter.is_empty() && !locations_filter.contains(&location.path) {
					return None;
				}
				let mut fields = vec![];
				let fpath = interner().resolve(&location.path);
				let contents = std::fs::read(fpath)
					.map_err(|err| error!("Failed to read {fpath}:\n{err}"))
					.ok()?;
				let mut parser = Parser::new();
				parser
					.set_language(tree_sitter_python::language())
					.expect(format_loc!("Failed to set language"));
				let ast = parser.parse(&contents, None)?;
				let byte_range = byte_range.erase();
				let mut cursor = QueryCursor::new();
				cursor.set_byte_range(byte_range);
				for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
					let mut field = None;
					let mut type_ = None;
					let mut is_relational = false;
					let mut relation = None;
					let mut kwarg = None::<Kwargs>;
					let mut help = None;
					let mut related = None;
					enum Kwargs {
						ComodelName,
						Help,
						Related,
					}
					for capture in match_.captures {
						match ModelFields::from(capture.index) {
							Some(ModelFields::Field) => {
								field = Some(capture.node);
							}
							Some(ModelFields::Type) => {
								type_ = Some(capture.node.byte_range());
								// TODO: fields.Reference
								is_relational = matches!(
									&contents[capture.node.byte_range()],
									b"One2many" | b"Many2one" | b"Many2many"
								);
							}
							Some(ModelFields::Relation) => {
								if is_relational {
									relation = Some(capture.node.byte_range().shrink(1));
								}
							}
							Some(ModelFields::Arg) => match &contents[capture.node.byte_range()] {
								b"comodel_name" if is_relational => kwarg = Some(Kwargs::ComodelName),
								b"help" => kwarg = Some(Kwargs::Help),
								b"related" => kwarg = Some(Kwargs::Related),
								_ => kwarg = None,
							},
							Some(ModelFields::Value) => match kwarg {
								Some(Kwargs::ComodelName) => {
									if capture.node.kind() == "string" {
										relation = Some(capture.node.byte_range().shrink(1));
									}
								}
								Some(Kwargs::Help) => {
									if matches!(capture.node.kind(), "string" | "concatenated_string") {
										help = Some(parse_help(&capture.node, &contents));
									}
								}
								Some(Kwargs::Related) => {
									if capture.node.kind() == "string" {
										related = Some(capture.node.byte_range().shrink(1));
									}
								}
								None => {}
							},
							None => {}
						}
					}
					if let (Some(field), Some(type_)) = (field, type_) {
						let range = ts_range_to_lsp_range(field.range());
						let field_str = String::from_utf8_lossy(&contents[field.byte_range()]);
						let field = interner().get_or_intern(&field_str);
						let type_ = String::from_utf8_lossy(&contents[type_]);
						let location = MinLoc {
							path: location.path,
							range,
						};
						let help = help.map(|help| Text::try_from(help.as_ref()).unwrap());
						let kind = if let Some(relation) = relation {
							let relation = String::from_utf8_lossy(&contents[relation]);
							let relation = interner().get_or_intern(&relation);
							FieldKind::Relational(relation)
						} else if let Some(related) = related {
							FieldKind::Related(String::from_utf8_lossy(&contents[related]).as_ref().into())
						} else {
							if is_relational {
								debug!("is_relational but no relation found: field={field_str} type={type_}");
							}
							FieldKind::Value
						};
						let type_ = interner().get_or_intern(&type_);
						fields.push((
							field,
							Field {
								kind,
								type_,
								location,
								help,
							},
						))
					}
				}
				Some(fields)
			})
			.flatten_iter();

		let ancestors = entry.ancestors.iter().collect::<Vec<_>>();
		let mut out = entry.fields.take().unwrap_or_default();
		let mut fields_set = core::mem::take(&mut entry.fields_set);

		// drop to prevent deadlock
		drop(entry);

		// recursively get or populate ancestors' fields
		for ancestor in ancestors {
			if let Some(entry) = self.populate_field_names(ancestor, locations_filter) {
				if let Some(fields) = entry.fields.as_ref() {
					// TODO: Implement copy-on-write to increase reuse
					out.extend(fields.iter().map(|(key, val)| (key.clone(), val.clone())));
				}
			}
		}

		for (key, type_) in fields.collect::<Vec<_>>() {
			out.insert_checked(key.into_usize() as u64, type_);
			fields_set.insert_str(interner().resolve(&key), ());
		}

		log::info!(
			"[{}] Populated {} fields in {}ms",
			model_name,
			out.len(),
			t0.elapsed().as_millis()
		);
		let mut entry = self.try_get_mut(&model).expect(format_loc!("deadlock")).unwrap();
		entry.fields = Some(out);
		entry.fields_set = fields_set;
		Some(entry)
	}
	/// For completing a `Model.write({'foo.bar.baz': ..})`:
	/// - `model` is the key of `Model`
	/// - `needle` is a left substring of 'foo.bar.baz'
	/// - `range` spans the entire range of 'foo.bar.baz'
	///
	/// Returns None if resolution fails before `needle` is exhausted.
	pub fn resolve_mapped(&self, model: &mut Spur, needle: &mut &str, mut range: Option<&mut ByteRange>) -> Option<()> {
		while let Some((lhs, rhs)) = needle.split_once('.') {
			trace!("mapped `{needle}`");
			// lhs: foo
			// rhs: ba
			let fields = self.populate_field_names(model.clone().into(), &[])?;
			let field = interner().get(&lhs);
			let field = field.and_then(|field| fields.fields.as_ref()?.get(&field.into()));
			let Some(FieldKind::Relational(rel)) = field.as_ref().map(|f| &f.kind) else {
				*needle = lhs;
				if let Some(range) = range.as_mut() {
					let end = range.start.0 + lhs.len();
					**range = range.start..ByteOffset(end);
				}
				return None;
			};
			*needle = rhs;
			*model = rel.clone();
			// old range: foo.bar.baz
			// range:         bar.baz
			if let Some(range) = range.as_mut() {
				let start = range.start.0 + lhs.len() + 1;
				**range = ByteOffset(start)..range.end;
			}
		}
		Some(())
	}
	pub fn normalize_field_relation(&self, field: Symbol<FieldName>, model: Spur) -> Option<Spur> {
		let model_entry = self.get(&model.into())?;
		let field_entry = model_entry.fields.as_ref()?.get(&field)?;
		let mut kind = field_entry.kind.clone();
		let mut field_model = model.clone();
		if let FieldKind::Related(related) = &field_entry.kind {
			trace!(
				"related={related} field={} model={}",
				interner().resolve(&field),
				interner().resolve(&model)
			);
			let related = related.clone();
			let mut related = related.as_str();
			drop(model_entry);
			if self.resolve_mapped(&mut field_model, &mut related, None).is_some() {
				kind = FieldKind::Relational(field_model.into());
				let mut model_entry = self.try_get_mut(&model.into()).expect(format_loc!("deadlock"))?;
				model_entry.fields.as_mut()?.get_mut(&field)?.kind = kind.clone();
			} else {
				warn!("(normalize_field_kind) failed to normalize {related}");
			}
		}

		match kind {
			FieldKind::Relational(rel) => Some(rel.clone()),
			FieldKind::Value => None,
			FieldKind::Related(_) => None,
		}
	}
	pub(crate) fn statistics(&self) -> serde_json::Value {
		let Self { inner, by_prefix } = self;
		serde_json::json! {{
			"entries": inner.usage(),
			"by_prefix": block_on(by_prefix.read()).usage(),
		}}
	}
}

query! {
	ModelHelp(Docstring);
r#"
(class_definition
  (block .
    (expression_statement
      (string
        ((string_start) . (string_content) @DOCSTRING)))))"#
}

impl ModelEntry {
	pub async fn resolve_details(&mut self) -> miette::Result<()> {
		let Some(ModelLocation(loc, byte_range)) = &self.base else {
			return Ok(());
		};
		if self.docstring.is_none() {
			let contents = tokio::fs::read(interner().resolve(&loc.path)).await.into_diagnostic()?;
			let mut parser = Parser::new();
			parser.set_language(tree_sitter_python::language()).into_diagnostic()?;
			let ast = parser
				.parse(&contents, None)
				.ok_or_else(|| diagnostic!("AST not parsed"))?;
			let query = ModelHelp::query();
			let mut cursor = QueryCursor::new();
			cursor.set_byte_range(byte_range.erase());
			'docstring: for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
				if let Some(docstring) = match_.nodes_for_capture_index(0).next() {
					let contents = String::from_utf8_lossy(&contents[docstring.byte_range()]);
					self.docstring = Some(Text::try_from(contents.trim()).into_diagnostic()?);
					break 'docstring;
				}
			}
		}

		Ok(())
	}
}

/// `node` must be `[(string) (concatenated_string)]`
fn parse_help<'text>(node: &Node, contents: &'text [u8]) -> Cow<'text, str> {
	let mut cursor = node.walk();
	match node.kind() {
		"string" => {
			let content = node
				.children(&mut cursor)
				.find_map(|child| (child.kind() == "string_content").then(|| &contents[child.byte_range()]));
			String::from_utf8_lossy(content.unwrap_or(&[]))
		}
		"concatenated_string" => {
			let mut content = vec![];
			for string in node.children(&mut cursor) {
				if string.kind() == "string" {
					let mut cursor = string.walk();
					let children = string.children(&mut cursor).find_map(|child| {
						(child.kind() == "string_content").then(|| {
							String::from_utf8_lossy(&contents[child.byte_range()])
								.trim()
								.replace("\\n", "  \n")
								.replace("\\t", "\t")
						})
					});
					content.extend(children);
				}
			}
			Cow::from(content.join(" "))
		}
		_ => unreachable!(),
	}
}
