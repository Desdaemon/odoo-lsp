use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Deref;
use std::sync::Arc;

use dashmap::mapref::one::RefMut;
use dashmap::DashMap;
use lasso::Spur;
use miette::{diagnostic, Diagnostic, IntoDiagnostic};
use qp_trie::Trie;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use smart_default::SmartDefault;
use tokio::sync::RwLock;
use tower_lsp_server::lsp_types::Range;
use tracing::{debug, error, info, trace, warn};
use tree_sitter::{Node, Parser, QueryCursor};
use ts_macros::query;

use crate::index::{interner, Interner, PathSymbol, Symbol};
use crate::str::Text;
use crate::utils::{ts_range_to_lsp_range, ByteOffset, ByteRange, Erase, MinLoc, RangeExt, TryResultExt};
use crate::{format_loc, test_utils, ImStr};

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

impl ModelType {
	/// NOTE: For testing only, this function deliberatetely leaks memory to make it
	/// easier to pattern-match.
	#[cfg(test)]
	pub fn splay(&self) -> (Option<&str>, &[&str]) {
		match self {
			Self::Base { name, ancestors } => (
				Some(name.as_str()),
				Box::leak(ancestors.iter().map(|a| a.as_str()).collect::<Box<[_]>>()),
			),
			Self::Inherit(ancestors) => (
				None,
				Box::leak(ancestors.iter().map(|a| a.as_str()).collect::<Box<[_]>>()),
			),
		}
	}
}

#[derive(SmartDefault)]
pub struct ModelIndex {
	#[default(_code = "DashMap::with_shard_amount(4)")]
	inner: DashMap<ModelName, ModelEntry>,
	pub by_prefix: RwLock<Trie<ImStr, ModelName>>,
}

pub type ModelName = Symbol<ModelEntry>;

#[derive(Default)]
pub struct ModelEntry {
	pub base: Option<ModelLocation>,
	pub descendants: Vec<ModelLocation>,
	pub ancestors: Vec<ModelName>,
	pub fields: Option<HashMap<Symbol<Field>, Arc<Field>>>,
	pub methods: Option<HashMap<Symbol<Method>, Arc<Method>>>,
	pub properties_by_prefix: qp_trie::Trie<&'static [u8], PropertyKind>,
	pub docstring: Option<Text>,
}

#[derive(Clone, Debug)]
pub enum FieldKind {
	Value,
	Relational(Spur),
	Related(ImStr),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PropertyKind {
	Field,
	Method,
}

#[derive(Clone, Debug)]
pub struct Field {
	pub kind: FieldKind,
	pub type_: Spur,
	pub location: MinLoc,
	pub help: Option<Text>,
}

#[derive(Clone, Debug)]
pub struct Method {
	pub return_type: MethodReturnType,
	pub locations: Vec<MinLoc>,
	pub docstring: Option<Text>,
}

#[derive(Clone, Debug, Default)]
pub enum MethodReturnType {
	#[default]
	Unprocessed,
	/// Set to prevent recursion
	Processing,
	Value,
	Relational(Symbol<ModelEntry>),
}

impl Field {
	pub fn merge<'this>(self: &'this mut Arc<Self>, other: &Self) -> &'this mut Self {
		let self_ = Arc::make_mut(self);
		let Self {
			kind,
			type_,
			location,
			help,
		} = other;
		debug!("TODO Field inheritance location {location}");
		match &mut self_.kind {
			FieldKind::Value | FieldKind::Related(_) => self_.kind = kind.clone(),
			FieldKind::Relational(_) => {}
		}
		self_.type_ = *type_;
		if let Some(help) = help {
			self_.help = Some(help.clone());
		}
		self_
	}
}

impl Method {
	pub fn add_override(self: &mut Arc<Self>, location: MinLoc) {
		let self_ = Arc::make_mut(self);
		if let Some(loc) = self_.locations.iter_mut().find(|loc| loc.path == location.path) {
			loc.range = location.range;
		} else {
			self_.locations.push(location);
		}
	}
	pub fn merge(self: &mut Arc<Self>, other: &Self) {
		let self_ = Arc::make_mut(self);
		if other.docstring.is_some() {
			self_.docstring = other.docstring.clone();
		}
		if self_.locations.is_empty() {
			self_.locations.clone_from(&other.locations);
			return;
		}

		let mut ranges_by_locations = HashMap::<_, Vec<_>>::new();
		for loc in other.locations.iter() {
			ranges_by_locations.entry(loc.path).or_default().push(loc.range);
		}
		for (path, ranges) in ranges_by_locations {
			let mut first = None;
			let mut last = None;
			for (idx, loc) in self_.locations.iter().enumerate() {
				if loc.path == path {
					if first.is_none() {
						first = Some(idx);
					}
					last = Some(idx);
				} else if first.is_some() {
					break;
				}
			}
			let (Some(first), Some(last)) = (first, last) else {
				(self_.locations).extend(ranges.into_iter().map(|range| MinLoc { path, range }));
				continue;
			};
			(self_.locations).splice(first..=last, ranges.into_iter().map(|range| MinLoc { path, range }));
		}
	}
}

#[derive(Clone)]
pub struct ModelLocation(pub MinLoc, pub ByteRange);

impl Display for ModelLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}:{}:{}",
			self.0.path,
			self.0.range.start.line + 1,
			self.0.range.start.character + 1,
		)
	}
}

impl Deref for ModelIndex {
	type Target = DashMap<ModelName, ModelEntry>;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

#[rustfmt::skip]
query! {
	ModelProperties(Field, Type, Relation, Arg, Value, Method, MethodBody);
((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @FIELD
        (call [
          (identifier) @TYPE
          (attribute (identifier) @_fields (identifier) @TYPE) ]
          (argument_list . ((comment)* . (string) @RELATION)?
            ((keyword_argument (identifier) @ARG (_) @VALUE) ","?)*))))))
  (#eq? @_fields "fields")
  (#match? @TYPE "^[A-Z]"))

(class_definition
  (block [
    (function_definition (identifier) @METHOD) @METHOD_BODY
    (decorated_definition
      (function_definition (identifier) @METHOD) @METHOD_BODY) ]))
}

#[derive(Diagnostic, thiserror::Error, Debug)]
pub enum ResolveMappedError {
	#[error("Tried to access a field on a non-relational field")]
	NonRelational,
}

impl ModelIndex {
	pub async fn append(&self, path: PathSymbol, interner: &Interner, replace: bool, items: &[Model]) {
		let mut by_prefix = self.by_prefix.write().await;
		for item in items {
			match &item.type_ {
				ModelType::Base { name: base, ancestors } => {
					let name = interner.get_or_intern(base).into();
					by_prefix.insert(base.clone(), name);
					let mut entry = self.entry(name).or_default();
					if entry.base.is_none() || replace {
						entry.base = Some(ModelLocation(
							MinLoc {
								path,
								range: item.range,
							},
							item.byte_range.clone(),
						));
						entry
							.ancestors
							.extend(ancestors.iter().map(|sym| ModelName::from(interner.get_or_intern(sym))));
					} else {
						warn!(
							"Conflicting bases for {}:\nfirst={}\n  new={}",
							interner.resolve(&name),
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
							if let Some(mut entry) = self.get_mut(&inherit.into()) {
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
	/// Recursively traverses this model's definitions and populates all of its properties, including fields and methods.
	///
	/// `locations_filter` can be set to an empty slice to search all definitions, or a list of specific paths to search.
	///
	/// Deadlocks if an entry in [`ModelIndex`] is being held with the key `model`.
	pub fn populate_properties<'model>(
		&'model self,
		model: ModelName,
		locations_filter: &[PathSymbol],
	) -> Option<RefMut<'model, ModelName, ModelEntry>> {
		let model_name = interner().resolve(&model);
		let mut entry = self.try_get_mut(&model).expect(format_loc!("deadlock"))?;
		if entry.fields.is_some() && entry.methods.is_some() && locations_filter.is_empty() {
			return Some(entry);
		}
		let t0 = std::time::Instant::now();
		let locations = entry.base.iter().chain(&entry.descendants).cloned().collect::<Vec<_>>();

		let query = ModelProperties::query();
		let iter = locations
			.into_par_iter()
			.filter_map(|ModelLocation(location, byte_range)| {
				if !locations_filter.is_empty() && !locations_filter.contains(&location.path) {
					return None;
				}
				let mut fields = vec![];
				let mut methods = vec![];
				let fpath = location.path.to_path();
				let contents = test_utils::fs::read(&fpath)
					.map_err(|err| error!("Failed to read {}:\n{err}", fpath.display()))
					.ok()?;
				let mut parser = Parser::new();
				parser
					.set_language(&tree_sitter_python::LANGUAGE.into())
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
					let mut method_name = None;
					let mut method_body = None;
					for capture in match_.captures {
						match ModelProperties::from(capture.index) {
							Some(ModelProperties::Field) => {
								field = Some(capture.node);
							}
							Some(ModelProperties::Type) => {
								type_ = Some(capture.node.byte_range());
								// TODO: fields.Reference
								is_relational = matches!(
									&contents[capture.node.byte_range()],
									b"One2many" | b"Many2one" | b"Many2many"
								);
							}
							Some(ModelProperties::Relation) => {
								if is_relational {
									relation = Some(capture.node.byte_range().shrink(1));
								}
							}
							Some(ModelProperties::Arg) => match &contents[capture.node.byte_range()] {
								b"comodel_name" if is_relational => kwarg = Some(Kwargs::ComodelName),
								b"help" => kwarg = Some(Kwargs::Help),
								b"related" => kwarg = Some(Kwargs::Related),
								_ => kwarg = None,
							},
							Some(ModelProperties::Value) => match kwarg {
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
							Some(ModelProperties::Method) => {
								method_name = Some(capture.node);
							}
							Some(ModelProperties::MethodBody) => {
								method_body = Some(capture.node);
							}
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
					if let (Some(method), Some(body)) = (method_name, method_body) {
						let method_str = String::from_utf8_lossy(&contents[method.byte_range()]);
						let method = interner().get_or_intern(&method_str);
						let range = ts_range_to_lsp_range(body.range());
						methods.push((
							method,
							MinLoc {
								path: location.path,
								range,
							},
						))
					}
				}
				Some((fields, methods))
			});

		let ancestors = entry.ancestors.to_vec();
		let mut out_fields = entry.fields.take().unwrap_or_default();
		let mut out_methods = entry.methods.take().unwrap_or_default();
		let mut properties_set = core::mem::take(&mut entry.properties_by_prefix);

		// drop to prevent deadlock
		drop(entry);

		// recursively get or populate ancestors' properties
		for ancestor in ancestors {
			if let Some(entry) = self.populate_properties(ancestor, locations_filter) {
				if let Some(fields) = entry.fields.as_ref() {
					for (name, field) in fields {
						properties_set.insert(interner().resolve(name).as_bytes(), PropertyKind::Field);
						match out_fields.entry(*name) {
							Entry::Occupied(mut old_field) => {
								old_field.get_mut().merge(field);
							}
							Entry::Vacant(empty) => {
								empty.insert(field.clone());
							}
						}
					}
				}
				if let Some(methods) = entry.methods.as_ref() {
					for (name, method) in methods {
						properties_set.insert(interner().resolve(name).as_bytes(), PropertyKind::Method);
						match out_methods.entry(*name) {
							Entry::Occupied(mut old_method) => {
								old_method.get_mut().merge(method);
							}
							Entry::Vacant(empty) => {
								empty.insert(method.clone());
							}
						}
					}
				}
			}
		}

		let (fields, methods): (Vec<_>, Vec<_>) = iter.collect();

		for (key, field) in fields.into_iter().flatten() {
			match out_fields.entry(key.into()) {
				Entry::Occupied(mut old_field) => {
					old_field.get_mut().merge(&field);
				}
				Entry::Vacant(empty) => {
					empty.insert(field.into());
				}
			}
			properties_set.insert(interner().resolve(&key).as_bytes(), PropertyKind::Field);
		}

		for (key, method_location) in methods.into_iter().flatten() {
			match out_methods.entry(key.into()) {
				Entry::Occupied(mut old_method) => {
					old_method.get_mut().add_override(method_location);
				}
				Entry::Vacant(empty) => {
					empty.insert(
						Method {
							return_type: Default::default(),
							locations: vec![method_location],
							docstring: None,
						}
						.into(),
					);
				}
			}
			properties_set.insert(interner().resolve(&key).as_bytes(), PropertyKind::Method);
		}

		info!(
			"{model_name}: {} fields, {} methods, {}ms",
			out_fields.len(),
			out_methods.len(),
			t0.elapsed().as_millis()
		);
		let mut entry = self.try_get_mut(&model).expect(format_loc!("deadlock")).unwrap();
		entry.fields = Some(out_fields);
		entry.methods = Some(out_methods);
		entry.properties_by_prefix = properties_set;
		Some(entry)
	}
	/// Splits a mapped access expression, e.g. `foo.bar.baz`, and traverses until the expression is exhausted.
	///
	/// For completing a `Model.write({'foo.bar.baz': ..})`:
	/// - `model` is the key of `Model`
	/// - `needle` is a left-wise substring of `foo.bar.baz`
	/// - `range` spans the entire range of `foo.bar.baz`
	///
	/// Returns an error if resolution fails before `needle` is exhausted.
	pub fn resolve_mapped(
		&self,
		model: &mut Spur,
		needle: &mut &str,
		mut range: Option<&mut ByteRange>,
	) -> Result<(), ResolveMappedError> {
		while let Some((lhs, rhs)) = needle.split_once('.') {
			trace!("(resolved_mapped) `{needle}` model=`{}`", interner().resolve(model));
			let mut resolved = interner()
				.get(lhs)
				.and_then(|key| self.resolve_related_field(key.into(), *model));
			if let Some(normalized) = &resolved {
				trace!("(resolved_mapped) prenormalized: {}", interner().resolve(normalized));
			}
			// lhs: foo
			// rhs: ba
			if resolved.is_none() {
				let Some(model_entry) = self.populate_properties((*model).into(), &[]) else {
					debug!(
						"tried to resolve before fields are populated for `{}`",
						interner().resolve(model)
					);
					return Ok(());
				};
				let field = interner().get(lhs);
				let field = field.and_then(|field| model_entry.fields.as_ref()?.get(&field.into()));
				match field.as_ref().map(|f| &f.kind) {
					Some(FieldKind::Relational(rel)) => resolved = Some(*rel),
					None | Some(FieldKind::Value) => return Err(ResolveMappedError::NonRelational),
					Some(FieldKind::Related(..)) => {
						drop(model_entry);
						resolved = self.resolve_related_field(interner().get(lhs).unwrap().into(), *model);
					}
				}
			}
			let Some(rel) = resolved else {
				warn!("unresolved field `{}`.`{lhs}`", interner().resolve(model));
				*needle = lhs;
				if let Some(range) = range.as_mut() {
					let end = range.start.0 + lhs.len();
					**range = range.start..ByteOffset(end);
				}
				return Err(ResolveMappedError::NonRelational);
			};
			*needle = rhs;
			*model = rel;
			// old range: foo.bar.baz
			// range:         bar.baz
			if let Some(range) = range.as_mut() {
				let start = range.start.0 + lhs.len() + 1;
				**range = ByteOffset(start)..range.end;
			}
		}
		Ok(())
	}
	/// Turns related fields ([`FieldKind::Related`]) into concrete fields, and return the field's type itself if successful.
	///
	/// Deadlocks if an entry in [`ModelIndex`] is held with the key `model`.
	#[must_use = "normalized relation might not have been updated back to the central index"]
	pub fn resolve_related_field(&self, field: Symbol<Field>, model: Spur) -> Option<Spur> {
		// Why populate?
		// If we came from a long chain of relations, we might encounter a field on a model
		// that hasn't been populated yet. This is because we only populate fields when they're
		// accessed. So we need to populate the fields of the model we're currently on.
		// It's a no-op if the fields are already populated.
		// If a stack overflow occurs, check populate_properties.
		let entry = self.populate_properties(model.into(), &[])?;
		let field_entry = entry.fields.as_ref()?.get(&field)?;
		let mut kind = field_entry.kind.clone();
		let mut field_model = model;
		if let FieldKind::Related(related) = &field_entry.kind {
			trace!(
				"(normalize_field_relation) related={related} field={} model={}",
				interner().resolve(&field),
				interner().resolve(&model)
			);
			let related = related.clone();
			let mut related = related.as_str();
			drop(entry);
			if self.resolve_mapped(&mut field_model, &mut related, None).is_ok() {
				// resolved_mapped took us to the final field, now we need to resolve it to a model
				let related_key = interner().get(related)?;
				let field_model = self.resolve_related_field(related_key.into(), field_model)?;

				kind = FieldKind::Relational(field_model);
				let mut model_entry = self.try_get_mut(&model.into()).expect(format_loc!("deadlock"))?;
				let Some(field) = Arc::get_mut(model_entry.fields.as_mut()?.get_mut(&field)?) else {
					// Field is already used elsewhere, don't modify it.
					return Some(field_model);
				};
				field.kind = kind.clone();
			} else {
				warn!("failed to normalize {related}");
			}
		}

		match kind {
			FieldKind::Relational(rel) => Some(rel),
			FieldKind::Value => None,
			FieldKind::Related(_) => None,
		}
	}
}

#[rustfmt::skip]
query! {
	ModelHelp(Docstring);
(class_definition
  (block .
    (expression_statement
      (string
        ((string_start) . (string_content) @DOCSTRING)))))
}

impl ModelEntry {
	pub fn resolve_details(&mut self) -> miette::Result<()> {
		let Some(ModelLocation(loc, byte_range)) = &self.base else {
			return Ok(());
		};
		if self.docstring.is_none() {
			let contents = std::fs::read(loc.path.to_path()).into_diagnostic()?;
			let mut parser = Parser::new();
			parser
				.set_language(&tree_sitter_python::LANGUAGE.into())
				.into_diagnostic()?;
			let ast = parser
				.parse(&contents, None)
				.ok_or_else(|| diagnostic!("AST not parsed"))?;
			let query = ModelHelp::query();
			let mut cursor = QueryCursor::new();
			cursor.set_byte_range(byte_range.erase());
			for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
				if let Some(docstring) = match_.nodes_for_capture_index(0).next() {
					let contents = String::from_utf8_lossy(&contents[docstring.byte_range()]);
					self.docstring = Some(Text::try_from(contents.trim()).into_diagnostic()?);
					return Ok(());
				}
			}
			self.docstring = Some(Text::try_from("").unwrap());
		}

		Ok(())
	}
	pub fn prop_kind(&self, prop: Spur) -> Option<PropertyKind> {
		if self
			.fields
			.as_ref()
			.is_some_and(|fields| fields.contains_key(&prop.into()))
		{
			Some(PropertyKind::Field)
		} else if self
			.methods
			.as_ref()
			.is_some_and(|methods| methods.contains_key(&prop.into()))
		{
			Some(PropertyKind::Method)
		} else {
			None
		}
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

#[cfg(test)]
mod tests {
	use pretty_assertions::assert_eq;
	use std::collections::HashSet;
	use tree_sitter::{Parser, QueryCursor};

	use crate::{
		index::{interner, ModelQuery},
		test_utils::cases::foo::{prepare_foo_index, FOO_PY},
	};

	fn clamp_str(str: &str) -> &str {
		if str.len() > 10 {
			&str[..10]
		} else {
			str
		}
	}

	#[test]
	fn test_model_query() {
		let query = ModelQuery::query();
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let ast = parser.parse(FOO_PY, None).unwrap();
		let matches = QueryCursor::new()
			.matches(query, ast.root_node(), FOO_PY)
			.map(|match_| {
				(match_.captures.iter())
					.map(|cap| {
						(
							ModelQuery::from(cap.index),
							clamp_str(unsafe { core::str::from_utf8_unchecked(&FOO_PY[cap.node.byte_range()]) }),
						)
					})
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		let matches = matches.iter().map(|match_| &match_[..]).collect::<Vec<_>>();
		use ModelQuery as T;
		assert_eq!(
			matches.as_slice(),
			[
				[
					(Some(T::Model), "class Foo("),
					(None, "Model"),
					(Some(T::Name), "_name"),
				],
				[
					(Some(T::Model), "class Bar("),
					(None, "Model"),
					(Some(T::Name), "_name"),
				],
				[
					(Some(T::Model), "class Bar("),
					(None, "Model"),
					(Some(T::Name), "_inherit"),
				],
				[
					(Some(T::Model), "class Quux"),
					(None, "Model"),
					(Some(T::Name), "_name"),
				],
				[
					(Some(T::Model), "class Quux"),
					(None, "Model"),
					(Some(T::Name), "_inherit"),
				]
			]
		);
	}

	#[test]
	fn test_populate_properties() {
		let index = prepare_foo_index();

		let foo = index
			.populate_properties(interner().get_or_intern_static("foo").into(), &[])
			.unwrap();

		assert_eq!(
			foo.fields
				.as_ref()
				.unwrap()
				.keys()
				.next()
				.map(|sym| interner().resolve(sym)),
			Some("bar")
		);
		drop(foo);

		let bar = index
			.populate_properties(interner().get_or_intern_static("bar").into(), &[])
			.unwrap();

		let bar_fields = bar
			.fields
			.as_ref()
			.unwrap()
			.keys()
			.map(|sym| interner().resolve(sym))
			.collect::<HashSet<_>>();
		assert_eq!(bar_fields.len(), 2);
		assert!(bar_fields.contains("baz"));
		assert!(bar_fields.contains("bar"));
	}
}
