//! Stores the state of Odoo [models][ModelEntry].

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::Display;
use std::ops::Deref;
use std::sync::Arc;
use std::sync::RwLock;

use dashmap::DashMap;
use dashmap::mapref::one::RefMut;
use derive_more::{Deref, DerefMut};
use qp_trie::Trie;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use smart_default::SmartDefault;
use ts_macros::query;

use crate::prelude::*;

use crate::analyze::FunctionParam;
use crate::{ImStr, errloc, format_loc, test_utils};

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
	pub docstring: Option<ImStr>,
	pub deleted: bool,
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
	pub location: TrackedMinLoc,
	pub help: Option<ImStr>,
}

#[derive(Clone, Debug)]
pub struct Method {
	pub return_type: MethodReturnType,
	pub locations: Vec<TrackedMinLoc>,
	pub docstring: Option<ImStr>,
	pub arguments: Option<Box<[FunctionParam]>>,
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct TrackedMinLoc {
	#[deref]
	#[deref_mut]
	inner: MinLoc,
	pub active: bool,
}

impl From<MinLoc> for TrackedMinLoc {
	#[inline]
	fn from(inner: MinLoc) -> Self {
		Self { inner, active: true }
	}
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
		debug!("TODO Field inheritance location {location:?}");
		match &mut self_.kind {
			FieldKind::Value | FieldKind::Related(_) => self_.kind = kind.clone(),
			FieldKind::Relational(_) => {}
		}
		self_.type_ = *type_;
		self_.location.active = true;
		if let Some(help) = help {
			self_.help = Some(help.clone());
		}
		self_
	}
}

impl Method {
	pub fn add_override(self: &mut Arc<Self>, location: MinLoc, top_level_scope: Option<Range>, base: bool) {
		let self_ = Arc::make_mut(self);
		let Some((idx, _)) = self_
			.locations
			.iter()
			.enumerate()
			.rfind(|(_, loc)| loc.path == location.path)
		else {
			if base {
				self_.locations.insert(0, location.into());
			} else {
				self_.locations.push(location.into());
			}
			return;
		};

		let Some(top_level_scope) = top_level_scope else {
			self_.locations.insert(idx + 1, location.into());
			return;
		};

		// find an exact match first
		if let Some(loc) = self_.locations.iter_mut().take(idx + 1).find(|loc| {
			loc.path == location.path
				&& (loc.range.start >= top_level_scope.start && loc.range.end <= top_level_scope.end)
		}) {
			loc.range = location.range;
			loc.active = true;
			return;
		}

		self_.locations.insert(idx + 1, location.into());
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
				(self_.locations).extend(ranges.into_iter().map(|range| MinLoc { path, range }.into()));
				continue;
			};
			(self_.locations).splice(
				first..=last,
				ranges.into_iter().map(|range| MinLoc { path, range }.into()),
			);
		}
	}
	/// Dedents the string according to python's textwrap logic, and parses the method parameters.
	pub fn postprocess_docstring(raw: &str) -> String {
		use std::fmt::Write;

		// Dedent lines
		let lines: Vec<&str> = raw.lines().collect();
		let nonempty: Vec<&str> = lines.iter().filter(|l| !l.trim().is_empty()).copied().collect();
		let min_indent = nonempty
			.into_iter()
			.skip(1)
			.filter_map(|l| {
				let trimmed = l.trim_start();
				if trimmed.is_empty() {
					None
				} else {
					Some(l.len() - trimmed.len())
				}
			})
			.min()
			.unwrap_or(0);
		let dedented: Vec<String> = lines
			.into_iter()
			.enumerate()
			.map(|(i, l)| {
				if i == 0 {
					l.trim().to_string()
				} else if l.len() >= min_indent {
					l[min_indent..].to_string()
				} else {
					l.trim_start().to_string()
				}
			})
			.collect();
		let dedented_str = dedented.join("\n");

		// Manual parser for :param <name>: <desc>
		let mut params = Vec::new();
		let mut body = Vec::new();
		for line in dedented_str.lines() {
			let trimmed = line.trim_start();
			if let Some(rest) = trimmed.strip_prefix(":param")
				&& let rest = rest.trim_start()
				&& let Some(colon_idx) = rest.find(':')
			{
				let (name, desc) = rest.split_at(colon_idx);
				let name = name.trim();
				let desc = desc[1..].trim(); // skip the colon
				if !name.is_empty() {
					params.push((name.to_string(), desc.to_string()));
					continue;
				}
			}
			body.push(line);
		}
		let mut result = body.join("\n").trim().to_string();
		if !params.is_empty() {
			result.push_str("\n\n");
			for (name, desc) in params {
				_ = writeln!(&mut result, "- **{name}**: {desc}");
			}
		}
		result
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

#[derive(Debug)]
pub enum ResolveMappedError {
	NonRelational,
}

impl Display for ResolveMappedError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::NonRelational => f.write_str("Tried to access a field on a non-relational field"),
		}
	}
}

impl core::error::Error for ResolveMappedError {}

impl ModelIndex {
	pub fn append(&self, path: PathSymbol, replace: bool, items: &[Model]) {
		let mut by_prefix = self
			.by_prefix
			.write()
			.expect(format_loc!("unable to acquire write lock now"));
		for item in items {
			match &item.type_ {
				ModelType::Base { name: base, ancestors } => {
					let name = _I(base).into();
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
							.extend(ancestors.iter().map(|sym| ModelName::from(_I(sym))));
					} else {
						warn!(
							"Conflicting bases for {}:\nfirst={}\n  new={}",
							_R(name),
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
							let Some(inherit) = _G(inherit) else { continue };
							if let Some(mut entry) = self.get_mut(&inherit.into()) {
								entry.descendants.retain(|loc| loc.0.path != path)
							}
						}
					}
					if let Some((primary, ancestors)) = inherits.split_first() {
						let inherit = _I(primary).into();
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
							.extend(ancestors.iter().map(|sym| ModelName::from(_I(sym))));
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
		let model_name = _R(model);
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
				let contents = test_utils::fs::read_to_string(&fpath)
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

				let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
				while let Some(match_) = matches.next() {
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
									"One2many" | "Many2one" | "Many2many"
								);
							}
							Some(ModelProperties::Relation) => {
								if is_relational {
									relation = Some(capture.node.byte_range().shrink(1));
								}
							}
							Some(ModelProperties::Arg) => match &contents[capture.node.byte_range()] {
								"comodel_name" if is_relational => kwarg = Some(Kwargs::ComodelName),
								"help" => kwarg = Some(Kwargs::Help),
								"related" => kwarg = Some(Kwargs::Related),
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
					if let Some(field) = field
						&& let Some(type_) = type_
					{
						let range = span_conv(field.range());
						let field_str = &contents[field.byte_range()];
						let field = _I(field_str);
						let type_ = &contents[type_];
						let location = MinLoc {
							path: location.path,
							range,
						}
						.into();
						let help = help.as_deref().map(ImStr::from);
						let kind = if let Some(relation) = relation {
							let relation = &contents[relation];
							let relation = _I(relation);
							FieldKind::Relational(relation)
						} else if let Some(related) = related {
							FieldKind::Related(contents[related].into())
						} else {
							if is_relational {
								debug!("is_relational but no relation found: field={field_str} type={type_}");
							}
							FieldKind::Value
						};
						let type_ = _I(type_);
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
					if let Some(method) = method_name
						&& let Some(body) = method_body
					{
						let method_str = &contents[method.byte_range()];
						let calls_super = contents[body.byte_range()].contains("super(");
						let method = _I(method_str);
						let range = span_conv(body.range());
						let top_level_scope = ast
							.root_node()
							.child_with_descendant(body)
							.map(|scope| span_conv(scope.range()));
						methods.push((
							method,
							top_level_scope,
							calls_super,
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

		if !locations_filter.is_empty() {
			// fields and methods might have been deleted
			let locations_filter = locations_filter
				.iter()
				.map(|filter| filter.to_path())
				.collect::<Vec<_>>();

			for (_, field) in out_fields.iter_mut() {
				if locations_filter
					.iter()
					.any(|filter| field.location.path.to_path().starts_with(filter))
				{
					Arc::make_mut(field).location.active = false;
				}
			}

			for (_, method) in out_methods.iter_mut() {
				let method = Arc::make_mut(method);
				for loc in method.locations.iter_mut() {
					if locations_filter
						.iter()
						.any(|filter| loc.path.to_path().starts_with(filter))
					{
						loc.active = false;
						method.arguments = None;
						method.return_type = MethodReturnType::Unprocessed;
					}
				}
			}
		}

		// drop to prevent deadlock
		drop(entry);

		// recursively get or populate ancestors' properties
		for ancestor in ancestors {
			if let Some(entry) = self.populate_properties(ancestor, locations_filter) {
				if let Some(fields) = entry.fields.as_ref() {
					for (name, field) in fields {
						properties_set.insert(_R(*name).as_bytes(), PropertyKind::Field);
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
						properties_set.insert(_R(*name).as_bytes(), PropertyKind::Method);
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
			properties_set.insert(_R(key).as_bytes(), PropertyKind::Field);
		}

		for (key, top_level_scope, calls_super, method_location) in methods.into_iter().flatten() {
			match out_methods.entry(key.into()) {
				Entry::Occupied(mut old_method) => {
					old_method
						.get_mut()
						.add_override(method_location, top_level_scope, !calls_super);
				}
				Entry::Vacant(empty) => {
					empty.insert(
						Method {
							return_type: Default::default(),
							locations: vec![method_location.into()],
							docstring: None,
							arguments: None,
						}
						.into(),
					);
				}
			}
			properties_set.insert(_R(key).as_bytes(), PropertyKind::Method);
		}

		if !locations_filter.is_empty() {
			// updating done, let's delete dead locations
			out_fields.retain(|_, field| field.location.active);
			out_methods.retain(|_, method| {
				let method = Arc::make_mut(method);
				method.locations.retain(|loc| loc.active);
				!method.locations.is_empty()
			});
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
			trace!("(resolved_mapped) `{needle}` model=`{}`", _R(*model));
			let mut resolved = _G(lhs).and_then(|key| self.resolve_related_field(key.into(), *model));
			if let Some(normalized) = resolved {
				trace!("(resolved_mapped) prenormalized: {}", _R(normalized));
			}
			// lhs: foo
			// rhs: ba
			if resolved.is_none() {
				let Some(model_entry) = self.populate_properties((*model).into(), &[]) else {
					debug!("tried to resolve before fields are populated for `{}`", _R(*model));
					return Ok(());
				};
				let field = _G(lhs);
				let field = field.and_then(|field| model_entry.fields.as_ref()?.get(&field.into()));
				match field.as_ref().map(|f| &f.kind) {
					Some(FieldKind::Relational(rel)) => resolved = Some(*rel),
					None | Some(FieldKind::Value) => return Err(ResolveMappedError::NonRelational),
					Some(FieldKind::Related(..)) => {
						drop(model_entry);
						resolved = self.resolve_related_field(_G(lhs).unwrap().into(), *model);
					}
				}
			}
			let Some(rel) = resolved else {
				warn!("unresolved field `{}`.`{lhs}`", _R(*model));
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
				_R(field),
				_R(model)
			);
			let related = related.clone();
			let mut related = related.as_str();
			drop(entry);
			if self.resolve_mapped(&mut field_model, &mut related, None).is_ok() {
				// resolved_mapped took us to the final field, now we need to resolve it to a model
				let related_key = _G(related)?;
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
	pub fn resolve_details(&mut self) -> anyhow::Result<()> {
		let Some(ModelLocation(loc, byte_range)) = &self.base else {
			return Ok(());
		};
		if self.docstring.is_none() {
			let contents = std::fs::read(loc.path.to_path())?;
			let mut parser = Parser::new();
			parser.set_language(&tree_sitter_python::LANGUAGE.into())?;
			let ast = parser.parse(&contents, None).ok_or_else(|| errloc!("AST not parsed"))?;
			let query = ModelHelp::query();
			let mut cursor = QueryCursor::new();
			cursor.set_byte_range(byte_range.erase());
			let mut matches = cursor.matches(query, ast.root_node(), &contents[..]);
			while let Some(match_) = matches.next() {
				if let Some(docstring) = match_.nodes_for_capture_index(0).next() {
					let contents = String::from_utf8_lossy(&contents[docstring.byte_range()]);
					self.docstring = Some(ImStr::from(contents.trim()));
					return Ok(());
				}
			}
			self.docstring = Some(ImStr::from_static(""));
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
fn parse_help<'text>(node: &Node, contents: &'text str) -> Cow<'text, str> {
	let mut cursor = node.walk();
	match node.kind() {
		"string" => {
			let content = node
				.children(&mut cursor)
				.find_map(|child| (child.kind() == "string_content").then(|| &contents[child.byte_range()]));
			content.unwrap_or("").into()
		}
		"concatenated_string" => {
			let mut content = vec![];
			for string in node.children(&mut cursor) {
				if string.kind() == "string" {
					let mut cursor = string.walk();
					let children = string.children(&mut cursor).find_map(|child| {
						(child.kind() == "string_content").then(|| {
							contents[child.byte_range()]
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
	use tree_sitter::{Parser, QueryCursor, StreamingIterator, StreamingIteratorMut};

	use crate::{
		index::{_I, _R, ModelQuery},
		test_utils::cases::foo::{FOO_PY, prepare_foo_index},
		utils::acc_vec,
	};

	fn clamp_str(str: &str) -> &str {
		if str.len() > 10 { &str[..10] } else { str }
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
			.fold_mut(vec![], acc_vec);
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

		let foo = index.populate_properties(_I("foo").into(), &[]).unwrap();

		assert_eq!(
			foo.fields.as_ref().unwrap().keys().next().map(|sym| _R(*sym)),
			Some("bar")
		);
		drop(foo);

		let bar = index.populate_properties(_I("bar").into(), &[]).unwrap();

		let bar_fields = bar
			.fields
			.as_ref()
			.unwrap()
			.keys()
			.map(|sym| _R(*sym))
			.collect::<HashSet<_>>();
		assert_eq!(bar_fields.len(), 2);
		assert!(bar_fields.contains("baz"));
		assert!(bar_fields.contains("bar"));
	}
}
