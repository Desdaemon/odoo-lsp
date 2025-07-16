use std::borrow::Cow;
use std::path::Path;

use lasso::Spur;
use ropey::Rope;
use tower_lsp_server::{UriExt, lsp_types::*};
use tracing::{debug, instrument, trace, warn};
use tree_sitter::{Node, Parser, QueryMatch};
use ts_macros::query;

use crate::prelude::*;

use crate::analyze::Type;
use crate::index::{_G, _I, _R, PathSymbol, index_models};
use crate::model::{ModelName, ModelType};
use crate::{backend::Backend, backend::Text};

use std::collections::HashMap;

mod completions;
mod diagnostics;

#[cfg(test)]
mod tests;

#[rustfmt::skip]
query! {
	PyCompletions(Request, XmlId, Mapped, MappedTarget, Depends, ReadFn, Model, Prop, ForXmlId, Scope, FieldDescriptor, FieldType);

(call [
  (attribute [
    (identifier) @_env
    (attribute (_) (identifier) @_env)] (identifier) @_ref)
  (attribute
    (identifier) @REQUEST (identifier) @_render)
  (attribute
    (_) (identifier) @FOR_XML_ID) ]
  (argument_list . (string) @XML_ID)
  (#eq? @_env "env")
  (#eq? @_ref "ref")
  (#eq? @REQUEST "request")
  (#eq? @_render "render")
  (#eq? @FOR_XML_ID "_for_xml_id"))

(subscript [
  (identifier) @_env
  (attribute (_) (identifier) @_env)]
  (string) @MODEL
  (#eq? @_env "env"))

((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @PROP [
        (string) @MODEL
        (list ((string) @MODEL ","?)*)
        (call
          (attribute
            (identifier) @_fields (identifier) @FIELD_TYPE (#eq? @_fields "fields"))
          (argument_list
            . [
              ((comment)+ (string) @MODEL)
              (string) @MODEL ]?
            // handles `related` `compute` `search` and `inverse`
            ((keyword_argument (identifier) @FIELD_DESCRIPTOR (_)) ","?)*)) ])))))

(call [
  (attribute
    (_) @MAPPED_TARGET (identifier) @_mapper)
  (attribute
    (identifier) @_api (identifier) @DEPENDS)]
  (argument_list (string) @MAPPED)
  (#match? @_mapper "^(mapp|filter|sort|group)ed$")
  (#eq? @_api "api")
  (#match? @DEPENDS "^(depends|constrains|onchange)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @_search)
  (argument_list [
    (list [
      (tuple . (string) @MAPPED)
      (parenthesized_expression (string) @MAPPED)])
    (keyword_argument
      (identifier) @_domain
      (list [
        (tuple . (string) @MAPPED)
        (parenthesized_expression (string) @MAPPED)]))]))
  (#eq? @_domain "domain")
  (#match? @_search "^(search(_(read|count))?|_?read_group|filtered_domain|_where_calc)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @READ_FN)
  (argument_list [
    (list (string) @MAPPED)
    (keyword_argument
      (identifier) @_domain
      (list (string) @MAPPED)) ]))
  (#match? @_domain "^(groupby|aggregates)$")
  (#match? @READ_FN "^(_?read(_group)?|flush_model)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @DEPENDS)
  (argument_list . [
    (set (string) @MAPPED)
     (dictionary [
       (pair key: (string) @MAPPED)
       (ERROR (string) @MAPPED)
       (ERROR) @MAPPED ])    (_ [
      (set (string) @MAPPED)
      (dictionary [
        (pair key: (string) @MAPPED)
        (ERROR (string) @MAPPED) ]) ]) ]))
  (#match? @DEPENDS "^(create|write|copy)$"))

((class_definition
  (block [
    (function_definition) @SCOPE
    (decorated_definition
      (decorator
        (call
          (attribute (identifier) @_api (identifier) @_depends)
          (argument_list ((string) @MAPPED ","?)*)))
      (function_definition) @SCOPE) ]))
  (#eq? @_api "api")
  (#eq? @_depends "depends"))

(class_definition
  (block
    (decorated_definition
      (decorator (_) @_)
      (function_definition) @SCOPE)*)
  (#not-match? @_ "^api.depends"))
}

#[rustfmt::skip]
query! {
	PyImports(ImportModule, ImportName, ImportAlias);

(import_from_statement
  module_name: (dotted_name) @IMPORT_MODULE
  name: (dotted_name) @IMPORT_NAME)

(import_from_statement
  module_name: (dotted_name) @IMPORT_MODULE
  name: (aliased_import
    name: (dotted_name) @IMPORT_NAME
    alias: (identifier) @IMPORT_ALIAS))

(import_statement
  name: (dotted_name) @IMPORT_NAME)

(import_statement
  name: (aliased_import
    name: (dotted_name) @IMPORT_NAME
    alias: (identifier) @IMPORT_ALIAS))
}

/// (module (_)*)
fn top_level_stmt(module: Node, offset: usize) -> Option<Node> {
	module
		.named_children(&mut module.walk())
		.find(|child| child.byte_range().contains_end(offset))
}

/// Recursively searches for a class definition with the given name in the AST.
fn find_class_definition<'a>(
	node: tree_sitter::Node<'a>,
	contents: &[u8],
	class_name: &str,
) -> Option<tree_sitter::Node<'a>> {
	use crate::utils::PreTravel;

	PreTravel::new(node)
		.find(|node| {
			node.kind() == "class_definition"
				&& node
					.child_by_field_name("name")
					.map(|name_node| {
						let name = String::from_utf8_lossy(&contents[name_node.byte_range()]);
						name == class_name
					})
					.unwrap_or(false)
		})
		.and_then(|node| node.child_by_field_name("name"))
}

#[derive(Debug)]
struct Mapped<'text> {
	needle: Cow<'text, str>,
	model: String,
	single_field: bool,
	range: ByteRange,
}

#[derive(Debug, Clone)]
struct ImportInfo {
	module_path: String,
	imported_name: String,
	alias: Option<String>,
}

type ImportMap = HashMap<String, ImportInfo>;

impl Backend {
	/// Helper function to resolve import-based jump-to-definition requests.
	/// Returns the location if successful, None if not found, or an error if resolution fails.
	fn resolve_import_location(
		&self,
		imports: &ImportMap,
		identifier: &str,
		_contents: &[u8],
	) -> anyhow::Result<Option<Location>> {
		let Some(import_info) = imports.get(identifier) else {
			return Ok(None);
		};

		// Enhanced debugging with alias information
		if let Some(alias) = &import_info.alias {
			debug!(
				"Found aliased import '{}' -> '{}' from module '{}'",
				alias, import_info.imported_name, import_info.module_path
			);
		} else {
			debug!(
				"Found direct import '{}' from module '{}'",
				import_info.imported_name, import_info.module_path
			);
		}

		let Some(file_path) = self.index.resolve_py_module(&import_info.module_path) else {
			debug!("Failed to resolve module path: {}", import_info.module_path);
			return Ok(None);
		};

		debug!("Resolved file path: {}", file_path.display());

		let target_contents = std::fs::read(&file_path)
			.map_err(|e| anyhow::anyhow!("Failed to read target file {}: {}", file_path.display(), e))?;

		let class_name = &import_info.imported_name;
		if let Some(alias) = &import_info.alias {
			debug!(
				"Looking for original class '{}' (aliased as '{}') in target file",
				class_name, alias
			);
		} else {
			debug!("Looking for class '{}' in target file", class_name);
		}

		let mut target_parser = Parser::new();
		target_parser
			.set_language(&tree_sitter_python::LANGUAGE.into())
			.map_err(|e| anyhow::anyhow!("Failed to set parser language: {}", e))?;

		let Some(target_ast) = target_parser.parse(&target_contents, None) else {
			debug!("Failed to parse target file with tree-sitter");
			return Ok(Some(Location {
				uri: Uri::from_file_path(file_path).unwrap(),
				range: Range::new(Position::new(0, 0), Position::new(0, 0)),
			}));
		};

		if let Some(class_node) = find_class_definition(target_ast.root_node(), &target_contents, class_name) {
			let range = class_node.range();
			if let Some(alias) = &import_info.alias {
				debug!(
					"Found class '{}' (aliased as '{}') at line {}, col {}",
					class_name, alias, range.start_point.row, range.start_point.column
				);
			} else {
				debug!(
					"Found class '{}' at line {}, col {}",
					class_name, range.start_point.row, range.start_point.column
				);
			}
			return Ok(Some(Location {
				uri: Uri::from_file_path(file_path).unwrap(),
				range: span_conv(range),
			}));
		}

		if let Some(alias) = &import_info.alias {
			debug!(
				"Class '{}' (aliased as '{}') not found in target file using tree-sitter",
				class_name, alias
			);
		} else {
			debug!("Class '{}' not found in target file using tree-sitter", class_name);
		}
		Ok(Some(Location {
			uri: Uri::from_file_path(file_path).unwrap(),
			range: Range::new(Position::new(0, 0), Position::new(0, 0)),
		}))
	}

	#[tracing::instrument(skip_all, fields(uri))]
	pub fn on_change_python(
		&self,
		text: &Text,
		uri: &Uri,
		rope: RopeSlice<'_>,
		old_rope: Option<Rope>,
	) -> anyhow::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(&tree_sitter_python::LANGUAGE.into())
			.expect("bug: failed to init python parser");
		self.update_ast(text, uri, rope, old_rope, parser)
	}

	/// Parse import statements from Python content and return a map of imported names to their module paths
	fn parse_imports(&self, contents: &[u8]) -> anyhow::Result<ImportMap> {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into())?;

		let ast = parser
			.parse(contents, None)
			.ok_or_else(|| errloc!("Failed to parse Python AST"))?;
		let query = PyImports::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut imports = ImportMap::new();

		debug!("Parsing imports from {} bytes", contents.len());

		for match_ in cursor.matches(query, ast.root_node(), contents) {
			let mut module_path = None;
			let mut import_name = None;
			let mut alias = None;

			debug!("Found import match with {} captures", match_.captures.len());

			for capture in match_.captures {
				let capture_text = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
				debug!("Capture {}: = '{}'", capture.index, capture_text);

				match PyImports::from(capture.index) {
					Some(PyImports::ImportModule) => {
						module_path = Some(capture_text.to_string());
					}
					Some(PyImports::ImportName) => {
						import_name = Some(capture_text.to_string());
					}
					Some(PyImports::ImportAlias) => {
						alias = Some(capture_text.to_string());
					}
					_ => {}
				}
			}

			if let Some(name) = import_name {
				let full_module_path = if let Some(module) = module_path {
					module // For "from module import name", the module path is just the module
				} else {
					name.clone() // For "import name", the module path is the name itself
				};

				let key = alias.as_ref().unwrap_or(&name).clone();
				debug!("Adding import: {} -> {} (from module {})", key, name, full_module_path);
				imports.insert(
					key,
					ImportInfo {
						module_path: full_module_path,
						imported_name: name,
						alias,
					},
				);
			}
		}

		debug!("Final imports map: {:?}", imports);
		Ok(imports)
	}
	pub fn update_models(&self, text: Text, path: &Path, root: Spur, rope: Rope) -> anyhow::Result<()> {
		let text = match text {
			Text::Full(text) => Cow::from(text),
			// TODO: Limit range of possible updates based on delta
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let models = index_models(text.as_bytes())?;
		let path = PathSymbol::strip_root(root, path);
		self.index.models.append(path, true, &models);
		for model in models {
			match model.type_ {
				ModelType::Base { name, ancestors } => {
					let model_key = _G(&name).unwrap();
					let mut entry = self
						.index
						.models
						.try_get_mut(&model_key.into())
						.expect(format_loc!("deadlock"))
						.unwrap();
					entry
						.ancestors
						.extend(ancestors.into_iter().map(|sym| ModelName::from(_I(&sym))));
					drop(entry);
					self.index.models.populate_properties(model_key.into(), &[path]);
				}
				ModelType::Inherit(inherits) => {
					let Some(model) = inherits.first() else { continue };
					let model_key = _G(model).unwrap();
					self.index.models.populate_properties(model_key.into(), &[path]);
				}
			}
		}
		Ok(())
	}
	pub async fn did_save_python(&self, uri: Uri, root: Spur) -> anyhow::Result<()> {
		let path = uri.to_file_path().unwrap();
		let zone;
		_ = {
			let mut document = self
				.document_map
				.get_mut(uri.path().as_str())
				.ok_or_else(|| errloc!("(did_save) did not build document"))?;
			zone = document.damage_zone.take();
			let rope = document.rope.clone();
			let text = Cow::from(&document.rope).into_owned();
			self.update_models(Text::Full(text), &path, root, rope)
		}
		.inspect_err(|err| warn!("{err:?}"));
		if zone.is_some() {
			debug!("diagnostics");
			{
				let mut document = self.document_map.get_mut(uri.path().as_str()).unwrap();
				let rope = document.rope.clone();
				self.diagnose_python(
					uri.path().as_str(),
					rope.slice(..),
					zone,
					&mut document.diagnostics_cache,
				);
				let diags = document.diagnostics_cache.clone();
				self.client.publish_diagnostics(uri, diags, None)
			}
			.await;
		}

		Ok(())
	}
	/// Gathers common information regarding a mapped access aka dot access.
	/// Only makes sense in [`PyCompletions`] queries.
	///
	/// `single_field_override` should be set in special cases where this function may not have the necessary context to
	/// determine whether the needle should be processed in mapped mode or single field mode.
	///
	/// Replacing:
	///
	/// ```text
	///     "foo.bar.baz"
	///           ^cursor
	///      -----------range
	///      ------needle
	/// ```
	///
	/// Not replacing:
	///
	/// ```text
	///     "foo.bar.baz"
	///           ^cursor
	///      -------range
	///      -------needle
	/// ```
	#[instrument(skip_all, ret, fields(range_content = String::from_utf8_lossy(&contents[range.clone()]).as_ref()))]
	fn gather_mapped<'text>(
		&self,
		root: Node,
		match_: &tree_sitter::QueryMatch,
		offset: Option<usize>,
		mut range: core::ops::Range<usize>,
		this_model: Option<&[u8]>,
		contents: &'text [u8],
		for_replacing: bool,
		single_field_override: Option<bool>,
	) -> Option<Mapped<'text>> {
		let mut needle = if for_replacing {
			range = range.shrink(1);
			let offset = offset.unwrap_or(range.end);
			String::from_utf8_lossy(&contents[range.start..offset])
		} else {
			let slice = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
			let relative_start = range.start + 1;
			let offset = offset
				.unwrap_or((range.end - 1).max(relative_start + 1))
				.max(relative_start)
				.min(relative_start + slice.len());
			// assert!(
			// 	offset >= relative_start,
			// 	"offset={} cannot be less than relative_start={}",
			// 	offset,
			// 	relative_start
			// );
			let start = offset - relative_start;
			let slice_till_end = slice.get(start..).unwrap_or("");
			// How many characters until the next period or end-of-string?
			let limit = slice_till_end.find('.').unwrap_or(slice_till_end.len());
			range = relative_start..offset + limit;
			// Cow::from(rope.get_byte_slice(range.clone())?)
			String::from_utf8_lossy(&contents[range.clone()])
		};
		if needle == "|" || needle == "&" {
			return None;
		}

		tracing::trace!(
			"(gather_mapped) {} matches={match_:?}",
			String::from_utf8_lossy(&contents[range.clone()])
		);

		let model;
		if let Some(local_model) = match_.nodes_for_capture_index(PyCompletions::MappedTarget as _).next() {
			let model_ = (self.index).model_of_range(root, local_model.byte_range().map_unit(ByteOffset), contents)?;
			model = _R(model_).to_string();
		} else if let Some(this_model) = &this_model {
			model = String::from_utf8_lossy(this_model).to_string();
		} else {
			return None;
		}

		let mut single_field = false;
		if let Some(depends) = match_.nodes_for_capture_index(PyCompletions::Depends as _).next() {
			single_field = matches!(
				&contents[depends.byte_range()],
				b"write" | b"create" | b"constrains" | b"onchange"
			);
		} else if let Some(read_fn) = match_.nodes_for_capture_index(PyCompletions::ReadFn as _).next() {
			// read or read_group, fields only
			single_field = true;
			if contents[read_fn.byte_range()].ends_with(b"read_group") {
				// split off aggregate functions
				needle = match cow_split_once(needle, ":") {
					Err(unchanged) => unchanged,
					Ok((field, _)) => {
						range = range.start..range.start + field.len();
						field
					}
				}
			}
		} else if let Some(override_) = single_field_override {
			single_field = override_;
		}

		Some(Mapped {
			needle,
			model,
			single_field,
			range: range.map_unit(ByteOffset),
		})
	}
	pub fn python_jump_def(
		&self,
		params: GotoDefinitionParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let Ok(ByteOffset(offset)) = rope_conv(params.text_document_position_params.position, rope) else {
			return Err(errloc!("could not find offset for {}", uri.path().as_str()));
		};
		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let root = some!(top_level_stmt(ast.root_node(), offset));

		// Parse imports from the current file
		let imports = self.parse_imports(contents).unwrap_or_default();
		debug!("Parsed imports: {:?}", imports);

		// Check if cursor is on an imported identifier
		if let Some(cursor_node) = ast.root_node().descendant_for_byte_range(offset, offset)
			&& cursor_node.kind() == "identifier"
		{
			let identifier_bytes = &contents[cursor_node.byte_range()];
			let identifier = std::str::from_utf8(identifier_bytes).unwrap_or("");
			debug!("Checking identifier '{}' at offset {}", identifier, offset);

			// Try to resolve import location
			if let Some(location) = self.resolve_import_location(&imports, identifier, contents)? {
				return Ok(Some(location));
			}
		}

		let query = PyCompletions::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut this_model = ThisModel::default();

		for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let slice = some!(rope.get_byte_slice(range.clone()));
						let slice = Cow::from(slice);
						return self
							.index
							.jump_def_xml_id(&slice, &params.text_document_position_params.text_document.uri);
					}
					Some(PyCompletions::Model) => {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::Prop as _)
							.next()
							.map(|prop| matches!(&contents[prop.byte_range()], b"_name" | b"_inherit"))
							.unwrap_or(true);
						if range.contains(&offset) {
							let range = range.shrink(1);
							let slice = some!(rope.get_byte_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.jump_def_model(&slice);
						} else if range.end < offset && is_meta
						// match_
						// 	.nodes_for_capture_index(PyCompletions::FieldType as _)
						// 	.next()
						// 	.is_none()
						{
							this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
						}
					}
					Some(PyCompletions::Mapped) => {
						if range.contains_end(offset)
							&& let Some(mapped) = self.gather_mapped(
								root,
								&match_,
								Some(offset),
								range.clone(),
								this_model.inner,
								contents,
								false,
								None,
							) {
							let mut needle = mapped.needle.as_ref();
							let mut model = _I(mapped.model);
							if !mapped.single_field {
								some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
							}
							let model = _R(model);
							return self.index.jump_def_property_name(needle, model);
						} else if let Some(cmdlist) = capture.node.next_named_sibling()
							&& Backend::is_commandlist(cmdlist, offset)
						{
							let (needle, _, model) = some!(self.gather_commandlist(
								cmdlist,
								root,
								&match_,
								offset,
								range,
								this_model.inner,
								contents,
								false,
							));
							return self.index.jump_def_property_name(&needle, _R(model));
						}
					}
					Some(PyCompletions::FieldDescriptor) => {
						let Some(desc_value) = capture.node.next_named_sibling() else {
							continue;
						};

						let descriptor = &contents[capture.node.byte_range()];
						if !desc_value.byte_range().contains_end(offset) {
							continue;
						}
						if matches!(descriptor, b"comodel_name") {
							let range = desc_value.byte_range().shrink(1);
							let slice = some!(rope.get_byte_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.jump_def_model(&slice);
						} else if matches!(descriptor, b"compute" | b"search" | b"inverse" | b"related") {
							let single_field = descriptor != b"related";
							// same as PyCompletions::Mapped
							let Some(mapped) = self.gather_mapped(
								root,
								&match_,
								Some(offset),
								desc_value.byte_range(),
								this_model.inner,
								contents,
								false,
								Some(single_field),
							) else {
								break;
							};
							let mut needle = mapped.needle.as_ref();
							let mut model = _I(mapped.model);
							if !mapped.single_field {
								some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
							}
							let model = _R(model);
							return self.index.jump_def_property_name(needle, model);
						}

						return Ok(None);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}
		}

		let (model, prop, _) = some!(self.attribute_at_offset(offset, root, contents));
		self.index.jump_def_property_name(&prop, model)
	}
	/// Resolves the attribute and the object's model at the cursor offset
	/// using [`model_of_range`][Index::model_of_range].
	///
	/// Returns `(model, property, range)`.
	fn attribute_at_offset<'out>(
		&'out self,
		offset: usize,
		root: Node<'out>,
		contents: &'out [u8],
	) -> Option<(&'out str, Cow<'out, str>, core::ops::Range<usize>)> {
		let (lhs, field, range) = Self::attribute_node_at_offset(offset, root, contents)?;
		let model = (self.index).model_of_range(root, lhs.byte_range().map_unit(ByteOffset), contents)?;
		Some((_R(model), field, range))
	}
	/// Resolves the attribute at the cursor offset.
	/// Returns `(object, field, range)`
	#[instrument(level = "trace", skip_all, ret)]
	pub fn attribute_node_at_offset<'out>(
		mut offset: usize,
		root: Node<'out>,
		contents: &'out [u8],
	) -> Option<(Node<'out>, Cow<'out, str>, core::ops::Range<usize>)> {
		if contents.is_empty() {
			return None;
		}
		offset = offset.clamp(0, contents.len() - 1);
		let mut cursor_node = root.descendant_for_byte_range(offset, offset)?;
		let mut real_offset = None;
		if cursor_node.is_named() && !matches!(cursor_node.kind(), "attribute" | "identifier") {
			// We got our cursor left in the middle of nowhere.
			real_offset = Some(offset);
			offset = offset.saturating_sub(1);
			cursor_node = root.descendant_for_byte_range(offset, offset)?;
		}
		trace!(
			"(attribute_node_to_offset) {} cursor={}\n  sexp={}",
			String::from_utf8_lossy(&contents[cursor_node.byte_range()]),
			contents[offset] as char,
			cursor_node.to_sexp(),
		);
		let lhs;
		let rhs;
		if !cursor_node.is_named() {
			// We landed on one of the punctuations inside the attribute.
			// Need to determine which one it is.
			// We cannot depend on prev_named_sibling because the AST may be all messed up
			let idx = contents[..=offset].iter().rposition(|c| *c == b'.')?;
			let ident = contents[..=idx].iter().rposition(u8::is_ascii_alphanumeric)?;
			lhs = root.descendant_for_byte_range(ident, ident)?;
			rhs = lhs.next_named_sibling().and_then(|attr| match attr.kind() {
				"identifier" => Some(attr),
				"attribute" => attr.child_by_field_name("attribute"),
				_ => None,
			});
		} else if cursor_node.kind() == "attribute" {
			lhs = cursor_node.child_by_field_name("object")?;
			rhs = cursor_node.child_by_field_name("attribute");
		} else {
			match cursor_node.parent() {
				Some(parent) if parent.kind() == "attribute" => {
					lhs = parent.child_by_field_name("object")?;
					rhs = Some(cursor_node);
				}
				Some(parent) if parent.kind() == "ERROR" => {
					// (ERROR (_) @cursor_node)
					lhs = cursor_node;
					rhs = None;
				}
				_ => return None,
			}
		}
		trace!(
			"(attribute_node_to_offset) lhs={} rhs={:?}",
			String::from_utf8_lossy(&contents[lhs.byte_range()]),
			rhs.as_ref()
				.map(|rhs| String::from_utf8_lossy(&contents[rhs.byte_range()])),
		);
		if lhs == cursor_node {
			// We shouldn't recurse into cursor_node itself.
			return None;
		}
		let Some(rhs) = rhs else {
			// In single-expression mode, rhs could be empty in which case
			// we return an empty needle/range.
			let offset = real_offset.unwrap_or(offset);
			return Some((lhs, Cow::from(""), offset..offset));
		};
		let (field, range) = if rhs.range().start_point.row != lhs.range().end_point.row {
			// tree-sitter has an issue with attributes spanning multiple lines
			// which is NOT valid Python, but allows it anyways because tree-sitter's
			// use cases don't require strict syntax trees.
			let offset = real_offset.unwrap_or(offset);
			(Cow::from(""), offset..offset)
		} else {
			let range = rhs.byte_range();
			(String::from_utf8_lossy(&contents[range.clone()]), range)
		};

		Some((lhs, field, range))
	}
	pub fn python_references(
		&self,
		params: ReferenceParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Vec<Location>>> {
		let ByteOffset(offset) = ok!(rope_conv(params.text_document_position.position, rope));
		let uri = &params.text_document_position.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let mut cursor = tree_sitter::QueryCursor::new();
		let path = some!(params.text_document_position.text_document.uri.to_file_path());
		let current_module = self.index.find_module_of(&path);
		let mut this_model = ThisModel::default();

		for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let slice = some!(rope.get_byte_slice(range.clone()));
						let slice = Cow::from(slice);
						return self.record_references(&path, &slice, current_module);
					}
					Some(PyCompletions::Model) => {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::Prop as _)
							.next()
							.map(|prop| matches!(&contents[prop.byte_range()], b"_name" | b"_inherit"))
							.unwrap_or(true);
						if is_meta && range.contains(&offset) {
							let range = range.shrink(1);
							let slice = some!(rope.get_byte_slice(range.clone()));
							let slice = Cow::from(slice);
							let slice = some!(_G(slice));
							return self.model_references(&path, &slice.into());
						} else if range.end < offset
							&& match_
								.nodes_for_capture_index(PyCompletions::FieldType as _)
								.next()
								.is_none()
						{
							this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
						}
					}
					Some(PyCompletions::FieldDescriptor) => {
						let Some(desc_value) = capture.node.next_named_sibling() else {
							continue;
						};
						let descriptor = &contents[range];
						// TODO: related, when field inheritance is implemented
						if !desc_value.byte_range().contains_end(offset) {
							continue;
						};

						if matches!(descriptor, b"comodel_name") {
							let range = desc_value.byte_range().shrink(1);
							let slice = some!(rope.get_byte_slice(range.clone()));
							let slice = Cow::from(slice);
							let slice = some!(_G(slice));
							return self.model_references(&path, &slice.into());
						} else if matches!(descriptor, b"compute" | b"search" | b"inverse") {
							let range = desc_value.byte_range().shrink(1);
							let model = String::from_utf8_lossy(some!(this_model.inner.as_ref()));
							let prop = String::from_utf8_lossy(&contents[range]);
							return self.index.method_references(&prop, &model);
						}

						return Ok(None);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::Mapped)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}
		}

		let (model, prop, _) = some!(self.attribute_at_offset(offset, root, contents));
		self.index.method_references(&prop, model)
	}

	pub fn python_hover(&self, params: HoverParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Hover>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let Ok(ByteOffset(offset)) = rope_conv(params.text_document_position_params.position, rope) else {
			return Err(errloc!("could not find offset for {}", uri.path().as_str()));
		};

		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut this_model = ThisModel::default();
		for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::Model) => {
						if range.contains_end(offset) {
							let range = range.shrink(1);
							let lsp_range = span_conv(capture.node.range());
							let slice = some!(rope.get_byte_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.hover_model(&slice, Some(lsp_range), false, None);
						} else if range.end < offset {
							this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
						}
					}
					Some(PyCompletions::Mapped) => {
						if range.contains(&offset) {
							let mapped = some!(self.gather_mapped(
								root,
								&match_,
								Some(offset),
								range.clone(),
								this_model.inner,
								contents,
								false,
								None,
							));
							let mut needle = mapped.needle.as_ref();
							let mut model = _I(mapped.model);
							let mut range = mapped.range;
							if !mapped.single_field {
								some!(
									self.index
										.models
										.resolve_mapped(&mut model, &mut needle, Some(&mut range))
										.ok()
								);
							}
							let model = _R(model);
							return self
								.index
								.hover_property_name(needle, model, rope_conv(range, rope).ok());
						} else if let Some(cmdlist) = capture.node.next_named_sibling()
							&& Backend::is_commandlist(cmdlist, offset)
						{
							let (needle, range, model) = some!(self.gather_commandlist(
								cmdlist,
								root,
								&match_,
								offset,
								range,
								this_model.inner,
								contents,
								false,
							));
							let range = rope_conv(range, rope).ok();
							return self.index.hover_property_name(&needle, _R(model), range);
						}
					}
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let xml_id = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
						return self
							.index
							.hover_record(&xml_id, rope_conv(range.map_unit(ByteOffset), rope).ok());
					}
					Some(PyCompletions::Prop) if range.contains(&offset) => {
						let model = some!(this_model.inner);
						let model = String::from_utf8_lossy(model);
						let name = String::from_utf8_lossy(&contents[range]);
						let range = span_conv(capture.node.range());
						return self.index.hover_property_name(&name, &model, Some(range));
					}
					Some(PyCompletions::FieldDescriptor) => {
						let Some(desc_value) = capture.node.next_named_sibling() else {
							continue;
						};
						let descriptor = &contents[range];
						if !desc_value.byte_range().contains_end(offset) {
							continue;
						}

						if matches!(descriptor, b"comodel_name") {
							let range = desc_value.byte_range().shrink(1);
							let lsp_range = span_conv(desc_value.range());
							let slice = some!(rope.get_byte_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.hover_model(&slice, Some(lsp_range), false, None);
						} else if matches!(descriptor, b"compute" | b"search" | b"inverse" | b"related") {
							let single_field = descriptor != b"related";
							let mapped = some!(self.gather_mapped(
								root,
								&match_,
								Some(offset),
								desc_value.byte_range(),
								this_model.inner,
								contents,
								false,
								Some(single_field)
							));
							let mut needle = mapped.needle.as_ref();
							let mut model = _I(mapped.model);
							let mut range = mapped.range;
							if !mapped.single_field {
								some!(
									self.index
										.models
										.resolve_mapped(&mut model, &mut needle, Some(&mut range))
										.ok()
								);
							}
							let model = _R(model);
							return self
								.index
								.hover_property_name(needle, model, rope_conv(range, rope).ok());
						}

						return Ok(None);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}
		}
		if let Some((model, prop, range)) = self.attribute_at_offset(offset, root, contents) {
			let lsp_range = rope_conv(range.map_unit(ByteOffset), rope).ok();
			return self.index.hover_property_name(&prop, model, lsp_range);
		}

		// No matches, assume arbitrary expression.
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let lsp_range = span_conv(needle.range());
		let (type_, scope) =
			some!((self.index).type_of_range(root, needle.byte_range().map_unit(ByteOffset), contents));
		if let Some(model) = self.index.try_resolve_model(&type_, &scope) {
			let model = _R(model);
			let identifier =
				(needle.kind() == "identifier").then(|| String::from_utf8_lossy(&contents[needle.byte_range()]));
			return self
				.index
				.hover_model(model, Some(lsp_range), true, identifier.as_deref());
		}

		// not a model! we only have so many things we can hover...
		match type_ {
			Type::Method(model, method) => self.index.hover_property_name(&method, _R(model), Some(lsp_range)),
			_ => Ok(None),
		}
	}

	pub(crate) fn python_signature_help(&self, params: SignatureHelpParams) -> anyhow::Result<Option<SignatureHelp>> {
		use std::fmt::Write;

		let document =
			some!((self.document_map).get(params.text_document_position_params.text_document.uri.path().as_str()));
		let ast = some!((self.ast_map).get(params.text_document_position_params.text_document.uri.path().as_str()));
		let contents = Cow::from(&document.rope);
		let contents = contents.as_bytes();

		let point = tree_sitter::Point::new(
			params.text_document_position_params.position.line as _,
			params.text_document_position_params.position.character as _,
		);
		let node = some!(ast.root_node().descendant_for_point_range(point, point));
		let mut args = node;
		while let Some(parent) = args.parent() {
			if args.kind() == "argument_list" {
				break;
			}
			args = parent;
		}

		if args.kind() != "argument_list" {
			return Ok(None);
		}

		let active_parameter = 'find_param: {
			if let Ok(offset) =
				rope_conv::<_, ByteOffset>(params.text_document_position_params.position, document.rope.slice(..))
				&& let Some(contents) = contents.get(..=offset.0)
				&& let Some(idx) = contents.iter().rposition(|&c| c == b',' || c == b'(')
			{
				if contents[idx] == b'(' {
					break 'find_param Some(0);
				}
				let prev_param = args.descendant_for_byte_range(idx, idx).unwrap().prev_named_sibling();
				for (idx, arg) in args.named_children(&mut args.walk()).enumerate() {
					if Some(arg) == prev_param {
						// the index might be intentionally out of bounds w.r.t the actual number of arguments
						// but this is better than leaving it as None because clients infer it as the first argument
						break 'find_param Some((idx + 1) as u32);
					}
				}
			}

			None
		};

		let callee = some!(args.prev_named_sibling());
		let Some((Type::Method(model_key, method), _)) =
			(self.index).type_of_range(ast.root_node(), callee.byte_range().map_unit(ByteOffset), contents)
		else {
			return Ok(None);
		};
		let method_key = some!(_G(&method));
		let rtype = (self.index).resolve_method_returntype(method_key.into(), model_key.into());
		let model = some!((self.index).models.get(&model_key));
		let method_obj = some!(some!(model.methods.as_ref()).get(&method_key.into()));

		let mut label = format!("{method}(");
		let mut parameters = vec![];

		for (idx, param) in method_obj.arguments.as_deref().unwrap_or(&[]).iter().enumerate() {
			let begin;
			if idx == 0 {
				begin = label.len();
				_ = write!(&mut label, "{param}");
			} else {
				begin = label.len() + 2;
				_ = write!(&mut label, ", {param}");
			}
			let end = label.len();
			parameters.push(ParameterInformation {
				label: ParameterLabel::LabelOffsets([begin as _, end as _]),
				documentation: None,
			});
		}

		match rtype {
			Some(rtype) => drop(write!(&mut label, ") -> Model[\"{}\"]", _R(rtype))),
			None => label.push_str(") -> ..."),
		};

		let sig = SignatureInformation {
			label,
			active_parameter,
			parameters: Some(parameters),
			documentation: method_obj.docstring.as_ref().map(|doc| {
				Documentation::MarkupContent(MarkupContent {
					kind: MarkupKind::Markdown,
					value: doc.to_string(),
				})
			}),
		};

		Ok(Some(SignatureHelp {
			signatures: vec![sig],
			active_signature: Some(0),
			active_parameter: None,
		}))
	}

	#[allow(clippy::unused_async)] // reason: custom method
	pub async fn debug_inspect_type(
		&self,
		params: TextDocumentPositionParams,
	) -> tower_lsp_server::jsonrpc::Result<Option<String>> {
		let uri = &params.text_document.uri;
		let document = some!(self.document_map.get(uri.path().as_str()));
		let ast = some!(self.ast_map.get(uri.path().as_str()));
		let rope = &document.rope;
		let contents = Cow::from(rope);
		let ByteOffset(offset) = some!(rope_conv(params.position, rope.slice(..)).ok());
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let (type_, _) =
			some!((self.index).type_of_range(root, needle.byte_range().map_unit(ByteOffset), contents.as_bytes()));
		Ok(Some(format!("{type_:?}").replacen("Text::", "", 1)))
	}
	fn is_commandlist(cmdlist: Node, offset: usize) -> bool {
		cmdlist.kind() == "list"
			&& cmdlist.byte_range().contains_end(offset)
			&& cmdlist.parent().is_some_and(|parent| parent.kind() == "pair")
	}
	/// `cmdlist` must have been checked by [is_commandlist][Backend::is_commandlist] first
	///
	/// Returns `(needle, range, model)` for a field
	fn gather_commandlist<'text>(
		&self,
		cmdlist: Node,
		root: Node,
		match_: &tree_sitter::QueryMatch,
		offset: usize,
		range: std::ops::Range<usize>,
		this_model: Option<&[u8]>,
		contents: &'text [u8],
		for_replacing: bool,
	) -> Option<(Cow<'text, str>, ByteRange, Spur)> {
		let mut access = vec![];
		access.extend_from_slice(&contents[range.shrink(1)]);
		tracing::debug!(
			"gather_commandlist: cmdlist range: {:?}, offset: {}",
			cmdlist.byte_range(),
			offset
		);
		let mut dest = cmdlist.descendant_for_byte_range(offset, offset);
		tracing::debug!("Initial dest: {:?}", dest.map(|n| (n.kind(), n.byte_range())));

		// If we can't find a node at the exact offset, try to find the last string node
		// This handles the case where cursor is after a string without a colon
		if dest.is_none() && offset > cmdlist.start_byte() {
			tracing::debug!("No node at offset {}, trying offset - 1", offset);
			// Try offset - 1 to see if we're just after a string
			if let Some(node) = cmdlist.descendant_for_byte_range(offset - 1, offset - 1) {
				tracing::debug!(
					"Found node at offset - 1: kind={}, range={:?}",
					node.kind(),
					node.byte_range()
				);
				if node.kind() == "string"
					|| (node.kind() == "string_content" && node.parent().map(|p| p.kind()) == Some("string"))
					|| node.kind() == "string_end"
				{
					// Check if this string is not part of a key-value pair (no colon after it)
					let string_node = if node.kind() == "string" {
						node
					} else if node.kind() == "string_end" && node.parent().map(|p| p.kind()) == Some("string") {
						node.parent()?
					} else {
						node.parent()?
					};
					if let Some(next_sibling) = string_node.next_sibling() {
						tracing::debug!("String has next sibling: {}", next_sibling.kind());
						if next_sibling.kind() != ":" {
							// This is an incomplete field name, provide completions
							dest = Some(string_node);
						}
					} else {
						tracing::debug!("String has no next sibling, treating as incomplete");
						// No next sibling means it's the last element, likely incomplete
						dest = Some(string_node);
					}
				}
			}
		}

		let mut dest = dest?;

		// If we're inside a string_content node, get the parent string node
		if dest.kind() == "string_content" {
			dest = dest.parent()?;
		}

		if dest.kind() != "string" {
			dest = dest.parent()?;
		}
		if dest.kind() != "string" {
			return None;
		}

		// First check if this string is in a broken syntax situation
		// (i.e., it's a key in a dictionary without a following colon)
		let mut is_broken_syntax = false;
		if let Some(parent) = dest.parent() {
			tracing::debug!("String parent kind: {}", parent.kind());
			if parent.kind() == "dictionary" {
				// Check if this string has a colon after it
				if let Some(next_sibling) = dest.next_sibling() {
					tracing::debug!("String next sibling kind: {}", next_sibling.kind());
					if next_sibling.kind() != ":" {
						is_broken_syntax = true;
					}
				} else {
					tracing::debug!("String has no next sibling");
					// No next sibling means it's the last element, likely incomplete
					is_broken_syntax = true;
				}
			} else if parent.kind() == "ERROR" {
				// When there's broken syntax, tree-sitter creates ERROR nodes
				// Check if the parent of the ERROR is a dictionary
				if let Some(grandparent) = parent.parent() {
					tracing::debug!("ERROR parent (grandparent) kind: {}", grandparent.kind());
					if grandparent.kind() == "dictionary" {
						// This is likely a string in a dictionary with broken syntax
						is_broken_syntax = true;
					}
				}
			}
		}

		if is_broken_syntax {
			tracing::debug!("Detected broken syntax: string in dictionary without colon");
			// For broken syntax, we need to continue processing to determine the model
			// but we'll use an empty needle to show all available fields
		}

		let (needle, model_str, range) = if is_broken_syntax {
			// For broken syntax, we don't want to complete the partial field name
			// We want to show all available fields
			// We still need to get the model context, so we'll use the parent model if available
			let range = ByteRange {
				start: ByteOffset(offset),
				end: ByteOffset(offset),
			};
			// Use the this_model if available, otherwise we'll need to determine it from context
			let model_bytes = if let Some(model) = this_model {
				model.to_vec()
			} else {
				// For command lists without explicit model, we need to continue processing
				// to determine the model from the field context
				vec![]
			};
			(
				Cow::Borrowed(""),
				String::from_utf8_lossy(&model_bytes).into_owned(),
				range,
			)
		} else {
			// Normal case - complete the field name
			let Mapped {
				needle, model, range, ..
			} = self.gather_mapped(
				root,
				match_,
				Some(offset),
				dest.byte_range(),
				this_model,
				contents,
				for_replacing,
				None,
			)?;
			(needle, model, range)
		};

		tracing::debug!(
			"needle={}, is_broken_syntax={}, model_str={}",
			needle,
			is_broken_syntax,
			model_str
		);

		// recursive descent to collect the chain of fields
		let mut cursor = cmdlist;
		let mut count = 0;
		while count < 30 {
			count += 1;
			let Some(candidate) = cursor.child_containing_descendant(dest) else {
				tracing::debug!("child_containing_descendant returned None at count={}", count);
				return None;
			};
			let obj;
			tracing::debug!("candidate kind: {}", candidate.kind());
			if candidate.kind() == "tuple" {
				// (0, 0, {})
				obj = candidate.child_containing_descendant(dest)?;
			} else if candidate.kind() == "call" {
				// Command.create({}), but we don't really care if the actual function is called.
				let args = dig!(candidate, argument_list(1))?;
				obj = args.child_containing_descendant(dest)?;
			} else {
				return None;
			}
			tracing::debug!("obj kind: {}", obj.kind());
			if obj.kind() == "dictionary" {
				let pair = obj.child_containing_descendant(dest)?;
				tracing::debug!("pair kind: {}", pair.kind());
				if pair.kind() != "pair" {
					// Check if this is a broken syntax case (string without colon)
					if pair.kind() == "string" && pair.byte_range().contains(&offset) {
						// This is a string in a dictionary without a colon
						// We're completing field names for this dictionary
						tracing::debug!("Breaking due to broken syntax string in dictionary");
						// Break out of the loop to resolve the model
						break;
					} else if pair.kind() == "ERROR" {
						// When there's broken syntax, tree-sitter might create an ERROR node
						// Check if the ERROR contains our string
						if pair.byte_range().contains(&offset) {
							tracing::debug!("Breaking due to ERROR node containing offset");
							break;
						}
					}
					tracing::debug!("Returning None: pair kind {} is not 'pair'", pair.kind());
					return None;
				}

				let key = dig!(pair, string)?;
				if key.byte_range().contains_end(offset) {
					break;
				}

				cursor = pair.child_containing_descendant(dest)?;
				access.push(b'.');
				access.extend_from_slice(&contents[key.byte_range().shrink(1)]);
			} else if obj.kind() == "set" {
				break;
			} else {
				// TODO: (ERROR) case
				return None;
			}
		}

		if count == 30 {
			warn!("recursion limit hit");
		}

		access.push(b'.'); // to force resolution of the last field
		let access = String::from_utf8_lossy(&access).into_owned();
		tracing::debug!("Access path: {}", access);
		tracing::debug!("Initial model before resolve: {}", model_str);
		let access = &mut access.as_str();
		let mut model = _I(&model_str);
		if self.index.models.resolve_mapped(&mut model, access, None).is_err() {
			tracing::debug!("resolve_mapped failed for model={} access={}", _R(model), access);
			return None;
		}
		tracing::debug!("Resolved model: {}", _R(model));

		Some((needle, range, model))
	}
}

#[derive(Default, Clone)]
struct ThisModel<'a> {
	inner: Option<&'a [u8]>,
	source: ThisModelKind,
	top_level_range: core::ops::Range<usize>,
}

#[derive(Default, Clone, Copy)]
enum ThisModelKind {
	Primary,
	#[default]
	Inherited,
}

impl<'this> ThisModel<'this> {
	/// Call this on captures of index [`PyCompletions::Model`].
	fn tag_model(
		&mut self,
		model: Node,
		match_: &QueryMatch,
		top_level_range: core::ops::Range<usize>,
		contents: &'this [u8],
	) {
		if match_
			.nodes_for_capture_index(PyCompletions::FieldType as _)
			.next()
			.is_some()
		{
			// debug_assert!(false, "tag_model called on a class model; handle this manually");
			return;
		}

		debug_assert_eq!(model.kind(), "string");
		let (is_name, mut is_inherit) = match_
			.nodes_for_capture_index(PyCompletions::Prop as _)
			.next()
			.map(|prop| {
				let prop = &contents[prop.byte_range()];
				(prop == b"_name", prop == b"_inherit")
			})
			.unwrap_or((false, false));
		let top_level_changed = top_level_range != self.top_level_range;
		// If still in same class AND _name already declared, skip.
		is_inherit = is_inherit && (top_level_changed || matches!(self.source, ThisModelKind::Inherited));
		if is_inherit {
			let parent = model.parent().expect(format_loc!("(tag_model) parent"));
			// _inherit = '..' OR _inherit = ['..']
			is_inherit = parent.kind() == "assignment" || parent.kind() == "list" && parent.named_child_count() == 1;
		}
		if is_inherit || is_name && top_level_changed {
			self.inner = Some(&contents[model.byte_range().shrink(1)]);
			self.top_level_range = top_level_range;
			if is_name {
				self.source = ThisModelKind::Primary;
			} else if is_inherit {
				self.source = ThisModelKind::Inherited;
			}
		}
	}
}
