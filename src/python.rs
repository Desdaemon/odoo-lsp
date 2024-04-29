use crate::analyze::{determine_scope, Scope, Type};
use crate::{Backend, Text};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::ops::ControlFlow;
use std::path::Path;

use lasso::Spur;
use log::{debug, trace, warn};
use miette::{diagnostic, miette};
use odoo_lsp::index::{index_models, interner, Module, PathSymbol, Symbol};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Parser, QueryCursor, QueryMatch, Tree};

use odoo_lsp::model::{ModelName, ModelType, ResolveMappedError};
use odoo_lsp::utils::*;
use odoo_lsp::{format_loc, some};
use ts_macros::query;

#[rustfmt::skip]
query! {
	PyCompletions(Request, XmlId, Mapped, MappedTarget, Depends, ReadFn, Model, Prop, ForXmlId, Scope);

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
            (identifier) @_fields (identifier))
          (argument_list
            (keyword_argument
              (identifier) @_related (string) @MAPPED)?))]))))
  (#eq? @_fields "fields")
  (#eq? @_related "related"))

(call [
  (attribute
    (_) @MAPPED_TARGET (identifier) @_mapper)
  (attribute
    (identifier) @_api (identifier) @DEPENDS)]
  (argument_list (string) @MAPPED)
  (#match? @_mapper "^(mapp|filter|sort)ed$")
  (#eq? @_api "api")
  (#match? @DEPENDS "^(depends|constrains|onchange)$"))

((call [
  (identifier) @_Field
  (attribute (identifier) @_fields (identifier) @_Field) ]
  (argument_list
    . ((comment)* . (string) @MODEL)?
    (keyword_argument
      (identifier) @_comodel_name (string) @MODEL)?
    (keyword_argument
      (identifier) @_domain
      (list [
        (parenthesized_expression (string) @MAPPED)
        (tuple . (string) @MAPPED) ]))? ))
  (#eq? @_fields "fields")
  (#match? @_Field "^(Many2one|One2many|Many2many)$")
  (#eq? @_comodel_name "comodel_name")
  (#eq? @_domain "domain"))

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
  (#match? @_search "^(search(_(read|count))?|_?read_group|filtered_domain)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @READ_FN)
  (argument_list [
    (list (string) @MAPPED)
    (keyword_argument
      (identifier) @_domain
      (list (string) @MAPPED)) ]))
  (#match? @_domain "^(groupby|aggregates)$")
  (#match? @READ_FN "^_?read(_group)?$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @DEPENDS)
  (argument_list . [
    (set (string) @MAPPED)
    (dictionary [
      (pair key: (string) @MAPPED)
      (ERROR (string) @MAPPED) ]) ]))
  (#eq? @DEPENDS "write"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @DEPENDS)
  (argument_list . [
    (set (string) @MAPPED)
    (dictionary [
      (pair key: (string) @MAPPED)
      (ERROR (string) @MAPPED) ])
    (list [
      (set (string) @MAPPED)
      (dictionary [
        (pair key: (string) @MAPPED)
        (ERROR (string) @MAPPED) ]) ]) ]))
  (#eq? @DEPENDS "create"))

(class_definition
  (block [
    (function_definition) @SCOPE
    (decorated_definition (function_definition) @SCOPE) ]))
}

/// (module (_)*)
fn top_level_stmt(module: Node, offset: usize) -> Option<Node> {
	module
		.named_children(&mut module.walk())
		.find(|child| child.byte_range().contains_end(offset))
}

struct Mapped<'text> {
	needle: Cow<'text, str>,
	model: String,
	single_field: bool,
	range: ByteRange,
}

impl Backend {
	pub fn on_change_python(&self, text: &Text, uri: &Url, rope: Rope, old_rope: Option<Rope>) -> miette::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(tree_sitter_python::language())
			.expect("bug: failed to init python parser");
		self.update_ast(text, uri, rope.clone(), old_rope, parser)
	}
	pub async fn update_models(&self, text: Text, path: &str, root: Spur, rope: Rope) -> miette::Result<()> {
		let text = match text {
			Text::Full(text) => Cow::from(text),
			// TODO: Limit range of possible updates based on delta
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let models = index_models(text.as_bytes())?;
		let path = PathSymbol::strip_root(root, Path::new(path));
		self.index.models.append(path, interner(), true, &models).await;
		for model in models {
			match model.type_ {
				ModelType::Base { name, ancestors } => {
					let model_key = interner().get(&name).unwrap();
					let mut entry = self.index.models.try_get_mut(&model_key.into()).unwrap();
					entry.ancestors.extend(
						ancestors
							.into_iter()
							.map(|sym| ModelName::from(interner().get_or_intern(&sym))),
					);
					drop(entry);
					self.index.models.populate_field_names(model_key.into(), &[path]);
				}
				ModelType::Inherit(inherits) => {
					let Some(model) = inherits.first() else { continue };
					let model_key = interner().get(model).unwrap();
					self.index.models.populate_field_names(model_key.into(), &[path]);
				}
			}
		}
		Ok(())
	}
	pub async fn python_completions(
		&self,
		params: CompletionParams,
		ast: Tree,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, &rope) else {
			warn!("invalid position {:?}", params.text_document_position.position);
			return Ok(None);
		};
		let Some(current_module) = self
			.index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
		else {
			debug!("no current module");
			return Ok(None);
		};
		let mut cursor = tree_sitter::QueryCursor::new();
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let query = PyCompletions::query();
		let mut items = MaxVec::new(Self::LIMIT);
		let mut early_return = None;
		let mut this_model = ThisModel::default();
		// FIXME: This hack is necessary to drop !Send locals before await points.
		enum EarlyReturn<'a> {
			XmlId(Option<&'a str>, Symbol<Module>),
			Model,
			Access(String),
			Mapped { model: String, single_field: bool },
		}
		{
			let root = some!(top_level_stmt(ast.root_node(), offset));
			'match_: for match_ in cursor.matches(query, root, contents) {
				let mut model_filter = None;
				for capture in match_.captures {
					let range = capture.node.byte_range();
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::Request) => {
							model_filter = Some("ir.ui.view");
						}
						Some(PyCompletions::ForXmlId) => {
							let model = || {
								let model = capture.node.prev_named_sibling()?;
								let model =
									self.model_of_range(root, model.byte_range().map_unit(ByteOffset), contents)?;
								Some(interner().resolve(&model))
							};
							model_filter = model()
						}
						Some(PyCompletions::XmlId) if range.contains(&offset) => {
							let range = range.shrink(1);
							let needle = String::from_utf8_lossy(&contents[range.start..offset]);
							early_return = Some((
								EarlyReturn::XmlId(model_filter, current_module),
								needle,
								range.map_unit(ByteOffset),
								rope.clone(),
							));
							break 'match_;
						}
						Some(PyCompletions::Model) => {
							// to be checked later down that it only belongs to (assignment)
							let is_inherit = match_
								.nodes_for_capture_index(PyCompletions::Prop as _)
								.next()
								.map(|prop| &contents[prop.byte_range()] == b"_inherit")
								.unwrap_or(true);
							if is_inherit && range.contains_end(offset) {
								let Some(slice) = rope.get_byte_slice(range.clone()) else {
									dbg!(&range);
									break 'match_;
								};
								let relative_offset = range.start;
								let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
								early_return = Some((
									EarlyReturn::Model,
									needle,
									range.shrink(1).map_unit(ByteOffset),
									rope.clone(),
								));
								break 'match_;
							} else if range.end < offset {
								this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
							}
						}
						Some(PyCompletions::Mapped) if range.contains_end(offset) => {
							let Some(Mapped {
								needle,
								model,
								single_field,
								range,
							}) = self.gather_mapped(
								root,
								&match_,
								Some(offset),
								range.clone(),
								this_model.inner,
								contents,
								true,
							)
							else {
								break 'match_;
							};

							early_return =
								Some((EarlyReturn::Mapped { model, single_field }, needle, range, rope.clone()));
							break 'match_;
						}
						Some(PyCompletions::Depends)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Mapped)
						| Some(PyCompletions::XmlId)
						| Some(PyCompletions::Prop)
						| Some(PyCompletions::Scope)
						| Some(PyCompletions::ReadFn)
						| None => {}
					}
				}
			}
			if early_return.is_none() {
				// If the offset doesn't land on the attribute but right at the end,
				// move one place back to get the attribute.
				let identifier_offset = if contents[offset].is_ascii_alphanumeric() {
					offset
				} else {
					offset - 1
				};
				let (model, needle, range) = some!(self.attribute_at_offset(identifier_offset, root, contents));
				early_return = Some((
					EarlyReturn::Access(model.to_string()),
					needle,
					range.map_unit(ByteOffset),
					rope.clone(),
				));
			}
		}
		match early_return {
			Some((EarlyReturn::XmlId(model_filter, current_module), needle, range, rope)) => {
				self.complete_xml_id(&needle, range, rope, model_filter, current_module, &mut items)
					.await?;
				return Ok(Some(CompletionResponse::List(CompletionList {
					is_incomplete: !items.has_space(),
					items: items.into_inner(),
				})));
			}
			Some((EarlyReturn::Model, needle, range, rope)) => {
				self.complete_model(&needle, range, rope.clone(), &mut items).await?;
				return Ok(Some(CompletionResponse::List(CompletionList {
					is_incomplete: !items.has_space(),
					items: items.into_inner(),
				})));
			}
			Some((EarlyReturn::Access(model), needle, range, rope)) => {
				self.complete_field_name(&needle, range, model, rope.clone(), &mut items)?;
				return Ok(Some(CompletionResponse::List(CompletionList {
					is_incomplete: !items.has_space(),
					items: items.into_inner(),
				})));
			}
			Some((EarlyReturn::Mapped { model, single_field }, needle, mut range, rope)) => {
				// range:  foo.bar.baz
				// needle: foo.ba
				let mut needle = needle.as_ref();
				let mut model = some!(interner().get(model));
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range))
						.ok());
				}
				let model_name = interner().resolve(&model);
				self.complete_field_name(needle, range, model_name.to_string(), rope, &mut items)?;
				return Ok(Some(CompletionResponse::List(CompletionList {
					is_incomplete: !items.has_space(),
					items: items.into_inner(),
				})));
			}
			None => {}
		}
		Ok(None)
	}
	/// Gathers common information regarding a mapped access aka dot access.
	/// Only makes sense in [`PyCompletions`] queries.
	///
	/// Replacing:
	///
	///     "foo.bar.baz"
	///           ^cursor
	///      -----------range
	///      ------needle
	///
	/// Not replacing:
	///
	///     "foo.bar.baz"
	///           ^cursor
	///      -------range
	///      -------needle
	///
	fn gather_mapped<'text>(
		&self,
		root: Node,
		match_: &tree_sitter::QueryMatch,
		offset: Option<usize>,
		mut range: core::ops::Range<usize>,
		this_model: Option<&[u8]>,
		contents: &'text [u8],
		for_replacing: bool,
	) -> Option<Mapped<'text>> {
		let mut needle = if for_replacing {
			range = range.shrink(1);
			let offset = offset.unwrap_or(range.end);
			// Cow::from(rope.get_byte_slice(range.start..offset)?)
			String::from_utf8_lossy(&contents[range.start..offset])
		} else {
			// let slice = Cow::from(rope.get_byte_slice(range.clone().shrink(1))?);
			let slice = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
			let relative_start = range.start + 1;
			let offset = offset.unwrap_or((range.end - 1).max(relative_start));
			assert!(
				offset >= relative_start,
				"offset={} cannot be less than relative_start={}",
				offset,
				relative_start
			);
			let slice_till_end = &slice[offset - relative_start..];
			// How many characters until the next period or end-of-string?
			let limit = slice_till_end.find('.').unwrap_or(slice_till_end.len());
			range = relative_start..offset + limit;
			// Cow::from(rope.get_byte_slice(range.clone())?)
			String::from_utf8_lossy(&contents[range.clone()])
		};
		if needle == "|" || needle == "&" {
			return None;
		}

		log::trace!(
			"(gather_mapped) {} matches={match_:?}",
			String::from_utf8_lossy(&contents[range.clone()])
		);

		let model;
		if let Some(local_model) = match_.nodes_for_capture_index(PyCompletions::MappedTarget as _).next() {
			let model_ = self.model_of_range(root, local_model.byte_range().map_unit(ByteOffset), contents)?;
			model = interner().resolve(&model_).to_string();
		} else if let Some(field_model) = match_.nodes_for_capture_index(PyCompletions::Model as _).next() {
			// A sibling @MODEL node; this is defined on the `fields.*(comodel_name='@MODEL', domain=[..])` pattern
			model = String::from_utf8_lossy(&contents[field_model.byte_range().shrink(1)]).into_owned();
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
		}

		Some(Mapped {
			needle,
			model,
			single_field,
			range: range.map_unit(ByteOffset),
		})
	}
	pub fn python_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, &rope) else {
			return Err(miette!("could not find offset for {}", uri.path()));
		};
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		enum EarlyReturn<'a> {
			Access(Cow<'a, str>, &'a str),
			Mapped(Mapped<'a>),
		}
		let mut early_return = None;
		{
			let root = some!(top_level_stmt(ast.root_node(), offset));
			let query = PyCompletions::query();
			let mut cursor = tree_sitter::QueryCursor::new();
			let mut this_model = ThisModel::default();
			'match_: for match_ in cursor.matches(query, root, contents) {
				for capture in match_.captures {
					let range = capture.node.byte_range();
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::XmlId) if range.contains(&offset) => {
							let range = range.shrink(1);
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							return self
								.jump_def_xml_id(&slice, &params.text_document_position_params.text_document.uri);
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
								let Some(slice) = rope.get_byte_slice(range.clone()) else {
									dbg!(&range);
									break 'match_;
								};
								let slice = Cow::from(slice);
								return self.jump_def_model(&slice);
							} else if range.end < offset {
								this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
							}
						}
						Some(PyCompletions::Mapped) if range.contains_end(offset) => {
							early_return = self
								.gather_mapped(
									root,
									&match_,
									Some(offset),
									range.clone(),
									this_model.inner,
									contents,
									false,
								)
								.map(EarlyReturn::Mapped);
							break 'match_;
						}
						Some(PyCompletions::Request)
						| Some(PyCompletions::Mapped)
						| Some(PyCompletions::ForXmlId)
						| Some(PyCompletions::XmlId)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Depends)
						| Some(PyCompletions::Prop)
						| Some(PyCompletions::ReadFn)
						| Some(PyCompletions::Scope)
						| None => {}
					}
				}
			}
			if early_return.is_none() {
				let (model, needle, _) = some!(self.attribute_at_offset(offset, root, contents));
				early_return = Some(EarlyReturn::Access(needle, model));
			}
		}
		match early_return {
			Some(EarlyReturn::Access(field, model)) => {
				return self.jump_def_field_name(&field, model);
			}
			Some(EarlyReturn::Mapped(Mapped {
				needle,
				model,
				single_field,
				mut range,
			})) => {
				let mut needle = needle.as_ref();
				let mut model = interner().get_or_intern(model);
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range))
						.ok());
				}
				let model = interner().resolve(&model);
				return self.jump_def_field_name(needle, model);
			}
			None => {}
		}
		Ok(None)
	}
	/// Resolves the attribute and the object's model at the cursor offset
	/// using [`model_of_range`][Backend::model_of_range].
	///
	/// Returns `(model, field, range)`.
	fn attribute_at_offset<'out>(
		&'out self,
		offset: usize,
		root: Node<'out>,
		contents: &'out [u8],
	) -> Option<(&str, Cow<str>, core::ops::Range<usize>)> {
		let (lhs, field, range) = self.attribute_node_at_offset(offset, root, contents)?;
		let model = self.model_of_range(root, lhs.byte_range().map_unit(ByteOffset), contents)?;
		let model = interner().resolve(&model);
		Some((model, field, range))
	}
	/// Resolves the attribute at the cursor offset.
	/// Returns `(object, field, range)`
	pub fn attribute_node_at_offset<'out>(
		&'out self,
		mut offset: usize,
		root: Node<'out>,
		contents: &'out [u8],
	) -> Option<(Node, Cow<str>, core::ops::Range<usize>)> {
		if contents.is_empty() {
			return None;
		}
		offset = offset.clamp(0, contents.len() - 1);
		let mut cursor_node = root.descendant_for_byte_range(offset, offset)?;
		if cursor_node == root {
			// We got our cursor left in the middle of nowhere.
			offset = offset.saturating_sub(1);
			cursor_node = root.descendant_for_byte_range(offset, offset)?;
		}
		trace!(
			"(attribute_node_to_offset) {} cursor={}\n  root={}\n  sexp={}",
			String::from_utf8_lossy(&contents[cursor_node.byte_range()]),
			contents[offset] as char,
			root.to_sexp(),
			cursor_node.to_sexp(),
		);
		let lhs;
		let rhs;
		if !cursor_node.is_named() {
			// We landed on one of the punctuations inside the attribute.
			// Need to determine which one is it.
			let dot = cursor_node.descendant_for_byte_range(offset, offset)?;
			lhs = dot.prev_named_sibling()?;
			rhs = dot.next_named_sibling().and_then(|attr| match attr.kind() {
				"identifier" => Some(attr),
				// TODO: Unwrap all layers of attributes
				"attribute" => attr
					.child_by_field_name("object")
					.and_then(|obj| (obj.kind() == "identifier").then_some(obj)),
				_ => None,
			});
		} else if cursor_node.kind() == "attribute" {
			lhs = cursor_node.child_by_field_name("object")?;
			rhs = cursor_node.child_by_field_name("attribute");
		} else {
			lhs = match cursor_node.parent() {
				Some(parent) if parent.kind() == "attribute" => parent.child_by_field_name("object")?,
				_ => return None,
			};
			rhs = Some(cursor_node);
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
			return Some((lhs, Cow::from(""), offset + 1..offset + 1));
		};
		let (field, range) = if rhs.range().start_point.row != lhs.range().end_point.row {
			// tree-sitter has an issue with attributes spanning multiple lines
			// which is NOT valid Python, but allows it anyways because tree-sitter's
			// use cases don't require strict syntax trees.
			(Cow::from(""), offset + 1..offset + 1)
		} else {
			let range = rhs.byte_range();
			(String::from_utf8_lossy(&contents[range.clone()]), range)
		};

		Some((lhs, field, range))
	}
	pub fn python_references(&self, params: ReferenceParams, rope: Rope) -> miette::Result<Option<Vec<Location>>> {
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, &rope) else {
			return Ok(None);
		};
		let uri = &params.text_document_position.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let mut cursor = tree_sitter::QueryCursor::new();
		let current_module = self
			.index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()));
		'match_: for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let slice = Cow::from(slice);
						return self.record_references(&slice, current_module);
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
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							let slice = some!(interner().get(slice));
							return self.model_references(&slice.into());
						}
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
					| None => {}
				}
			}
		}
		Ok(None)
	}

	pub fn python_hover(&self, params: HoverParams, rope: Rope) -> miette::Result<Option<Hover>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, &rope) else {
			Err(diagnostic!("could not find offset for {}", uri.path()))?
		};

		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		enum EarlyReturn<'text> {
			Access {
				field: Cow<'text, str>,
				model: ModelName,
				lsp_range: Range,
			},
			Mapped(Mapped<'text>),
		}
		let mut early_return = None;
		{
			let root = some!(top_level_stmt(ast.root_node(), offset));
			let query = PyCompletions::query();
			let mut cursor = tree_sitter::QueryCursor::new();
			let mut this_model = ThisModel::default();
			'match_: for match_ in cursor.matches(query, root, contents) {
				for capture in match_.captures {
					let range = capture.node.byte_range();
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::Model) => {
							if range.contains(&offset) {
								let range = range.shrink(1);
								let lsp_range = ts_range_to_lsp_range(capture.node.range());
								let Some(slice) = rope.get_byte_slice(range.clone()) else {
									dbg!(&range);
									break 'match_;
								};
								let slice = Cow::from(slice);
								return self.hover_model(&slice, Some(lsp_range), false, None);
							} else if range.end < offset {
								this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
							}
						}
						Some(PyCompletions::Mapped) if range.contains(&offset) => {
							early_return = self
								.gather_mapped(
									root,
									&match_,
									Some(offset),
									range.clone(),
									this_model.inner,
									contents,
									false,
								)
								.map(EarlyReturn::Mapped);
							break 'match_;
						}
						Some(PyCompletions::XmlId) if range.contains(&offset) => {
							let xml_id = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
							return self
								.hover_record(&xml_id, offset_range_to_lsp_range(range.map_unit(ByteOffset), rope));
						}
						Some(PyCompletions::Request)
						| Some(PyCompletions::XmlId)
						| Some(PyCompletions::Mapped)
						| Some(PyCompletions::ForXmlId)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Depends)
						| Some(PyCompletions::Prop)
						| Some(PyCompletions::ReadFn)
						| Some(PyCompletions::Scope)
						| None => {}
					}
				}
			}
			if early_return.is_none() {
				early_return = self
					.attribute_at_offset(offset, root, contents)
					.and_then(|(model, field, range)| {
						let model = interner().get(model)?.into();
						Some(EarlyReturn::Access {
							field,
							model,
							lsp_range: offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone())?,
						})
					});
			}
		}
		match early_return {
			Some(EarlyReturn::Access {
				field,
				model,
				lsp_range,
			}) => {
				let model = interner().resolve(&model);
				return self.hover_field_name(&field, model, Some(lsp_range));
			}
			Some(EarlyReturn::Mapped(Mapped {
				needle,
				model,
				single_field,
				mut range,
			})) => {
				let mut needle = needle.as_ref();
				let mut model = interner().get_or_intern(model);
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range))
						.ok());
				}
				let model = interner().resolve(&model);
				return self.hover_field_name(needle, model, offset_range_to_lsp_range(range, rope.clone()));
			}
			None => {}
		}

		// No matches, assume arbitrary expression.
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let lsp_range = ts_range_to_lsp_range(needle.range());
		let model = some!(self.model_of_range(root, needle.byte_range().map_unit(ByteOffset), contents));
		let model = interner().resolve(&model);
		let identifier =
			(needle.kind() == "identifier").then(|| String::from_utf8_lossy(&contents[needle.byte_range()]));
		self.hover_model(model, Some(lsp_range), true, identifier.as_deref())
	}
	pub fn diagnose_python(
		&self,
		path: &str,
		rope: &Rope,
		damage_zone: Option<ByteRange>,
		diagnostics: &mut Vec<Diagnostic>,
	) {
		let Some(ast) = self.ast_map.get(path) else {
			warn!("Did not build AST for {path}");
			return;
		};
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let query = PyCompletions::query();
		let mut root = ast.root_node();
		// TODO: Limit range of diagnostics with new heuristics
		if let Some(zone) = damage_zone.as_ref() {
			root = top_level_stmt(root, zone.end.0).unwrap_or(root);
			diagnostics.retain(|diag| {
				// If we couldn't get a range here, rope has changed significantly so just toss the diag.
				let range = lsp_range_to_offset_range(diag.range, rope).unwrap_or_default();
				!root.byte_range().contains(&range.start.0)
			});
		} else {
			// There is no damage zone, assume everything has been reset.
			diagnostics.clear();
		}
		let in_active_root =
			|range: core::ops::Range<usize>| damage_zone.as_ref().map(|zone| zone.intersects(range)).unwrap_or(true);
		let top_level_ranges = root
			.named_children(&mut root.walk())
			.map(|node| node.byte_range())
			.collect::<Vec<_>>();
		let mut cursor = QueryCursor::new();
		let mut this_model = ThisModel::default();
		for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						let xml_id = String::from_utf8_lossy(&contents[capture.node.byte_range().shrink(1)]);
						let xml_id_key = interner().get(&xml_id);
						let mut id_found = false;
						if let Some(id) = xml_id_key {
							id_found = self.index.records.contains_key(&id.into());
						}
						if !id_found {
							diagnostics.push(Diagnostic {
								range: ts_range_to_lsp_range(capture.node.range()),
								message: format!("No XML record with ID `{xml_id}` found"),
								severity: Some(DiagnosticSeverity::WARNING),
								..Default::default()
							})
						}
					}
					Some(PyCompletions::Model) => {
						let Ok(idx) = top_level_ranges.binary_search_by(|range| {
							let needle = capture.node.end_byte();
							if needle < range.start {
								Ordering::Greater
							} else if needle > range.end {
								Ordering::Less
							} else {
								Ordering::Equal
							}
						}) else {
							debug!("binary search for top-level range failed");
							continue;
						};
						this_model.tag_model(capture.node, &match_, top_level_ranges[idx].clone(), contents);
					}
					Some(PyCompletions::Mapped) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						let Some(Mapped {
							needle,
							model,
							single_field,
							mut range,
						}) = self.gather_mapped(
							root,
							&match_,
							None,
							capture.node.byte_range(),
							this_model.inner,
							// &rope,
							contents,
							false,
						)
						else {
							continue;
						};
						let mut model = interner().get_or_intern(&model);
						let mut needle = needle.as_ref();
						if single_field {
							if let Some(dot) = needle.find('.') {
								let message_range = range.start.0 + dot..range.end.0;
								diagnostics.push(Diagnostic {
									range: offset_range_to_lsp_range(message_range.map_unit(ByteOffset), rope.clone())
										.unwrap(),
									severity: Some(DiagnosticSeverity::ERROR),
									message: "Dotted access is not supported in this context".to_string(),
									..Default::default()
								});
								needle = &needle[..dot];
								range = (range.start.0..range.start.0 + dot).map_unit(ByteOffset);
							}
						} else {
							match self
								.index
								.models
								.resolve_mapped(&mut model, &mut needle, Some(&mut range))
							{
								Ok(()) => {}
								Err(ResolveMappedError::NonRelational) => {
									diagnostics.push(Diagnostic {
										range: offset_range_to_lsp_range(range, rope.clone()).unwrap(),
										severity: Some(DiagnosticSeverity::ERROR),
										message: format!("`{needle}` is not a relational field"),
										..Default::default()
									});
									continue;
								}
							}
						}
						if needle.is_empty() {
							// Nothing to compare yet, keep going.
							continue;
						}
						let mut has_field = false;
						if self.index.models.contains_key(&model.into()) {
							let Some(entry) = self.index.models.populate_field_names(model.into(), &[]) else {
								continue;
							};
							let Some(fields) = entry.fields.as_ref() else { continue };
							static MAPPED_BUILTINS: phf::Set<&str> =
								phf::phf_set!("id", "display_name", "create_date", "write_date");
							if MAPPED_BUILTINS.contains(needle) {
								continue;
							}
							if let Some(key) = interner().get(needle) {
								has_field = fields.contains_key(&key.into());
							}
						}
						if !has_field {
							diagnostics.push(Diagnostic {
								range: offset_range_to_lsp_range(range, rope.clone()).unwrap(),
								severity: Some(DiagnosticSeverity::WARNING),
								message: format!("Model `{}` has no field `{needle}`", interner().resolve(&model)),
								..Default::default()
							});
						}
					}
					Some(PyCompletions::Scope) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						// Most of these steps are similar to what is done inside model_of_range.
						let offset = capture.node.start_byte();
						let Some((self_type, fn_scope, self_param)) = determine_scope(root, contents, offset) else {
							continue;
						};
						let mut scope = Scope::default();
						let self_type = match self_type {
							Some(type_) => &contents[type_.byte_range().shrink(1)],
							None => &[],
						};
						scope.super_ = Some(self_param.as_ref().into());
						scope.insert(
							self_param.into_owned(),
							Type::Model(String::from_utf8_lossy(self_type).as_ref().into()),
						);
						let scope_end = fn_scope.end_byte();
						self.walk_scope(fn_scope, Some(scope), |scope, node| {
							let entered = self.build_scope(scope, node, scope_end, contents)?;

							let attribute = node.child_by_field_name("attribute");
							if node.kind() != "attribute" || attribute.as_ref().unwrap().kind() != "identifier" {
								return ControlFlow::Continue(entered);
							}

							let attribute = attribute.unwrap();
							static MODEL_BUILTINS: phf::Set<&str> =
								phf::phf_set!("env", "id", "ids", "display_name", "create_date", "write_date");
							let prop = String::from_utf8_lossy(&contents[attribute.byte_range()]);
							if prop.starts_with(' ') || MODEL_BUILTINS.contains(&prop) {
								return ControlFlow::Continue(entered);
							}

							let Some(lhs_t) =
								self.type_of(node.child_by_field_name("object").unwrap(), scope, contents)
							else {
								return ControlFlow::Continue(entered);
							};

							let Some(model_name) = self.resolve_type(&lhs_t, scope) else {
								return ControlFlow::Continue(entered);
							};

							if self.has_attribute(&lhs_t, &contents[attribute.byte_range()], scope) {
								return ControlFlow::Continue(entered);
							}

							diagnostics.push(Diagnostic {
								range: ts_range_to_lsp_range(attribute.range()),
								severity: Some(DiagnosticSeverity::ERROR),
								message: format!(
									"Model `{}` has no field `{}",
									interner().resolve(&model_name),
									String::from_utf8_lossy(&contents[attribute.byte_range()]),
								),
								..Default::default()
							});

							ControlFlow::Continue(entered)
						});
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| None => {}
				}
			}
		}
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

#[cfg(test)]
mod tests {
	use super::*;
	use odoo_lsp::model::ModelFields;
	use pretty_assertions::assert_eq;

	/// Tricky behavior here. The query syntax must match the trailing comma between
	/// named arguments, and this test checks that. Furthermore, @help cannot be matched
	/// as a `(string)` since that would reify its shape and refuse subsequent matches.
	#[test]
	fn test_model_fields() {
		let mut parser = Parser::new();
		parser.set_language(tree_sitter_python::language()).unwrap();
		let contents = br#"
class Foo(models.Model):
	foo = fields.Char('asd', help='asd')
	bar = fields.Many2one(comodel_name='asd', help='asd')
	what = fields.What(asd)
	haha = fields.Many2many('asd')
	html = fields.Html(related='asd', foo=123, help='asdf')"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = ModelFields::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[
			&["foo", "fields", "Char", "'asd'", "help", "'asd'"],
			&["bar", "fields", "Many2one", "comodel_name", "'asd'", "help", "'asd'"],
			&["what", "fields", "What"],
			&["haha", "fields", "Many2many", "'asd'"],
			&[
				"html", "fields", "Html", "related", "'asd'", "foo", "123", "help", "'asdf'",
			],
		];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}

	#[test]
	fn test_py_completions() {
		let mut parser = Parser::new();
		parser.set_language(tree_sitter_python::language()).unwrap();
		let contents = br#"
self.env.ref('ref')
env['model']
request.render('template')
foo = fields.Char()
bar = fields.Many2one('positional')
baz = fields.Many2many(comodel_name='named')
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = PyCompletions::query();
		let mut cursor = QueryCursor::new();
		let expected = vec![
			(0, vec!["env", "ref", "'ref'"]),
			(1, vec!["env", "'model'"]),
			(0, vec!["request", "render", "'template'"]),
			(4, vec!["fields", "Many2one", "'positional'"]),
			(4, vec!["fields", "Many2many", "comodel_name", "'named'"]),
		];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				(
					match_.pattern_index,
					match_
						.captures
						.iter()
						.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
						.collect::<Vec<_>>(),
				)
			})
			.collect::<Vec<_>>();
		let actual = actual
			.iter()
			.map(|(index, captures)| (*index, captures.iter().map(|x| x.as_ref()).collect::<Vec<_>>()))
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}

	#[test]
	fn test_py_completions_class_scoped() {
		let mut parser = Parser::new();
		parser.set_language(tree_sitter_python::language()).unwrap();
		let contents = br#"
class Foo(models.AbstractModel):
	_name = 'foo'
	_inherit = ['inherit_foo', 'inherit_bar']
	foo = fields.Char(related='related')
	@api.constrains('mapped', 'meh')
	def foo(self):
		what = self.sudo().mapped('ha.ha')
	def bar(self):
		pass
	foo = fields.Foo()
	@api.depends('mapped2')
	@api.depends_context('uid')
	def another(self):
		pass
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = PyCompletions::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[
			&["_name", "'foo'"],
			&["_inherit", "'inherit_foo'", "'inherit_bar'"],
			&["foo", "fields", "related", "'related'"],
			&["api", "constrains", "'mapped'"],
			&["api", "constrains", "'meh'"],
			&["<scope>"],
			&["self.sudo()", "mapped", "'ha.ha'"],
			&["<scope>"],
			&["foo", "fields"],
			&["api", "depends", "'mapped2'"],
			&["<scope>"],
		];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.map(|capture| match PyCompletions::from(capture.index) {
						Some(PyCompletions::Scope) => Cow::from("<scope>"),
						_ => String::from_utf8_lossy(&contents[capture.node.byte_range()]),
					})
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}
}
