use crate::{Backend, Text};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;

use dashmap::try_result::TryResult;
use lasso::Key;
use log::{debug, error, warn};
use miette::diagnostic;
use odoo_lsp::index::{index_models, interner, Module, Symbol};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Parser, QueryCursor, QueryMatch, Tree};

use odoo_lsp::model::{ModelName, ModelType};
use odoo_lsp::utils::*;
use odoo_lsp::{format_loc, some};
use ts_macros::query;

query! {
	PyCompletions(Request, XmlId, Mapped, MappedTarget, Depends, Model, Access, Prop, ForXmlId);
r#"
((call [
	(attribute [(identifier) @_env (attribute (_) (identifier) @_env)] (identifier) @_ref)
	(attribute (identifier) @REQUEST (identifier) @_render)
	(attribute (_) (identifier) @FOR_XML_ID)
] (argument_list . (string) @XML_ID))
(#eq? @_env "env")
(#eq? @_ref "ref")
(#eq? @REQUEST "request")
(#eq? @_render "render")
(#eq? @FOR_XML_ID "_for_xml_id"))

(subscript
	[(identifier) @_env (attribute (_) (identifier) @_env)] (string) @MODEL
(#eq? @_env "env"))

((class_definition
	(block
		(expression_statement
			(assignment
				(identifier) @PROP
				[(string) @MODEL
				 (list ((string) @MODEL ","?)*)
				 (call
					(attribute (identifier) @_fields (identifier))
					(argument_list
						(keyword_argument (identifier) @_related (string) @MAPPED)?))]))))
(#eq? @_fields "fields")
(#eq? @_related "related"))

((call
	[(attribute (_) @MAPPED_TARGET (identifier) @_mapper)
	 (attribute (identifier) @_api (identifier) @DEPENDS)]
	(argument_list (string) @MAPPED))
(#match? @_mapper "^(mapp|filter|sort)ed$")
(#eq? @_api "api")
(#match? @DEPENDS "^(depends|constrains|onchange)$"))

((call
	(attribute (_) @MAPPED_TARGET (identifier) @_search)
	(argument_list
		[(list
			[(tuple . (string) @MAPPED)
			 (parenthesized_expression (string) @MAPPED)])
		 (keyword_argument (identifier) @_domain
			(list
				[(tuple . (string) @MAPPED)
			 	 (parenthesized_expression (string) @MAPPED)]))]))
(#eq? @_domain "domain")
(#match? @_search "^(search(_(read|count))?|read_group|filtered_domain)$"))

((call [
	(identifier) @_Field
    (attribute (identifier) @_fields (identifier) @_Field)
] [
 	(argument_list . (string) @MODEL)
    (argument_list
    	(keyword_argument (identifier) @_comodel_name (string) @MODEL))
])
(#match? @_Field "^(Many2one|One2many|Many2many)$")
(#eq? @_comodel_name "comodel_name"))

((call
	(attribute (_) @MAPPED_TARGET (identifier) @DEPENDS)
	(argument_list . [
		(set (string) @MAPPED)
		(dictionary [ 
			(pair key: (string) @MAPPED)
			(ERROR (string) @MAPPED)
		])
	]))
(#eq? @DEPENDS "write"))

((call
	(attribute (_) @MAPPED_TARGET (identifier) @DEPENDS)
	(argument_list . [
		(set (string) @MAPPED)	
		(dictionary [
			(pair key: (string) @MAPPED)
			(ERROR (string) @MAPPED)
		])
		(list [
			(set (string) @MAPPED)
			(dictionary [
				(pair key: (string) @MAPPED)
				(ERROR (string) @MAPPED)
			])
		])
	]))
(#eq? @DEPENDS "create"))

(attribute (_) (identifier) @ACCESS)"#
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
	pub async fn update_models(&self, text: Text, uri: &Url, rope: Rope) -> miette::Result<()> {
		let text = match text {
			Text::Full(text) => Cow::from(text),
			// TODO: Limit range of possible updates based on delta
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let models = index_models(text.as_bytes())?;
		let path = interner().get_or_intern(uri.path());
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
			error!(format_loc!("python_completions: invalid offset"));
			return Ok(None);
		};
		let Some(current_module) = self
			.index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
		else {
			debug!(format_loc!("python_completions: no current_module"));
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
			'match_: for match_ in cursor.matches(query, root, &contents[..]) {
				let mut model_filter = None;
				for capture in match_.captures {
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::Request) => {
							model_filter = Some("ir.ui.view");
						}
						Some(PyCompletions::ForXmlId) => {
							let model = || {
								let model = capture.node.prev_named_sibling()?;
								let model = self.model_of_range(
									root,
									model.byte_range().map_unit(ByteOffset),
									None,
									&contents,
								)?;
								Some(interner().resolve(&model))
							};
							model_filter = model()
						}
						Some(PyCompletions::XmlId) => {
							let range = capture.node.byte_range();
							if range.contains(&offset) {
								let range = range.shrink(1);
								let needle = String::from_utf8_lossy(&contents[range.start..offset]);
								early_return = Some((
									EarlyReturn::XmlId(model_filter, current_module.into()),
									needle,
									range.map_unit(ByteOffset),
									rope.clone(),
								));
								break 'match_;
							}
						}
						Some(PyCompletions::Model) => {
							let range = capture.node.byte_range();
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
						Some(PyCompletions::Mapped) => {
							let range = capture.node.byte_range();
							if range.contains_end(offset) {
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
									&rope,
									&contents[..],
									true,
								)
								else {
									break 'match_;
								};

								early_return =
									Some((EarlyReturn::Mapped { model, single_field }, needle, range, rope.clone()));
								break 'match_;
							}
						}
						Some(PyCompletions::Access) => {
							let range = capture.node.byte_range();
							if range.contains_end(offset) {
								let Some(slice) = rope.get_byte_slice(range.clone()) else {
									dbg!(&range);
									break 'match_;
								};
								let lhs = some!(capture.node.prev_named_sibling());
								let model = some!(self.model_of_range(
									root,
									lhs.byte_range().map_unit(ByteOffset),
									None,
									&contents
								));
								let model = interner().resolve(&model);
								let needle = Cow::from(slice.byte_slice(..offset - range.start));
								early_return = Some((
									EarlyReturn::Access(model.to_string()),
									needle,
									range.map_unit(ByteOffset),
									rope.clone(),
								));
								break 'match_;
							}
						}
						Some(PyCompletions::Depends)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Prop)
						| None => {}
					}
				}
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
				self.complete_field_name(&needle, range, model, rope.clone(), &mut items)
					.await?;
				return Ok(Some(CompletionResponse::List(CompletionList {
					is_incomplete: !items.has_space(),
					items: items.into_inner(),
				})));
			}
			Some((EarlyReturn::Mapped { model, single_field }, needle, mut range, rope)) => {
				// range:  foo.bar.baz
				// needle: foo.ba
				let mut needle = needle.as_ref();
				let mut model = some!(interner().get(&model));
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range)));
				}
				let model_name = interner().resolve(&model);
				self.complete_field_name(needle, range, model_name.to_string(), rope, &mut items)
					.await?;
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
	/// 	"foo.bar.baz"
	///           ^cursor
	///      -----------range
	///      ------needle
	///
	/// Not replacing:
	///
	/// 	"foo.bar.baz"
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
		rope: &'text Rope,
		contents: &[u8],
		for_replacing: bool,
	) -> Option<Mapped<'text>> {
		let needle = if for_replacing {
			range = range.shrink(1);
			let offset = offset.unwrap_or(range.end);
			Cow::from(rope.get_byte_slice(range.start..offset)?)
		} else {
			let slice = Cow::from(rope.get_byte_slice(range.clone().shrink(1))?);
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
			Cow::from(rope.get_byte_slice(range.clone())?)
		};

		let model;
		if let Some(local_model) = match_.nodes_for_capture_index(PyCompletions::MappedTarget as _).next() {
			let model_ = self.model_of_range(root, local_model.byte_range().map_unit(ByteOffset), None, &contents)?;
			model = interner().resolve(&model_).to_string();
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
		}

		Some(Mapped {
			needle,
			model,
			single_field,
			range: range.map_unit(ByteOffset),
		})
	}
	pub async fn python_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
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
			'match_: for match_ in cursor.matches(query, root, &contents[..]) {
				for capture in match_.captures {
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::XmlId) => {
							let range = capture.node.byte_range();
							if range.contains(&offset) {
								let range = range.shrink(1);
								let Some(slice) = rope.get_byte_slice(range.clone()) else {
									dbg!(&range);
									break 'match_;
								};
								let slice = Cow::from(slice);
								return self
									.jump_def_xml_id(&slice, &params.text_document_position_params.text_document.uri);
							}
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
						Some(PyCompletions::Access) => {
							let range = capture.node.byte_range();
							if range.contains_end(offset) {
								let lhs = some!(capture.node.prev_named_sibling());
								let lhs = lhs.byte_range().map_unit(ByteOffset);
								let model = some!(self.model_of_range(root, lhs, None, &contents));
								let field = String::from_utf8_lossy(&contents[range]);
								let model = interner().resolve(&model);
								early_return = Some(EarlyReturn::Access(field, model));
								break 'match_;
							}
						}
						Some(PyCompletions::Mapped) => {
							let range = capture.node.byte_range();
							if range.contains_end(offset) {
								early_return = self
									.gather_mapped(
										root,
										&match_,
										Some(offset),
										range.clone(),
										this_model.inner,
										&rope,
										&contents,
										false,
									)
									.map(EarlyReturn::Mapped);
								break 'match_;
							}
						}
						Some(PyCompletions::Request)
						| Some(PyCompletions::ForXmlId)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Depends)
						| Some(PyCompletions::Prop)
						| None => {}
					}
				}
			}
		}
		match early_return {
			Some(EarlyReturn::Access(field, model)) => {
				return self.jump_def_field_name(&field, model).await;
			}
			Some(EarlyReturn::Mapped(Mapped {
				needle,
				model,
				single_field,
				mut range,
			})) => {
				let mut needle = needle.as_ref();
				let mut model = interner().get_or_intern(&model);
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range)));
				}
				let model = interner().resolve(&model);
				return self.jump_def_field_name(needle, model).await;
			}
			None => {}
		}
		Ok(None)
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
		'match_: for match_ in cursor.matches(query, root, &contents[..]) {
			for capture in match_.captures {
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) => {
						let range = capture.node.byte_range();
						if range.contains(&offset) {
							let range = range.shrink(1);
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							return self.record_references(&slice, current_module);
						}
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
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::Mapped)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Access)
					| Some(PyCompletions::Prop)
					| None => {}
				}
			}
		}
		Ok(None)
	}

	pub async fn python_hover(&self, params: HoverParams, rope: Rope) -> miette::Result<Option<Hover>> {
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
			'match_: for match_ in cursor.matches(query, root, &contents[..]) {
				for capture in match_.captures {
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::Model) => {
							let range = capture.node.byte_range();
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
						Some(PyCompletions::Access) => {
							let range = capture.node.byte_range();
							if range.contains(&offset) {
								let lsp_range = ts_range_to_lsp_range(capture.node.range());
								let lhs = some!(capture.node.prev_named_sibling());
								let lhs = lhs.byte_range().map_unit(ByteOffset);
								let model = some!(self.model_of_range(root, lhs, None, &contents));
								let field = String::from_utf8_lossy(&contents[range]);
								early_return = Some(EarlyReturn::Access {
									field,
									model,
									lsp_range,
								});
								break 'match_;
							}
						}
						Some(PyCompletions::Mapped) => {
							let range = capture.node.byte_range();
							if range.contains(&offset) {
								early_return = self
									.gather_mapped(
										root,
										&match_,
										Some(offset),
										range.clone(),
										this_model.inner,
										&rope,
										&contents[..],
										false,
									)
									.map(EarlyReturn::Mapped);
								break 'match_;
							}
						}
						Some(PyCompletions::XmlId) => {
							let range = capture.node.byte_range();
							if range.contains(&offset) {
								let xml_id = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
								return self.hover_record(
									&xml_id,
									offset_range_to_lsp_range(range.map_unit(ByteOffset), rope),
								);
							}
						}
						Some(PyCompletions::Request)
						| Some(PyCompletions::ForXmlId)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Depends)
						| Some(PyCompletions::Prop)
						| None => {}
					}
				}
			}
		}
		match early_return {
			Some(EarlyReturn::Access {
				field,
				model,
				lsp_range,
			}) => {
				let model = interner().resolve(&model);
				return self.hover_field_name(&field, model, Some(lsp_range)).await;
			}
			Some(EarlyReturn::Mapped(Mapped {
				needle,
				model,
				single_field,
				mut range,
			})) => {
				let mut needle = needle.as_ref();
				let mut model = interner().get_or_intern(&model);
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range)));
				}
				let model = interner().resolve(&model);
				return self
					.hover_field_name(needle, model, offset_range_to_lsp_range(range, rope.clone()))
					.await;
			}
			None => {}
		}

		// No matches, assume arbitrary expression.
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let lsp_range = ts_range_to_lsp_range(needle.range());
		let model = some!(self.model_of_range(root, needle.byte_range().map_unit(ByteOffset), None, &contents));
		let model = interner().resolve(&model);
		let identifier =
			(needle.kind() == "identifier").then(|| String::from_utf8_lossy(&contents[needle.byte_range()]));
		self.hover_model(model, Some(lsp_range), true, identifier.as_deref())
	}
	pub fn diagnose_python(
		&self,
		uri: &Url,
		rope: &Rope,
		damage_zone: Option<ByteRange>,
		diagnostics: &mut Vec<Diagnostic>,
	) {
		let path = uri.path();
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
				let range = lsp_range_to_offset_range(diag.range.clone(), &rope).unwrap();
				!root.byte_range().contains(&range.start.0)
			});
		}
		let in_active_root =
			|range: core::ops::Range<usize>| damage_zone.as_ref().map(|zone| zone.intersects(range)).unwrap_or(true);
		let top_level_ranges = root
			.named_children(&mut root.walk())
			.map(|node| node.byte_range())
			.collect::<Vec<_>>();
		let mut cursor = QueryCursor::new();
		let mut this_model = ThisModel::default();
		for match_ in cursor.matches(query, root, &contents[..]) {
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
							debug!("(diagnose_python) binary search for top-level range failed");
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
							&rope,
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
							self.index
								.models
								.resolve_mapped(&mut model, &mut needle, Some(&mut range));
						}
						if needle.is_empty() {
							// Nothing to compare yet, keep going.
							continue;
						}
						let models = self.index.models.get(&model.into());
						let mut has_field_or_unresolved = models.is_none();
						if let (Some(model), Some(field)) =
							(self.index.models.get(&model.into()), interner().get(needle))
						{
							if let Some(fields) = model.fields.as_ref() {
								has_field_or_unresolved = fields.contains_key(field.into_usize() as _);
							} else {
								has_field_or_unresolved = true;
							}
						}

						if !has_field_or_unresolved {
							diagnostics.push(Diagnostic {
								range: offset_range_to_lsp_range(range, rope.clone()).unwrap(),
								severity: Some(DiagnosticSeverity::WARNING),
								message: format!("Model `{}` has no field `{needle}`", interner().resolve(&model)),
								..Default::default()
							});
						}
					}
					Some(PyCompletions::Access) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						if let Some(gp) = capture.node.parent().expect("attribute").parent() {
							if gp.kind() == "call" {
								// TODO: Index methods
								continue;
							}
						}
						let prop = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
						static MODEL_BUILTINS: phf::Set<&str> =
							phf::phf_set!("env", "id", "ids", "display_name", "create_date", "write_date");
						if prop.starts_with('_') || MODEL_BUILTINS.contains(&prop) {
							continue;
						}
						let Some(obj) = capture.node.prev_named_sibling() else {
							continue;
						};
						let Some(model) =
							self.model_of_range(root, obj.byte_range().map_unit(ByteOffset), None, &contents[..])
						else {
							continue;
						};
						let TryResult::Present(entry) = self.index.models.try_get(&model) else {
							continue;
						};
						let Some(fields) = entry.fields.as_ref() else { continue };
						let prop_key = interner().get(&prop);
						if !prop_key
							.map(|key| fields.contains_key(key.into_usize() as _))
							.unwrap_or(true)
						{
							diagnostics.push(Diagnostic {
								range: ts_range_to_lsp_range(capture.node.range()),
								severity: Some(DiagnosticSeverity::WARNING),
								message: format!("Model `{}` has no field `{prop}`", interner().resolve(&model)),
								..Default::default()
							});
						}
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
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
	use tree_sitter::QueryCursor;

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
		let expected: &[&[&str]] = &[
			&["env"],
			&["ref"],
			&["env", "ref", "'ref'"],
			&["env", "'model'"],
			&["render"],
			&["request", "render", "'template'"],
			&["Char"],
			&["Many2one"],
			&["fields", "Many2one", "'positional'"],
			&["Many2many"],
			&["fields", "Many2many", "comodel_name", "'named'"],
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
			&["AbstractModel"],
			&["_name", "'foo'"],
			&["_inherit", "'inherit_foo'", "'inherit_bar'"],
			&["Char"],
			&["foo", "fields", "related", "'related'"],
			&["constrains"],
			&["api", "constrains", "'mapped'"],
			&["api", "constrains", "'meh'"],
			&["sudo"],
			&["mapped"],
			&["self.sudo()", "mapped", "'ha.ha'"],
			&["Foo"],
			&["foo", "fields"],
			&["depends"],
			&["api", "depends", "'mapped2'"],
			&["depends_context"],
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
}
