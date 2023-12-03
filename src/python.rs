use crate::{Backend, Text};

use std::borrow::Cow;
use std::path::Path;

use log::{debug, error};
use miette::diagnostic;
use odoo_lsp::index::{index_models, interner, Module, Symbol};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Parser, Tree};

use odoo_lsp::model::{ModelName, ModelType};
use odoo_lsp::utils::*;
use odoo_lsp::{format_loc, some};
use ts_macros::query;

query! {
	PyCompletions(REQUEST, XML_ID, MAPPED, MAPPED_TARGET, DEPENDS, MODEL, ACCESS, PROP);
r#"
((call [
	(attribute [(identifier) @_env (attribute (_) (identifier) @_env)] (identifier) @_ref)
	(attribute (identifier) @REQUEST (identifier) @_render)
] (argument_list . (string) @XML_ID))
(#eq? @_env "env")
(#eq? @_ref "ref")
(#eq? @REQUEST "request")
(#eq? @_render "render"))

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
	range: CharRange,
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
					entry.ancestors = ancestors
						.into_iter()
						.map(|sym| interner().get_or_intern(&sym).into())
						.collect();
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
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope.clone()) else {
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
		let mut this_model = None;
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
					if capture.index == PyCompletions::REQUEST {
						model_filter = Some("ir.ui.view");
					} else if capture.index == PyCompletions::XML_ID {
						let range = capture.node.byte_range();
						if range.contains(&offset) {
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let relative_offset = range.start;
							let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
							// remove the quotes
							let range = range.shrink(1).map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
							early_return = Some((
								EarlyReturn::XmlId(model_filter, current_module.into()),
								needle,
								range,
								rope.clone(),
							));
							break 'match_;
						}
					} else if capture.index == PyCompletions::MODEL {
						let range = capture.node.byte_range();
						// default true for is_inherit, to be checked later down that
						// it only belongs to (assignment)
						let (is_inherit, is_name) = match_
							.nodes_for_capture_index(PyCompletions::PROP)
							.next()
							.map(|prop| {
								(
									&contents[prop.byte_range()] == b"_inherit",
									&contents[prop.byte_range()] == b"_name",
								)
							})
							.unwrap_or((true, false));
						if is_inherit && range.contains_end(offset) {
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let relative_offset = range.start;
							let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
							let range = range.shrink(1).map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
							early_return = Some((EarlyReturn::Model, needle, range, rope.clone()));
							break 'match_;
						} else if range.end < offset
							&& (is_name
							// _inherit = '..' OR _inherit = ['..'] qualifies as primary model name
							|| (is_inherit && capture.node.parent()
								.map(|parent| parent.kind() == "assignment" || (parent.kind() == "list" && parent.named_child_count() == 1))
								.unwrap_or(false)))
						{
							this_model = Some(&contents[capture.node.byte_range().shrink(1)]);
						}
					} else if capture.index == PyCompletions::MAPPED {
						let range = capture.node.byte_range();
						if range.contains_end(offset) {
							let Some(Mapped {
								needle,
								model,
								single_field,
								range,
							}) = self.gather_mapped(
								root,
								match_,
								offset,
								range.clone(),
								this_model,
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
					} else if capture.index == PyCompletions::ACCESS {
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
							let range = range.map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
							early_return = Some((EarlyReturn::Access(model.to_string()), needle, range, rope.clone()));
							break 'match_;
						}
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
			Some((EarlyReturn::Mapped { model, single_field }, needle, range, rope)) => {
				// range:  foo.bar.baz
				// needle: foo.ba
				let mut needle = needle.as_ref();
				let mut model = some!(interner().get(&model));
				let mut range = range.map_unit(|unit| ByteOffset(rope.char_to_byte(unit.0)));
				if !single_field {
					some!(self
						.index
						.models
						.resolve_mapped(&mut model, &mut needle, Some(&mut range)));
				}
				let model_name = interner().resolve(&model);
				let range = range.map_unit(|unit| CharOffset(rope.byte_to_char(unit.0)));
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
	/// In the context of `Model.create({'foo.bar.baz': ..})`, `for_replacing` changes the
	/// meaning of `Mapped.needle` and `Mapped.range`:
	/// - `needle`: if replacing, refers to a prefix of 'foo.bar.baz', otherwise 'foo.bar.baz' itself.
	/// - `range`: if replacing, refers to the entire range of 'foo.bar.baz', otherwise the smallest substring of 'foo.bar.baz'
	///            which contains the cursor AND no periods.
	fn gather_mapped<'text>(
		&self,
		root: Node,
		match_: tree_sitter::QueryMatch,
		offset: usize,
		mut range: core::ops::Range<usize>,
		this_model: Option<&[u8]>,
		rope: &'text Rope,
		contents: &[u8],
		for_replacing: bool,
	) -> Option<Mapped<'text>> {
		let needle = if for_replacing {
			range = range.shrink(1);
			Cow::from(rope.get_byte_slice(range.start..offset)?)
		} else {
			let slice = Cow::from(rope.get_byte_slice(range.clone().shrink(1))?);
			let relative_offset = range.start + 1;
			let slice = &slice[offset - relative_offset..];
			// How many characters until the next period or end-of-string?
			let limit = slice.find('.').unwrap_or(slice.len());
			range = relative_offset..offset + limit;
			Cow::from(rope.get_byte_slice(range.clone())?)
		};

		let model;
		if let Some(local_model) = match_.nodes_for_capture_index(PyCompletions::MAPPED_TARGET).next() {
			let model_ = self.model_of_range(root, local_model.byte_range().map_unit(ByteOffset), None, &contents)?;
			model = interner().resolve(&model_).to_string();
		} else if let Some(this_model) = &this_model {
			model = String::from_utf8_lossy(this_model).to_string();
		} else {
			return None;
		}

		let mut single_field = false;
		if let Some(depends) = match_.nodes_for_capture_index(PyCompletions::DEPENDS).next() {
			single_field = matches!(
				&contents[depends.byte_range()],
				b"write" | b"create" | b"constrains" | b"onchange"
			);
		}

		Some(Mapped {
			needle,
			model,
			single_field,
			range: range.map_unit(|unit| CharOffset(rope.byte_to_char(unit))),
		})
	}
	pub async fn python_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, rope.clone())
		else {
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
			let mut this_model = None;
			'match_: for match_ in cursor.matches(query, root, &contents[..]) {
				for capture in match_.captures {
					if capture.index == PyCompletions::XML_ID {
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
					} else if capture.index == PyCompletions::MODEL {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::PROP)
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
						} else if range.end < offset &&
							// _inherit = '..' OR _inherit = ['..'] qualifies as primary model name
							(is_meta && capture.node.parent()
								.map(|parent| parent.kind() == "assignment" || (parent.kind() == "list" && parent.named_child_count() == 1))
								.unwrap_or(false))
						{
							this_model = Some(&contents[capture.node.byte_range().shrink(1)]);
						}
					} else if capture.index == PyCompletions::ACCESS {
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
					} else if capture.index == PyCompletions::MAPPED {
						let range = capture.node.byte_range();
						if range.contains_end(offset) {
							early_return = self
								.gather_mapped(root, match_, offset, range.clone(), this_model, &rope, &contents, false)
								.map(EarlyReturn::Mapped);
							break 'match_;
						}
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
				range,
			})) => {
				let mut needle = needle.as_ref();
				let mut model = interner().get_or_intern(&model);
				let mut range = range.map_unit(|unit| ByteOffset(rope.char_to_byte(unit.0)));
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
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope.clone()) else {
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
				if capture.index == PyCompletions::XML_ID {
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
				} else if capture.index == PyCompletions::MODEL {
					let range = capture.node.byte_range();
					let is_meta = match_
						.nodes_for_capture_index(PyCompletions::PROP)
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
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, rope.clone())
		else {
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
			let mut this_model = None;
			'match_: for match_ in cursor.matches(query, root, &contents[..]) {
				for capture in match_.captures {
					if capture.index == PyCompletions::MODEL {
						let range = capture.node.byte_range();
						let (is_inherit, is_name) = match_
							.nodes_for_capture_index(PyCompletions::PROP)
							.next()
							.map(|prop| {
								(
									&contents[prop.byte_range()] == b"_inherit",
									&contents[prop.byte_range()] == b"_name",
								)
							})
							.unwrap_or((true, false));
						if range.contains(&offset) {
							let range = range.shrink(1);
							let lsp_range = ts_range_to_lsp_range(capture.node.range());
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							return self.hover_model(&slice, Some(lsp_range), false);
						} else if range.end < offset
							&& (is_name
							// _inherit = '..' OR _inherit = ['..'] qualifies as primary model name
							|| (is_inherit && capture.node.parent()
								.map(|parent| parent.kind() == "assignment" || (parent.kind() == "list" && parent.named_child_count() == 1))
								.unwrap_or(false)))
						{
							this_model = Some(&contents[capture.node.byte_range().shrink(1)]);
						}
					} else if capture.index == PyCompletions::ACCESS {
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
					} else if capture.index == PyCompletions::MAPPED {
						let range = capture.node.byte_range();
						if range.contains(&offset) {
							early_return = self
								.gather_mapped(
									root,
									match_,
									offset,
									range.clone(),
									this_model,
									&rope,
									&contents[..],
									false,
								)
								.map(EarlyReturn::Mapped);
							break 'match_;
						}
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
				range,
			})) => {
				let mut needle = needle.as_ref();
				let mut model = interner().get_or_intern(&model);
				let mut range = range.map_unit(|unit| ByteOffset(rope.char_to_byte(unit.0)));
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
		self.hover_model(model, Some(lsp_range), true)
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
