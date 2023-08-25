use crate::{Backend, Text};

use std::borrow::Cow;
use std::path::Path;
use std::sync::OnceLock;

use log::{debug, error};
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Parser, Query, Tree};

use odoo_lsp::{format_loc, utils::*};

fn py_completions() -> &'static Query {
	static QUERY: OnceLock<Query> = OnceLock::new();
	QUERY.get_or_init(|| {
		tree_sitter::Query::new(
			tree_sitter_python::language(),
			include_str!("queries/py_completions.scm"),
		)
		.unwrap()
	})
}

fn py_references() -> &'static Query {
	static QUERY: OnceLock<Query> = OnceLock::new();
	QUERY.get_or_init(|| {
		tree_sitter::Query::new(
			tree_sitter_python::language(),
			include_str!("queries/py_references.scm"),
		)
		.unwrap()
	})
}

impl Backend {
	pub async fn on_change_python(
		&self,
		text: &Text,
		uri: &Url,
		rope: Rope,
		old_rope: Option<Rope>,
	) -> miette::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(tree_sitter_python::language())
			.into_diagnostic()
			.with_context(|| "failed to init python parser")?;
		self.update_ast(text, uri, rope, old_rope, parser)?;
		Ok(())
	}
	const BYTE_WINDOW: usize = 200;
	pub fn python_completions(
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
			.module_index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
		else {
			debug!(format_loc!("python_completions: no current_module"));
			return Ok(None);
		};
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		let bytes = rope.bytes().collect::<Vec<_>>();
		// TODO: Very inexact, is there a better way?
		let range = offset.saturating_sub(Self::BYTE_WINDOW)..bytes.len().min(offset + Self::BYTE_WINDOW);
		let query = py_completions();
		cursor.set_byte_range(range.clone());
		let mut items = vec![];
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			for capture in match_.captures {
				if capture.index == 2 {
					// @xml_id
					let range = capture.node.byte_range();
					if range.contains(&offset) {
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let relative_offset = range.start;
						let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
						// remove the quotes
						let range = range.contract(1).map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
						self.complete_inherit_id(&needle, range, rope.clone(), None, &current_module, &mut items)?;
						return Ok(Some(CompletionResponse::List(CompletionList {
							is_incomplete: items.len() >= Self::LIMIT,
							items,
						})));
					}
				} else if capture.index == 3 {
					// @model
					let range = capture.node.byte_range();
					if range.contains(&offset) {
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let relative_offset = range.start;
						let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
						let range = range.contract(1).map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
						self.complete_model(&needle, range, rope.clone(), &mut items)?;
						return Ok(Some(CompletionResponse::List(CompletionList {
							is_incomplete: items.len() >= Self::LIMIT,
							items,
						})));
					}
				}
			}
		}
		Ok(None)
	}
	pub fn python_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, rope.clone())
		else {
			Err(diagnostic!("could not find offset for {}", uri.path()))?
		};
		let query = py_completions();
		let bytes = rope.bytes().collect::<Vec<_>>();
		let range = offset.saturating_sub(Self::BYTE_WINDOW)..bytes.len().min(offset + Self::BYTE_WINDOW);
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		cursor.set_byte_range(range);
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			for capture in match_.captures {
				if capture.index == 2 {
					// @xml_id
					let range = capture.node.byte_range();
					if range.contains(&offset) {
						let range = range.contract(1);
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let slice = Cow::from(slice);
						return self
							.jump_def_inherit_id(&slice, &params.text_document_position_params.text_document.uri);
					}
				} else if capture.index == 3 {
					// @model
					let range = capture.node.byte_range();
					if range.contains(&offset) {
						let range = range.contract(1);
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let slice = Cow::from(slice);
						return self.jump_def_model(&slice);
					}
				}
			}
		}
		Ok(None)
	}
	pub fn python_references(
		&self,
		params: ReferenceParams,
		rope: Rope,
		ast: Tree,
	) -> miette::Result<Option<Vec<Location>>> {
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope.clone()) else {
			return Ok(None);
		};
		let query = py_references();
		let bytes = rope.bytes().collect::<Vec<_>>();
		let range = offset.saturating_sub(Self::BYTE_WINDOW)..bytes.len().min(offset + Self::BYTE_WINDOW);
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		cursor.set_byte_range(range);
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			for model in match_.nodes_for_capture_index(1) {
				let range = model.byte_range();
				if range.contains(&offset) {
					let range = range.contract(1);
					let Some(slice) = rope.get_byte_slice(range.clone()) else {
						dbg!(&range);
						break 'match_;
					};
					let slice = Cow::from(slice);
					return self.model_references(&slice);
				}
			}
		}
		Ok(None)
	}
}
