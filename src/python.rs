use crate::{Backend, Text};

use std::borrow::Cow;
use std::path::Path;
use std::sync::OnceLock;

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
	pub async fn on_change_python(&self, text: &Text, uri: &Url, rope: Rope) -> miette::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(tree_sitter_python::language())
			.into_diagnostic()
			.with_context(|| "failed to init python parser")?;
		self.update_ast(text, uri, rope, parser)?;
		Ok(())
	}
	pub fn python_completions(
		&self,
		params: CompletionParams,
		ast: Tree,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope.clone()) else {
			eprintln!(format_loc!("python_completions: invalid offset"));
			return Ok(None);
		};
		let Some(current_module) = self
			.module_index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
		else {
			eprintln!(format_loc!("python_completions: no current_module"));
			return Ok(None);
		};
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		let bytes = rope.bytes().collect::<Vec<_>>();
		// TODO: Very inexact, is there a better way?
		let range = offset.saturating_sub(50)..bytes.len().min(offset + 200);
		let query = py_completions();
		cursor.set_byte_range(range.clone());
		let mut items = vec![];
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			match match_.captures {
				[_, _, xml_id] => {
					let range = xml_id.node.byte_range();
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
				}
				[_, model] => {
					let range = model.node.byte_range();
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
				unk => Err(diagnostic!("Unknown pattern {unk:?}"))?,
			}
		}
		Ok(None)
	}
	pub fn python_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let Some(ast) = self
			.ast_map
			.get(params.text_document_position_params.text_document.uri.path())
		else {
			return Ok(None);
		};
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, rope.clone())
		else {
			return Ok(None);
		};
		let query = py_completions();
		let bytes = rope.bytes().collect::<Vec<_>>();
		let range = offset.saturating_sub(50)..bytes.len().min(offset + 200);
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		cursor.set_byte_range(range.clone());
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			match match_.captures {
				[_, _, xml_id] => {
					let range = xml_id.node.byte_range();
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
				}
				[_, model] => {
					let range = model.node.byte_range();
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
				unk => Err(diagnostic!("Unknown pattern {unk:?}"))?,
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
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope.clone())
		else {
			return Ok(None);
		};
		let query = py_references();
		let bytes = rope.bytes().collect::<Vec<_>>();
		let range = offset.saturating_sub(50)..bytes.len().min(offset + 200);
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		cursor.set_byte_range(range.clone());
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			match match_.captures {
				[_, model] => {
					let range = model.node.byte_range();
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
				unk => Err(diagnostic!("Unknown pattern {unk:?}"))?,
			}
		}
		Ok(None)
	}
}
