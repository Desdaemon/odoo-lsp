use crate::{Backend, Text};

use std::borrow::Cow;
use std::path::Path;
use std::sync::OnceLock;

use miette::{Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Parser, Query, Tree};

use odoo_lsp::{format_loc, utils::*};

fn env_ref_query() -> &'static Query {
	static ENV_REF: OnceLock<Query> = OnceLock::new();
	ENV_REF.get_or_init(|| {
		tree_sitter::Query::new(tree_sitter_python::language(), include_str!("queries/env_ref.scm")).unwrap()
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
	pub async fn python_completions(
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
		let query = env_ref_query();
		cursor.set_byte_range(range.clone());
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			for xml_id in match_.nodes_for_capture_index(2) {
				if xml_id.byte_range().contains(&offset) {
					let Some(slice) = rope.get_byte_slice(xml_id.byte_range()) else {
						dbg!((xml_id.byte_range(), &range));
						break 'match_;
					};
					let relative_offset = xml_id.byte_range().start;
					// remove the quotes
					let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
					let range = xml_id.range().start_byte + 1..xml_id.range().end_byte - 1;
					let range = range.map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
					let mut items = vec![];
					self.complete_inherit_id(
						dbg!(&needle),
						dbg!(range),
						rope.clone(),
						None,
						&current_module,
						&mut items,
					)?;
					return Ok(Some(CompletionResponse::List(CompletionList {
						is_incomplete: items.len() >= Self::LIMIT,
						items,
					})));
				}
			}
		}
		Ok(None)
	}
}
