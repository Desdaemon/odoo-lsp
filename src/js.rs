use std::borrow::Cow;

use tower_lsp_server::lsp_types::*;

use crate::prelude::*;

use crate::backend::Backend;
use crate::backend::Text;
use crate::index::JsQuery;

impl Backend {
	pub fn on_change_js(
		&self,
		text: &Text,
		uri: &Uri,
		rope: RopeSlice<'_>,
		old_rope: Option<Rope>,
	) -> anyhow::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(&tree_sitter_javascript::LANGUAGE.into())
			.expect("bug: failed to init js parser");
		self.update_ast(text, uri, rope, old_rope, parser)
	}
	pub fn js_jump_def(&self, params: GotoDefinitionParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let Ok(ByteOffset(offset)) = rope_conv(params.text_document_position_params.position, rope) else {
			Err(errloc!("could not find offset for {}", uri.path().as_str()))?
		};
		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let query = JsQuery::query();
		let mut cursor = QueryCursor::new();
		for match_ in cursor.matches(query, ast.root_node(), contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == JsQuery::TemplateName as u32 && range.contains(&offset) {
					let key = some!(_G(String::from_utf8_lossy(&contents[range.shrink(1)])));
					return Ok(some!(self.index.templates.get(&key.into()))
						.location
						.clone()
						.map(Into::into));
				}
			}
		}

		Ok(None)
	}
	pub fn js_references(&self, params: ReferenceParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Vec<Location>>> {
		let uri = &params.text_document_position.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let Ok(ByteOffset(offset)) = rope_conv(params.text_document_position.position, rope) else {
			Err(errloc!("could not find offset for {}", uri.path().as_str()))?
		};
		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let query = JsQuery::query();
		let mut cursor = QueryCursor::new();
		for match_ in cursor.matches(query, ast.root_node(), contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == JsQuery::TemplateName as u32 && range.contains(&offset) {
					let key = String::from_utf8_lossy(&contents[range.shrink(1)]);
					let key = some!(_G(key));
					let template = some!(self.index.templates.get(&key.into()));
					return Ok(Some(
						template
							.descendants
							.iter()
							.flat_map(|tpl| tpl.location.clone().map(Into::into))
							.collect(),
					));
				}
			}
		}

		Ok(None)
	}
	pub fn js_hover(&self, params: HoverParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Hover>> {
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
		let query = JsQuery::query();
		let mut cursor = QueryCursor::new();
		for match_ in cursor.matches(query, ast.root_node(), contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == JsQuery::TemplateName as u32 && range.contains(&offset) {
					return Ok(self.index.hover_template(
						&String::from_utf8_lossy(&contents[range.shrink(1)]),
						Some(span_conv(capture.node.range())),
					));
				}
				if capture.index == JsQuery::Name as u32 && range.contains(&offset) {
					return Ok(self.index.hover_component(
						&String::from_utf8_lossy(&contents[range]),
						Some(span_conv(capture.node.range())),
					));
				}
			}
		}

		Ok(None)
	}
}
