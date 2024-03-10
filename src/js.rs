use std::borrow::Cow;

use crate::backend::Backend;
use crate::Text;

use fomat_macros::fomat;
use miette::diagnostic;
use odoo_lsp::index::{interner, ComponentQuery};
use odoo_lsp::some;
use odoo_lsp::utils::{position_to_offset, ts_range_to_lsp_range, ByteOffset, RangeExt};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Parser, QueryCursor};

impl Backend {
	pub fn on_change_js(&self, text: &Text, uri: &Url, rope: Rope, old_rope: Option<Rope>) -> miette::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(tree_sitter_javascript::language())
			.expect("bug: failed to init js parser");
		self.update_ast(text, uri, rope, old_rope, parser)
	}
	pub fn js_jump_def(&self, params: GotoDefinitionParams, rope: &Rope) -> miette::Result<Option<Location>> {
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
		let query = ComponentQuery::query();
		let mut cursor = QueryCursor::new();
		for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == ComponentQuery::TemplateName as u32 && range.contains(&offset) {
					let key = some!(interner().get(&String::from_utf8_lossy(&contents[range.shrink(1)])));
					return Ok(some!(self.index.templates.get(&key.into()))
						.location
						.clone()
						.map(Into::into));
				}
			}
		}

		Ok(None)
	}
	pub fn js_references(&self, params: ReferenceParams, rope: &Rope) -> miette::Result<Option<Vec<Location>>> {
		let uri = &params.text_document_position.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope) else {
			Err(diagnostic!("could not find offset for {}", uri.path()))?
		};
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let query = ComponentQuery::query();
		let mut cursor = QueryCursor::new();
		for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == ComponentQuery::TemplateName as u32 && range.contains(&offset) {
					let key = String::from_utf8_lossy(&contents[range.shrink(1)]);
					let key = some!(interner().get(&key));
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
	pub fn js_hover(&self, params: HoverParams, rope: Rope) -> miette::Result<Option<Hover>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build AST for {}", uri.path()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, &rope) else {
			Err(diagnostic!("could not find offset for {}", uri.path()))?
		};
		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let query = ComponentQuery::query();
		let mut cursor = QueryCursor::new();
		for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == ComponentQuery::TemplateName as u32 && range.contains(&offset) {
					let name = String::from_utf8_lossy(&contents[range.shrink(1)]);
					let key = some!(interner().get(&name));
					let template = some!(self.index.templates.get(&key.into()));
					let value = fomat!(
						"```xml\n"
						"<t t-name=\"" (name) "\"/>\n"
						"```"
						if let Some(loc) = template.location.as_ref() { "\n*Defined at:* " (loc) }
					);
					return Ok(Some(Hover {
						contents: HoverContents::Scalar(MarkedString::String(value)),
						range: Some(ts_range_to_lsp_range(capture.node.range())),
					}));
				}
				if capture.index == ComponentQuery::Name as u32 && range.contains(&offset) {
					let name = String::from_utf8_lossy(&contents[range]);
					let key = some!(interner().get(&name));
					let component = some!(self.index.components.get(&key.into()));
					let value = fomat!(
						"```js\n"
						"(component) class " (name) ";\n"
						"```"
						if let Some(loc) = component.location.as_ref() { "\n*Defined at:* " (loc) }
					);
					return Ok(Some(Hover {
						contents: HoverContents::Scalar(MarkedString::String(value)),
						range: Some(ts_range_to_lsp_range(capture.node.range())),
					}));
				}
			}
		}

		Ok(None)
	}
}
