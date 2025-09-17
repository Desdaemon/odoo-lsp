use std::borrow::Cow;
use std::sync::atomic::Ordering::Relaxed;

use tower_lsp_server::lsp_types::*;
use tree_sitter::{QueryCursor, Tree};

use crate::prelude::*;

use crate::backend::Backend;
use crate::backend::Text;
use crate::index::{_G, JsQuery};
use crate::model::PropertyKind;
use crate::utils::{ByteOffset, MaxVec, RangeExt, span_conv};
use tracing::instrument;
use ts_macros::query;

query! {
	#[lang = "tree_sitter_javascript"]
	OrmCallQuery(OrmObject, CallMethod, ModelArg, MethodArg);
	// Match this.orm.call('model', 'method')
	(call_expression
		function: (member_expression
			object: (member_expression
				object: (this)
				property: (property_identifier) @ORM_OBJECT (#eq? @ORM_OBJECT "orm"))
			property: (property_identifier) @CALL_METHOD (#eq? @CALL_METHOD "call"))
		arguments: (arguments
			. (string) @MODEL_ARG
			. ","
			. (string) @METHOD_ARG))
}

/// Javascript extensions.
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
		let file_path = uri.to_file_path().unwrap();
		let ast = self
			.ast_map
			.get(file_path.to_str().unwrap())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let ByteOffset(offset) = rope_conv(params.text_document_position_params.position, rope);
		let contents = Cow::from(rope);

		// try templates first
		let query = JsQuery::query();
		let mut cursor = QueryCursor::new();
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == JsQuery::TemplateName as u32 && range.contains(&offset) {
					let key = some!(_G(&contents[range.shrink(1)]));
					return Ok(some!(self.index.templates.get(&key.into()))
						.location
						.clone()
						.map(Into::into));
				}
			}
		}

		// try gotodefs for ORM calls
		let query = OrmCallQuery::query();
		let mut cursor = QueryCursor::new();
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			let mut model_arg_node = None;
			let mut method_arg_node = None;

			for capture in match_.captures {
				match OrmCallQuery::from(capture.index) {
					Some(OrmCallQuery::ModelArg) => {
						model_arg_node = Some(capture.node);
					}
					Some(OrmCallQuery::MethodArg) => {
						method_arg_node = Some(capture.node);
					}
					_ => {}
				}
			}

			if let Some(model_node) = model_arg_node
				&& let range = model_node.byte_range()
				&& range.contains_end(offset)
			{
				let range = range.shrink(1);
				let model = &contents[range];
				return self.index.jump_def_model(model);
			}

			if let Some(model_node) = model_arg_node
				&& let Some(method_node) = method_arg_node
				&& let range = method_node.byte_range()
				&& range.contains_end(offset)
			{
				let model = &contents[model_node.byte_range().shrink(1)];
				let method = &contents[range.shrink(1)];
				return self.index.jump_def_property_name(method, model);
			}
		}

		Ok(None)
	}
	pub fn js_references(&self, params: ReferenceParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Vec<Location>>> {
		let uri = &params.text_document_position.text_document.uri;
		let file_path = uri.to_file_path().unwrap();
		let ast = self
			.ast_map
			.get(file_path.to_str().unwrap())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let ByteOffset(offset) = rope_conv(params.text_document_position.position, rope);
		let contents = Cow::from(rope);
		let query = JsQuery::query();
		let mut cursor = QueryCursor::new();
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == JsQuery::TemplateName as u32 && range.contains(&offset) {
					let key = &contents[range.shrink(1)];
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
		let file_path = uri.to_file_path().unwrap();
		let ast = self
			.ast_map
			.get(file_path.to_str().unwrap())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let ByteOffset(offset) = rope_conv(params.text_document_position_params.position, rope);
		let contents = Cow::from(rope);
		let query = JsQuery::query();
		let mut cursor = QueryCursor::new();
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if capture.index == JsQuery::TemplateName as u32 && range.contains(&offset) {
					return Ok(self
						.index
						.hover_template(&contents[range.shrink(1)], Some(span_conv(capture.node.range()))));
				}
				if capture.index == JsQuery::Name as u32 && range.contains(&offset) {
					return Ok(self
						.index
						.hover_component(&contents[range], Some(span_conv(capture.node.range()))));
				}
			}
		}

		// try hover for ORM calls
		let query = OrmCallQuery::query();
		let mut cursor = QueryCursor::new();
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			let mut model_arg_node = None;
			let mut method_arg_node = None;

			for capture in match_.captures {
				match OrmCallQuery::from(capture.index) {
					Some(OrmCallQuery::ModelArg) => {
						model_arg_node = Some(capture.node);
					}
					Some(OrmCallQuery::MethodArg) => {
						method_arg_node = Some(capture.node);
					}
					_ => {}
				}
			}

			if let Some(model_node) = model_arg_node
				&& let range = model_node.byte_range()
				&& range.contains_end(offset)
			{
				let range = range.shrink(1);
				let model = &contents[range.clone()];
				return (self.index).hover_model(model, Some(rope_conv(range.map_unit(ByteOffset), rope)), false, None);
			}

			if let Some(model_node) = model_arg_node
				&& let Some(method_node) = method_arg_node
				&& let range = method_node.byte_range()
				&& range.contains_end(offset)
			{
				let range = range.shrink(1);
				let model = &contents[model_node.byte_range().shrink(1)];
				let method = &contents[range.clone()];
				return self.index.hover_property_name(
					method,
					model,
					Some(rope_conv(range.map_unit(ByteOffset), rope)),
				);
			}
		}

		Ok(None)
	}

	#[instrument(skip_all)]
	pub async fn js_completions(
		&self,
		params: CompletionParams,
		ast: Tree,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let ByteOffset(offset) = rope_conv(position, rope);
		let path = some!(params.text_document_position.text_document.uri.to_file_path());
		let completions_limit = self
			.workspaces
			.find_workspace_of(&path, |_, ws| ws.completions.limit)
			.unwrap_or_else(|| self.project_config.completions_limit.load(Relaxed));

		let contents = Cow::from(rope);
		let query = OrmCallQuery::query();
		let mut cursor = QueryCursor::new();

		// Find the orm.call node that contains the cursor position
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			let mut model_arg_node = None;
			let mut method_arg_node = None;

			for capture in match_.captures {
				match OrmCallQuery::from(capture.index) {
					Some(OrmCallQuery::ModelArg) => {
						model_arg_node = Some(capture.node);
					}
					Some(OrmCallQuery::MethodArg) => {
						method_arg_node = Some(capture.node);
					}
					_ => {}
				}
			}

			// Check if cursor is within the model argument
			if let Some(model_node) = model_arg_node {
				let range = model_node.byte_range();
				if range.contains(&offset) {
					// Extract the current prefix (excluding quotes)
					let inner_range = range.shrink(1);
					let prefix = &contents[inner_range.start..offset];
					let lsp_range = rope_conv(inner_range.map_unit(ByteOffset), rope);
					let mut items = MaxVec::new(completions_limit);
					self.index.complete_model(prefix, lsp_range, &mut items)?;

					return Ok(Some(CompletionResponse::List(CompletionList {
						is_incomplete: !items.has_space(),
						items: items.into_inner(),
					})));
				}
			}

			// Check if cursor is within the method argument
			if let Some(method_node) = method_arg_node {
				let range = method_node.byte_range();
				if range.contains(&offset) {
					// Extract the model name from the first argument
					if let Some(model_node) = model_arg_node {
						let model_range = model_node.byte_range().shrink(1);
						let model_name = &contents[model_range];

						// Extract the current method prefix (excluding quotes)
						let inner_range = range.clone().shrink(1);
						let prefix = &contents[inner_range.start..offset];

						let byte_range = inner_range.map_unit(ByteOffset);

						let mut items = MaxVec::new(completions_limit);
						self.index.complete_property_name(
							prefix,
							byte_range,
							model_name.into(),
							rope,
							Some(PropertyKind::Method),
							true,
							true,
							&mut items,
						)?;

						return Ok(Some(CompletionResponse::List(CompletionList {
							is_incomplete: !items.has_space(),
							items: items.into_inner(),
						})));
					}
				}
			}

			// Check if cursor is in a position where we should start a new string argument
			// This handles cases where the user is typing after the comma but hasn't started the string yet
			if let Some(model_node) = model_arg_node {
				let contents_bytes = contents.as_bytes();
				let model_end = model_node.byte_range().end;
				// Look for comma after model argument
				let mut i = model_end;
				while i < contents_bytes.len() && contents_bytes[i].is_ascii_whitespace() {
					i += 1;
				}
				if i < contents_bytes.len() && contents_bytes[i] == b',' {
					i += 1;
					// Skip whitespace after comma
					while i < contents_bytes.len() && contents_bytes[i].is_ascii_whitespace() {
						i += 1;
					}
					// If cursor is at or after this position and before any method argument
					if offset >= i
						&& (method_arg_node.is_none() || offset < method_arg_node.unwrap().byte_range().start)
					{
						// We're completing the method name
						let model_range = model_node.byte_range().shrink(1);
						let model_name = &contents[model_range];

						let synthetic_range = ByteOffset(i)..ByteOffset(offset.max(i));

						let mut items = MaxVec::new(100);
						self.index.complete_property_name(
							"",
							synthetic_range,
							model_name.into(),
							rope,
							Some(PropertyKind::Method),
							true,
							true,
							&mut items,
						)?;

						return Ok(Some(CompletionResponse::List(CompletionList {
							is_incomplete: false,
							items: items.into_inner(),
						})));
					}
				}
			}
		}

		Ok(None)
	}
}
