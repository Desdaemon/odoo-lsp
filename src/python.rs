// use crate::partial::{emit_partial, PartialCompletions};
use crate::{Backend, Text};

use std::borrow::Cow;
use std::path::Path;
use std::sync::OnceLock;

use intmap::IntMap;
use lasso::Key;
use log::{debug, error};
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Parser, Query, QueryCursor, Tree};

use odoo_lsp::format_loc;
use odoo_lsp::model::{Field, FieldKind, ModelEntry, ModelLocation};
use odoo_lsp::utils::*;

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

fn model_fields() -> &'static Query {
	static QUERY: OnceLock<Query> = OnceLock::new();
	QUERY.get_or_init(|| {
		tree_sitter::Query::new(tree_sitter_python::language(), include_str!("queries/model_fields.scm")).unwrap()
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
						self.complete_xml_id(&needle, range, rope.clone(), None, &current_module, &mut items)
							.await?;
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
						self.complete_model(&needle, range, rope.clone(), &mut items).await?;
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
		let current_module = self
			.module_index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
			.map(|ref_| ref_.to_string());
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
						return self.record_references(&slice, current_module.as_deref());
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
						return self.model_references(&slice);
					}
				}
			}
		}
		Ok(None)
	}
	pub async fn populate_field_names<'model>(
		&self,
		entry: &'model mut ModelEntry,
	) -> miette::Result<&'model mut IntMap<Field>> {
		if entry.fields.is_some() {
			return Ok(entry.fields.as_mut().unwrap());
		}
		let mut out = IntMap::new();
		let locations = entry.base.iter().chain(entry.descendants.iter());
		let query = model_fields();
		let mut tasks = tokio::task::JoinSet::<miette::Result<Vec<_>>>::new();
		for ModelLocation(location, byte_range) in locations.cloned() {
			let interner = self.module_index.interner.clone();
			tasks.spawn(async move {
				let mut fields = vec![];
				let contents = tokio::fs::read(location.path.as_str()).await.into_diagnostic()?;
				let mut parser = Parser::new();
				parser.set_language(tree_sitter_python::language()).into_diagnostic()?;
				let ast = parser
					.parse(&contents, None)
					.ok_or_else(|| diagnostic!("AST not built"))?;
				let byte_range = byte_range.erase();
				let mut cursor = QueryCursor::new();
				cursor.set_byte_range(byte_range);
				for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
					let mut field = None;
					let mut type_ = None;
					for capture in match_.captures {
						if capture.index == 0 {
							// @field
							field = Some(capture.node);
						} else if capture.index == 1 {
							// @_Relational
							type_ = Some(capture.node.byte_range());
						} else if capture.index == 3 {
							// @relation
							if let (Some(field), Some(type_)) = (field.take(), type_.take()) {
								let range = ts_range_to_lsp_range(field.range());
								let field = String::from_utf8_lossy(&contents[field.byte_range()]);
								let field = interner.get_or_intern(&field);
								let relation = capture.node.byte_range().contract(1);
								let relation = String::from_utf8_lossy(&contents[relation]);
								let relation = interner.get_or_intern(&relation);
								let type_ = String::from_utf8_lossy(&contents[type_]);
								let type_ = interner.get_or_intern(&type_);
								fields.push((
									field,
									Field {
										kind: FieldKind::Relational(relation),
										type_,
										location: MinLoc {
											range,
											path: location.path.clone(),
										},
									},
								))
							}
						} else if capture.index == 5 {
							// @_Type
							if let Some(field) = field.take() {
								let range = ts_range_to_lsp_range(field.range());
								let field = String::from_utf8_lossy(&contents[field.byte_range()]);
								let field = interner.get_or_intern(&field);
								let type_ = capture.node.byte_range();
								let type_ = String::from_utf8_lossy(&contents[type_]);
								let type_ = interner.get_or_intern(&type_);
								fields.push((
									field,
									Field {
										kind: FieldKind::Value,
										type_,
										location: MinLoc {
											range,
											path: location.path.clone(),
										},
									},
								))
							}
						}
					}
				}
				Ok(fields)
			});
		}
		while let Some(fields) = tasks.join_next().await {
			let fields = match fields {
				Ok(Ok(ret)) => ret,
				Ok(Err(err)) => {
					debug!("{err}");
					continue;
				}
				Err(err) => {
					debug!("join error {err}");
					continue;
				}
			};
			// if let Some(partial_token) = partial_token.clone() {
			// 	let resp = fields.iter().map(|(key, _)| {
			// 		let label = self.module_index.interner.resolve(key);
			// 		CompletionItem {
			// 			label: label.to_string(),
			// 			kind: Some(CompletionItemKind::FIELD),
			// 			text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
			// 				new_text: label.to_string(),
			// 				insert: replace_range,
			// 				replace: replace_range,
			// 			})),
			// 			..Default::default()
			// 		}
			// 	});
			// 	emit_partial::<PartialCompletions, _>(
			// 		&self.client,
			// 		partial_token,
			// 		CompletionResponse::Array(resp.collect()),
			// 	)
			// 	.await;
			// }
			for (key, type_) in fields {
				out.insert_checked(key.into_usize() as u64, type_);
			}
		}
		Ok(entry.fields.insert(out))
	}
}
