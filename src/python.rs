use crate::{Backend, Text};

use std::borrow::Cow;
use std::path::Path;
use std::sync::OnceLock;

use lasso::Key;
use log::{debug, error};
use miette::{diagnostic, Context, IntoDiagnostic};
use odoo_lsp::index::SymbolMap;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Parser, Query, QueryCursor, Tree};

use odoo_lsp::format_loc;
use odoo_lsp::model::{Field, FieldKind, FieldName, ModelEntry, ModelLocation};
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

/// `node` must be `[(string) (concatenated_string)]`
fn parse_help<'text>(node: &Node, contents: &'text [u8]) -> Cow<'text, str> {
	let mut cursor = node.walk();
	match node.kind() {
		"string" => {
			let content = node
				.children(&mut cursor)
				.find_map(|child| (child.kind() == "string_content").then(|| &contents[child.byte_range()]));
			String::from_utf8_lossy(content.unwrap_or(&[]))
		}
		"concatenated_string" => {
			let mut content = vec![];
			for string in node.children(&mut cursor) {
				if string.kind() == "string" {
					let mut cursor = string.walk();
					let children = string.children(&mut cursor).find_map(|child| {
						(child.kind() == "string_content").then(|| {
							String::from_utf8_lossy(&contents[child.byte_range()])
								.trim()
								.replace("\\n", "\n")
								.replace("\\t", "\t")
						})
					});
					content.extend(children);
				}
			}
			Cow::from(content.join(" "))
		}
		_ => unreachable!(),
	}
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
			.index
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
			let mut model_filter = None;
			for capture in match_.captures {
				if capture.index == 2 {
					// @_request
					model_filter = Some("ir.ui.view");
				} else if capture.index == 4 {
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
						self.complete_xml_id(&needle, range, rope.clone(), model_filter, *current_module, &mut items)
							.await?;
						return Ok(Some(CompletionResponse::List(CompletionList {
							is_incomplete: items.len() >= Self::LIMIT,
							items,
						})));
					}
				} else if capture.index == 5 {
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
				if capture.index == 4 {
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
				} else if capture.index == 5 {
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
			.index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()));
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
	) -> miette::Result<&'model mut SymbolMap<FieldName, Field>> {
		if entry.fields.is_some() {
			return Ok(entry.fields.as_mut().unwrap());
		}
		let mut out = SymbolMap::default();
		let locations = entry.base.iter().chain(entry.descendants.iter());
		let query = model_fields();
		let mut tasks = tokio::task::JoinSet::<miette::Result<Vec<_>>>::new();
		for ModelLocation(location, byte_range) in locations.cloned() {
			let interner = self.index.interner.clone();
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
					let mut is_relational = false;
					let mut relation = None;
					let mut kwarg = None::<Kwargs>;
					let mut help = None;
					enum Kwargs {
						ComodelName,
						Help,
					}
					for capture in match_.captures {
						if capture.index == 0 {
							// @field
							field = Some(capture.node);
						} else if capture.index == 1 {
							// @Type
							type_ = Some(capture.node.byte_range());
							is_relational = matches!(
								&contents[capture.node.byte_range()],
								b"One2many" | b"Many2one" | b"Many2many"
							);
						} else if capture.index == 3 {
							// @relation
							if is_relational {
								relation = Some(capture.node.byte_range().contract(1));
							}
						} else if capture.index == 4 {
							// @_arg
							match &contents[capture.node.byte_range()] {
								b"comodel_name" if is_relational => kwarg = Some(Kwargs::ComodelName),
								b"help" => kwarg = Some(Kwargs::Help),
								_ => kwarg = None,
							}
						} else if capture.index == 5 {
							// @value
							match kwarg {
								Some(Kwargs::ComodelName) => {
									if capture.node.kind() == "string" {
										relation = Some(capture.node.byte_range().contract(1));
									}
								}
								Some(Kwargs::Help) => {
									if matches!(capture.node.kind(), "string" | "concatenated_string") {
										help = Some(parse_help(&capture.node, &contents));
									}
								}
								None => {}
							}
						}
					}
					if let (Some(field), Some(type_)) = (field, type_) {
						let range = ts_range_to_lsp_range(field.range());
						let field = String::from_utf8_lossy(&contents[field.byte_range()]);
						let field = interner.get_or_intern(&field);
						let kind = if let Some(relation) = relation {
							let relation = String::from_utf8_lossy(&contents[relation]);
							let relation = interner.get_or_intern(&relation);
							FieldKind::Relational(relation)
						} else if is_relational {
							debug!("is_relational but no relation found");
							continue;
						} else {
							FieldKind::Value
						};
						let type_ = String::from_utf8_lossy(&contents[type_]);
						let type_ = interner.get_or_intern(&type_);
						let location = MinLoc {
							path: location.path.clone(),
							range,
						};
						let help = help.map(|help| odoo_lsp::str::Text::try_from(help.as_ref()).unwrap());
						fields.push((
							field,
							Field {
								kind,
								type_,
								location,
								help,
							},
						))
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
			for (key, type_) in fields {
				out.insert_checked(key.into_usize() as u64, type_);
			}
		}
		Ok(entry.fields.insert(out))
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use pretty_assertions::assert_eq;

	/// Tricky behavior here. The query syntax required to match the trailing comma between
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
		let query = model_fields();
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
}
