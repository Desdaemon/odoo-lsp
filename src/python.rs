use crate::{Backend, Text};

use std::borrow::Cow;
use std::future::ready;
use std::path::Path;

use dashmap::mapref::one::RefMut;
use lasso::{Key, Spur};
use log::{debug, error, warn};
use miette::{diagnostic, IntoDiagnostic};
use odoo_lsp::index::{index_models, interner, Module, Symbol, SymbolMap};
use odoo_lsp::utils::future::FutureOr;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Parser, QueryCursor, Tree};

use odoo_lsp::model::{Field, FieldKind, ModelEntry, ModelLocation, ModelName, ModelType};
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
(#match? @DEPENDS "^(depend|constrain)s$"))

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

(attribute (_) (identifier) @ACCESS)"#
}

query! {
	ModelFields(FIELD, TYPE, RELATION, ARG, VALUE);
r#"
((class_definition
	(block
		(expression_statement
			(assignment
				(identifier) @FIELD
				(call [
					(identifier) @TYPE
					(attribute (identifier) @_fields (identifier) @TYPE)
				] (argument_list . (string)? @RELATION
					((keyword_argument (identifier) @ARG (_) @VALUE) ","?)*))))))
(#eq? @_fields "fields")
(#match? @TYPE "^[A-Z]"))"#
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
								.replace("\\n", "  \n")
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

/// (module (_)*)
fn top_level_stmt(module: Node, offset: usize) -> Option<Node> {
	module
		.named_children(&mut module.walk())
		.find(|child| child.byte_range().contains_end(offset))
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
			.expect("bug: failed to init python parser");
		self.update_ast(text, uri, rope.clone(), old_rope, parser)?;
		Ok(())
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
					if let Some(fut) = self.populate_field_names(model_key.into(), &[path]) {
						fut.await;
					}
				}
				ModelType::Inherit(inherits) => {
					let Some(model) = inherits.first() else { continue };
					let model_key = interner().get(model).unwrap();
					if let Some(fut) = self.populate_field_names(model_key.into(), &[path]) {
						fut.await;
					}
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
		let contents = rope.bytes().collect::<Vec<_>>();
		let query = PyCompletions::query();
		let mut items = MaxVec::new(Self::LIMIT);
		let mut early_return = None;
		let mut this_model = None;
		// FIXME: This hack is necessary to drop !Send locals before await points.
		enum EarlyReturn<'a> {
			XmlId(Option<&'a str>, Symbol<Module>),
			Model,
			Access(String),
			Mapped { model: String, constrains: bool },
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
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let relative_offset = range.start;
							let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));

							let model;
							let mut constrains = false;
							if let Some(local_model) =
								match_.nodes_for_capture_index(PyCompletions::MAPPED_TARGET).next()
							{
								let model_ = some!(self.model_of_range(
									root,
									local_model.byte_range().map_unit(ByteOffset),
									None,
									&contents
								));
								model = interner().resolve(&model_).to_string();
							} else if let Some(this_model) = &this_model {
								model = String::from_utf8_lossy(this_model).to_string();
								if let Some(depends) = match_.nodes_for_capture_index(PyCompletions::DEPENDS).next() {
									constrains = b"constrains" == &contents[depends.byte_range()];
								}
							} else {
								break 'match_;
							}

							early_return = Some((
								EarlyReturn::Mapped { model, constrains },
								needle,
								range.shrink(1).map_unit(|offset| CharOffset(rope.byte_to_char(offset))),
								rope.clone(),
							));
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
			Some((EarlyReturn::Mapped { model, constrains }, needle, mut range, rope)) => {
				// range:  foo.bar.baz
				// needle: foo.ba
				let mut needle = needle.as_ref();
				let mut model = some!(interner().get(&model));
				if !constrains {
					while let Some((lhs, rhs)) = needle.split_once('.') {
						debug!("mapped {}", needle);
						// lhs: foo
						// rhs: ba
						needle = rhs;
						let fields = some!(self.populate_field_names(model.into(), &[])).await;
						let field = some!(interner().get(&lhs));
						let field = some!(fields.fields.as_ref()).get(&field.into());
						let FieldKind::Relational(rel) = some!(field).kind else {
							return Ok(None);
						};
						model = rel;
						// old range: foo.bar.baz
						// range:         bar.baz
						let start = rope.char_to_byte(range.start.0) + lhs.len() + 1;
						range = CharOffset(rope.byte_to_char(start))..range.end;
					}
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
		let contents = rope.bytes().collect::<Vec<_>>();
		let mut early_return = None;
		{
			let root = some!(top_level_stmt(ast.root_node(), offset));
			let query = PyCompletions::query();
			let mut cursor = tree_sitter::QueryCursor::new();
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
						}
					} else if capture.index == PyCompletions::ACCESS {
						let range = capture.node.byte_range();
						if range.contains_end(offset) {
							let lhs = some!(capture.node.prev_named_sibling());
							let lhs = lhs.byte_range().map_unit(ByteOffset);
							let model = some!(self.model_of_range(root, lhs, None, &contents));
							let field = String::from_utf8_lossy(&contents[range]);
							let model = interner().resolve(&model);
							early_return = Some((field, model));
							break 'match_;
						}
					}
				}
			}
		}
		if let Some((field, model)) = early_return {
			return self.jump_def_field_name(&field, model).await;
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
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let contents = rope.bytes().collect::<Vec<_>>();
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
	pub fn populate_field_names<'model>(
		&'model self,
		model: ModelName,
		locations_filter: &'model [Spur],
	) -> Option<FutureOr<RefMut<'model, ModelName, ModelEntry>>> {
		let model_name = interner().resolve(&model);
		let entry = self.index.models.try_get_mut(&model).expect(format_loc!("deadlock"))?;
		if entry.fields.is_some() && locations_filter.is_empty() {
			return Some(FutureOr::Ready(ready(entry)));
		}
		let t0 = std::time::Instant::now();
		let mut out = SymbolMap::default();
		let locations = entry.base.iter().chain(&entry.descendants).cloned().collect::<Vec<_>>();

		let query = ModelFields::query();
		let mut tasks = tokio::task::JoinSet::<miette::Result<Vec<_>>>::new();
		for ModelLocation(location, byte_range) in locations {
			if !locations_filter.is_empty() && !locations_filter.contains(&location.path) {
				continue;
			}
			tasks.spawn(async move {
				let mut fields = vec![];
				let contents = tokio::fs::read(interner().resolve(&location.path))
					.await
					.into_diagnostic()?;
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
						if capture.index == ModelFields::FIELD {
							field = Some(capture.node);
						} else if capture.index == ModelFields::TYPE {
							type_ = Some(capture.node.byte_range());
							// TODO: fields.Reference
							is_relational = matches!(
								&contents[capture.node.byte_range()],
								b"One2many" | b"Many2one" | b"Many2many"
							);
						} else if capture.index == ModelFields::RELATION {
							if is_relational {
								relation = Some(capture.node.byte_range().shrink(1));
							}
						} else if capture.index == ModelFields::ARG {
							match &contents[capture.node.byte_range()] {
								b"comodel_name" if is_relational => kwarg = Some(Kwargs::ComodelName),
								b"help" => kwarg = Some(Kwargs::Help),
								_ => kwarg = None,
							}
						} else if capture.index == ModelFields::VALUE {
							match kwarg {
								Some(Kwargs::ComodelName) => {
									if capture.node.kind() == "string" {
										relation = Some(capture.node.byte_range().shrink(1));
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
						let field_str = String::from_utf8_lossy(&contents[field.byte_range()]);
						let field = interner().get_or_intern(&field_str);
						let type_ = String::from_utf8_lossy(&contents[type_]);
						let kind = if let Some(relation) = relation {
							let relation = String::from_utf8_lossy(&contents[relation]);
							let relation = interner().get_or_intern(&relation);
							FieldKind::Relational(relation)
						} else if is_relational {
							debug!("is_relational but no relation found: field={field_str} type={type_}");
							continue;
						} else {
							FieldKind::Value
						};
						let type_ = interner().get_or_intern(&type_);
						let location = MinLoc {
							path: location.path,
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
		Some(FutureOr::Pending(Box::pin(async move {
			let ancestors = entry.ancestors.iter().cloned().collect::<Vec<_>>();

			// drop to prevent deadlock
			drop(entry);

			// recursively get or populate ancestors' fields
			for ancestor in ancestors {
				if let Some(entry) = self.populate_field_names(ancestor, locations_filter) {
					let entry = entry.await;
					if let Some(fields) = entry.fields.as_ref() {
						// TODO: Implement copy-on-write to increase reuse
						out.extend(fields.iter().map(|(key, val)| (key.clone(), val.clone())));
					}
				}
			}

			while let Some(fields) = tasks.join_next().await {
				let fields = match fields {
					Ok(Ok(ret)) => ret,
					Ok(Err(err)) => {
						warn!("{err}");
						continue;
					}
					Err(err) => {
						warn!("join error {err}");
						continue;
					}
				};
				for (key, type_) in fields {
					out.insert_checked(key.into_usize() as u64, type_);
				}
			}

			log::info!(
				"[{}] Populated {} fields in {}ms",
				model_name,
				out.len(),
				t0.elapsed().as_millis()
			);

			let mut entry = self
				.index
				.models
				.try_get_mut(&model)
				.expect(format_loc!("deadlock"))
				.expect(format_loc!("no entry"));
			entry.fields = Some(out);
			entry
		})))
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

		let bytes = rope.bytes().collect::<Vec<_>>();
		let mut early_return = None;
		{
			let root = some!(top_level_stmt(ast.root_node(), offset));
			let query = PyCompletions::query();
			let mut cursor = tree_sitter::QueryCursor::new();
			'match_: for match_ in cursor.matches(query, root, &bytes[..]) {
				for capture in match_.captures {
					if capture.index == PyCompletions::MODEL {
						let range = capture.node.byte_range();
						if range.contains(&offset) {
							let range = range.shrink(1);
							let lsp_range = ts_range_to_lsp_range(capture.node.range());
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							return self.hover_model(&slice, Some(lsp_range), false);
						}
					} else if capture.index == PyCompletions::ACCESS {
						let range = capture.node.byte_range();
						if range.contains(&offset) {
							let lsp_range = ts_range_to_lsp_range(capture.node.range());
							let lhs = some!(capture.node.prev_named_sibling());
							let lhs = lhs.byte_range().map_unit(ByteOffset);
							let model = some!(self.model_of_range(root, lhs, None, &bytes));
							let field = String::from_utf8_lossy(&bytes[range]);
							let model = interner().resolve(&model);
							early_return = Some((field, model, lsp_range));
							break 'match_;
						}
					}
				}
			}
		}
		if let Some((field, model, lsp_range)) = early_return {
			return self.hover_field_name(&field, model, Some(lsp_range)).await;
		}

		// No matches, assume arbitrary expression.
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let lsp_range = ts_range_to_lsp_range(needle.range());
		let model = some!(self.model_of_range(root, needle.byte_range().map_unit(ByteOffset), None, &bytes));
		let model = interner().resolve(&model);
		self.hover_model(model, Some(lsp_range), true)
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use pretty_assertions::assert_eq;

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
