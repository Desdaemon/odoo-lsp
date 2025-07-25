use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::Ordering::Relaxed;

use fomat_macros::fomat;
use lasso::Spur;
use tower_lsp_server::{UriExt, lsp_types::*};
use tracing::{debug, instrument, warn};
use tree_sitter::Parser;
use xmlparser::{ElementEnd, Error, StrSpan, StreamError, Token, Tokenizer};

use crate::prelude::*;

use crate::analyze::{Scope, Type, normalize};
use crate::component::{ComponentTemplate, PropType};
use crate::index::Index;
use crate::model::{Field, FieldKind, PropertyKind};
use crate::record::Record;
use crate::template::gather_templates;
use crate::{ImStr, errloc, format_loc, some, utils::*};
use crate::{backend::Backend, backend::Text};

#[cfg(test)]
mod tests;

/// All Odoo action model types that can be referenced in menuitem action attributes
const ACTION_MODELS: &[&str] = &[
	"ir.actions.act_window",
	"ir.actions.report",
	"ir.actions.server",
	"ir.actions.client",
	"ir.actions.act_url",
];

#[derive(Debug)]
enum RefKind<'a> {
	/// `<field name=bar ref=".."/>`
	Ref(&'a str),
	Model,
	Id,
	/// A list of `(start_byte, field)` which came before this property, or empty if a top-level field.
	PropertyName(Vec<(usize, &'a str)>),
	MethodName(Vec<(usize, &'a str)>),
	TName,
	TInherit,
	TCall,
	/// ref'd value is a prop of this component.
	PropOf(&'a str),
	/// An arbitrary Python expression.
	/// Includes the relative offset of where the cursor is.
	PyExpr(usize),
	/// `<Component />`
	Component,
	/// `<field widget=".."/>`
	Widget,
	/// `<field name="tag">..</field>`
	ActionTag,
}

enum Tag<'a> {
	Field,
	Button,
	Template,
	Record,
	Menuitem,
	TComponent(&'a str),
}

impl Backend {
	pub fn update_xml(
		&self,
		root: Spur,
		text: &Text,
		uri: &Uri,
		rope: RopeSlice<'_>,
		did_save: bool,
	) -> anyhow::Result<()> {
		let text = match text {
			Text::Full(full) => Cow::Borrowed(full.as_str()),
			// Assume rope is up to date
			Text::Delta(_) => Cow::from(rope),
		};
		let mut reader = Tokenizer::from(text.as_ref());
		let mut record_ranges = vec![];
		let path = uri.to_file_path().ok_or_else(|| errloc!("uri.to_file_path failed"))?;
		let current_module = self
			.index
			.find_module_of(&path)
			.ok_or_else(|| errloc!("module_of_path for {} failed", uri.path().as_str()))?;
		let mut record_prefix = if did_save {
			Some(
				self.index
					.records
					.by_prefix
					.write()
					.expect(format_loc!("cannot acquire write lock now")),
			)
		} else {
			None
		};
		let path = uri.to_file_path().unwrap();
		let path_uri = PathSymbol::strip_root(root, &path);
		loop {
			match reader.next() {
				Some(Ok(Token::ElementStart { local, span, .. })) => {
					let offset = ByteOffset(span.start());
					if matches!(local.as_str(), "record" | "template" | "menuitem") {
						let record = match local.as_str() {
							"record" => Record::from_reader(offset, current_module, path_uri, &mut reader, rope),
							"template" => Record::template(offset, current_module, path_uri, &mut reader, rope),
							"menuitem" => Record::menuitem(offset, current_module, path_uri, &mut reader, rope),
							_ => unreachable!(),
						};
						let record = match record {
							Ok(Some(rec)) => rec,
							Ok(None) => {
								if local.as_str() != "template" {
									continue;
								}
								// legacy <templates /> container without id=
								let mut entries = vec![];
								if let Err(err) = gather_templates(path_uri, &mut reader, rope, &mut entries, true) {
									warn!("{err}");
									continue;
								}
								record_ranges.extend(
									entries
										.into_iter()
										.map(|entry| rope_conv(entry.template.location.unwrap().range, rope).unwrap()),
								);
								continue;
							}
							Err(err) => {
								warn!(
									target: "on_change_xml",
									"{local} could not be completely parsed: {}\n{err}",
									uri.path().as_str()
								);
								continue;
							}
						};
						let Ok(range) = rope_conv(record.location.range, rope) else {
							debug!("no range for {}", record.id);
							continue;
						};
						record_ranges.push(range);
						if let Some(prefix) = record_prefix.as_mut() {
							self.index
								.records
								.insert(_I(record.qualified_id()).into(), record, Some(prefix));
						}
					} else if local.as_str() == "templates" {
						let mut entries = vec![];
						if let Err(err) = gather_templates(path_uri, &mut reader, rope, &mut entries, false) {
							warn!("gather_templates failed: {err}");
							continue;
						}
						record_ranges.extend(
							entries
								.into_iter()
								.map(|entry| rope_conv(entry.template.location.unwrap().range, rope).unwrap()),
						);
					}
				}
				None => break,
				Some(Err(err)) => {
					debug!("error parsing xml:\n{err}");
					break;
				}
				_ => {}
			}
		}
		self.record_ranges
			.insert(uri.path().as_str().to_string(), record_ranges.into_boxed_slice());
		Ok(())
	}
	/// Returns `(slice, offset_at_cursor, relative_offset)`.
	/// - `slice` spans the record range that contains the cursor.
	/// - `offset_at_cursor` is `position` relative to the start of the slice.
	/// - `relative_offset` is the offset of the entire slice relative to the start of the rope.
	fn record_slice<'rope>(
		&self,
		rope: RopeSlice<'rope>,
		uri: &Uri,
		position: Position,
	) -> anyhow::Result<(RopeSlice<'rope>, ByteOffset, usize)> {
		let ranges = self
			.record_ranges
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build record ranges for {}", uri.path().as_str()))?;
		let mut offset_at_cursor = ok!(rope_conv(position, rope));
		let Ok(record) = ranges.value().binary_search_by(|range| {
			if offset_at_cursor < range.start {
				Ordering::Greater
			} else if offset_at_cursor > range.end {
				Ordering::Less
			} else {
				Ordering::Equal
			}
		}) else {
			debug!("(record_slice) fall back to full slice");
			// let slice = Cow::from(rope.slice(..));
			return Ok((rope.slice(..), offset_at_cursor, 0));
		};
		let record_range = &ranges.value()[record];
		debug!(
			"(record_slice) found {:?}, cursor={:?}",
			record_range.erase(),
			offset_at_cursor
		);
		let relative_offset = record_range.start.0;
		offset_at_cursor.0 = offset_at_cursor.0.saturating_sub(relative_offset);

		let slice = rope.byte_slice(record_range.erase());

		Ok((slice, offset_at_cursor, relative_offset))
	}
	pub fn did_save_xml(&self, uri: Uri, root: Spur) -> anyhow::Result<()> {
		let rope = {
			let document = self
				.document_map
				.get(uri.path().as_str())
				.ok_or_else(|| errloc!("(did_save) did not build document"))?;
			document.rope.clone()
		};
		self.update_xml(root, &Text::Delta(vec![]), &uri, rope.slice(..), true)?;
		Ok(())
	}
	pub fn xml_completions(
		&self,
		params: CompletionParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let (slice, offset_at_cursor, relative_offset) = self.record_slice(rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let path = some!(uri.to_file_path());
		let completions_limit = self
			.workspaces
			.find_workspace_of(&path, |_, ws| ws.completions.limit)
			.unwrap_or_else(|| self.project_config.completions_limit.load(Relaxed));

		self.index.xml_completions(
			&path,
			completions_limit,
			rope,
			offset_at_cursor,
			relative_offset,
			&mut reader,
		)
	}
	pub fn xml_jump_def(&self, params: GotoDefinitionParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Location>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
			scope,
		} = self.index.gather_refs(cursor_by_char, &mut reader, slice)?;

		let Some((mut needle, _)) = ref_at_cursor else {
			return Ok(None);
		};
		match ref_kind {
			Some(RefKind::Ref(_)) => self.index.jump_def_xml_id(needle, uri),
			Some(RefKind::Model) => self.index.jump_def_model(needle),
			Some(RefKind::PropertyName(access)) | Some(RefKind::MethodName(access)) => {
				let model_filters = some!(model_filter);
				// For property/method access, we expect a single model
				let mut model_filter = some!(model_filters.first()).clone();
				let mapped = format!(
					"{}.{needle}",
					access.iter().map(|(_, prop)| *prop).collect::<Vec<_>>().join(".")
				);
				if !access.is_empty() {
					needle = mapped.as_str();
					let mut model = _I(&model_filter);
					some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
					model_filter = ImStr::from(_R(model));
				}
				self.index.jump_def_property_name(needle, &model_filter)
			}
			Some(RefKind::TInherit) | Some(RefKind::TName) | Some(RefKind::TCall) => {
				self.index.jump_def_template_name(needle)
			}
			Some(RefKind::PropOf(component)) => {
				if let Some((handler, _)) = needle.split_once('.') {
					needle = handler;
				}
				self.index.jump_def_component_prop(component, needle)
			}
			Some(RefKind::Id) => self.index.jump_def_xml_id(needle, uri),
			Some(RefKind::PyExpr(py_offset)) => {
				let mut parser = Parser::new();
				parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
				let contents = needle.as_bytes();
				let ast = some!(parser.parse(contents, None));
				let (object, field, _) = some!(Self::attribute_node_at_offset(py_offset, ast.root_node(), contents));
				let model = some!(self.index.type_of(object, &scope, contents));
				let model = some!(self.index.try_resolve_model(&model, &Scope::default()));
				self.index.jump_def_property_name(&field, _R(model))
			}
			Some(RefKind::Component) => {
				let component = some!(_G(needle));
				let component = some!(self.index.components.get(&component.into()));
				let Some(ComponentTemplate::Name(template)) = component.template.as_ref() else {
					return Ok(None);
				};
				self.index.jump_def_template_name(_R(*template))
			}
			Some(RefKind::Widget) => self.index.jump_def_widget(needle),
			Some(RefKind::ActionTag) => self.index.jump_def_action_tag(needle),
			None => Ok(None),
		}
	}
	pub fn xml_references(
		&self,
		params: ReferenceParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Vec<Location>>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let path = some!(uri.to_file_path());
		let (slice, cursor_by_char, _) = self.record_slice(rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let XmlRefs {
			ref_at_cursor: cursor_value,
			ref_kind,
			..
		} = self.index.gather_refs(cursor_by_char, &mut reader, slice)?;

		let (cursor_value, _) = some!(cursor_value);
		let current_module = self.index.find_module_of(&path);
		match ref_kind {
			Some(RefKind::Model) => {
				let model = some!(_G(cursor_value));
				self.model_references(&path, &model.into())
			}
			Some(RefKind::Ref(_)) | Some(RefKind::Id) => self.record_references(&path, cursor_value, current_module),
			Some(RefKind::TInherit) | Some(RefKind::TCall) => self.index.template_references(cursor_value, true),
			Some(RefKind::TName) => self.index.template_references(cursor_value, false),
			Some(RefKind::PyExpr(_))
			| Some(RefKind::PropertyName(_))
			| Some(RefKind::MethodName(_))
			| Some(RefKind::PropOf(..))
			| Some(RefKind::Component)
			| Some(RefKind::Widget)
			| Some(RefKind::ActionTag)
			| None => Ok(None),
		}
	}
	pub fn xml_hover(&self, params: HoverParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Hover>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let path = some!(uri.to_file_path());
		let (slice, offset_at_cursor, relative_offset) = self.record_slice(rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
			scope,
		} = self.index.gather_refs(offset_at_cursor, &mut reader, slice)?;

		let (mut needle, ref_range) = some!(ref_at_cursor);

		let mut lsp_range = rope_conv(
			ref_range.clone().map_unit(|unit| ByteOffset(unit + relative_offset)),
			rope,
		)
		.ok();
		let current_module = self.index.find_module_of(&path);
		match ref_kind {
			Some(RefKind::Model) => self.index.hover_model(needle, lsp_range, false, None),
			Some(RefKind::Ref(_)) => {
				if needle.contains('.') {
					self.index.hover_record(needle, lsp_range)
				} else {
					let current_module = some!(current_module);
					let xml_id = format!("{}.{}", _R(current_module), needle);
					self.index.hover_record(&xml_id, lsp_range)
				}
			}
			Some(RefKind::PropertyName(access)) | Some(RefKind::MethodName(access)) => {
				// let model = some!(model_filter);
				let model_filters = some!(model_filter);
				// For property/method access, we expect a single model
				let mut model_filter = some!(model_filters.first()).clone();
				let mapped = format!(
					"{}.{needle}",
					access.iter().map(|(_, field)| *field).collect::<Vec<_>>().join(".")
				);
				if !access.is_empty() {
					needle = mapped.as_str();
					let mut model = _I(&model_filter);
					some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
					model_filter = ImStr::from(_R(model));
				}
				self.index.hover_property_name(needle, &model_filter, lsp_range)
			}
			Some(RefKind::Id) => {
				let current_module = some!(current_module);
				let xml_id = if needle.contains('.') {
					Cow::from(needle)
				} else {
					format!("{}.{}", _R(current_module), needle).into()
				};
				self.index.hover_record(&xml_id, lsp_range)
			}
			Some(RefKind::TInherit) | Some(RefKind::TCall) => Ok(self.index.hover_template(needle, lsp_range)),
			Some(RefKind::PyExpr(py_offset)) => {
				let mut parser = Parser::new();
				parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
				let contents = needle.as_bytes();
				let ast = some!(parser.parse(contents, None));
				let Some((object, field, range)) = Self::attribute_node_at_offset(py_offset, ast.root_node(), contents)
				else {
					let cursor_node = some!(ast.root_node().named_descendant_for_byte_range(py_offset, py_offset));
					let needle = String::from_utf8_lossy(&contents[cursor_node.byte_range()]);
					let scope_type = some!(scope.get(needle.as_ref()));
					let lsp_range = rope_conv(
						cursor_node
							.byte_range()
							.clone()
							.map_unit(|unit| ByteOffset(unit + ref_range.start + relative_offset)),
						rope,
					)
					.ok();
					if let Some(model) = (self.index).try_resolve_model(scope_type, &scope) {
						return self.index.hover_model(_R(model), lsp_range, true, Some(&needle));
					}
					return Ok(Some(Hover {
						range: lsp_range,
						contents: HoverContents::Scalar(MarkedString::from_language_code(
							"python".to_string(),
							format!("(local) {needle}: Any"),
						)),
					}));
				};
				let model = some!(self.index.type_of(object, &scope, contents));
				let model = some!(self.index.try_resolve_model(&model, &scope));
				let anchor = ref_range.start + relative_offset;
				self.index.hover_property_name(
					&field,
					_R(model),
					rope_conv(range.map_unit(|rel_unit| ByteOffset(rel_unit + anchor)), rope).ok(),
				)
			}
			Some(RefKind::Component) => Ok(self.index.hover_component(needle, lsp_range)),
			Some(RefKind::PropOf(component_key)) => {
				if let Some((handler, _)) = needle.split_once('.') {
					// accept handler.bind syntax as well
					if let Some(lsp_range) = lsp_range.as_mut() {
						lsp_range.end.character = lsp_range.start.character + handler.len() as u32;
					}
					needle = handler;
				}
				let prop = some!(_G(needle));
				let component = some!(_G(component_key));
				let component = some!(self.index.components.get(&component.into()));
				let field = some!(component.props.get(&prop.into()));
				let type_descriptor = field.type_ & !(PropType::Optional | PropType::Array);
				let needs_paren = field.type_.contains(PropType::Array) && type_descriptor.iter().count() > 1;
				let contents = fomat! {
					"(property) " (component_key) "." (needle)
					if field.type_.contains(PropType::Optional) { "?" } ": "
					if needs_paren { "(" }
					for type_ in type_descriptor.iter() {
						match type_ {
							PropType::String => { "string" }
							PropType::Number => { "number" }
							PropType::Boolean => { "boolean" }
							PropType::Object => { "object" }
							PropType::Function => { "Function" }
							_ => { "unknown" }
						}
					}
					separated { " | " }
					if needs_paren { ")" }
					if field.type_.contains(PropType::Array) { "[]" }
				};
				Ok(Some(Hover {
					range: lsp_range,
					contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
						language: "ts".to_string(),
						value: contents,
					})),
				}))
			}
			Some(RefKind::TName) | Some(RefKind::Widget) | Some(RefKind::ActionTag) | None => {
				#[cfg(not(debug_assertions))]
				return Ok(None);

				#[cfg(debug_assertions)]
				{
					let contents = format!("{:#?}", (needle, ref_kind, model_filter));
					Ok(Some(Hover {
						contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
							language: "rust".to_string(),
							value: contents,
						})),
						range: lsp_range,
					}))
				}
			}
		}
	}
	pub fn xml_code_actions(
		&self,
		params: CodeActionParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<CodeActionResponse>> {
		let uri = &params.text_document.uri;
		let position = params.range.start;
		let (slice, offset_at_cursor, _) = self.record_slice(rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());

		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			..
		} = self.index.gather_refs(offset_at_cursor, &mut reader, slice)?;
		let (Some((value, _)), Some(RefKind::Component)) = (ref_at_cursor, ref_kind) else {
			return Ok(None);
		};

		Ok(Some(vec![CodeActionOrCommand::Command(Command {
			title: "Go to Owl component".to_string(),
			command: "goto_owl".to_string(),
			arguments: Some(vec![String::new().into(), value.into()]),
		})]))
	}
}

impl Index {
	fn xml_completions<'read>(
		&self,
		path: &Path,
		completions_limit: usize,
		rope: RopeSlice<'read>,
		offset_at_cursor: ByteOffset,
		relative_offset: usize,
		reader: &mut Tokenizer<'read>,
	) -> Result<Option<CompletionResponse>, anyhow::Error> {
		let current_module = self.find_module_of(path).expect("must be in a module");

		let mut items = MaxVec::new(completions_limit);
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
			scope,
		} = self.gather_refs(offset_at_cursor, reader, rope)?;
		let (Some((value, value_range)), Some(record_field)) = (ref_at_cursor, ref_kind) else {
			return Ok(None);
		};
		let mut needle = if offset_at_cursor.0 >= value_range.end {
			// Cursor is at or after the end of the value, use the full value as needle
			value
		} else if offset_at_cursor.0 >= value_range.start {
			// Cursor is within the value, use substring up to cursor
			&value[..offset_at_cursor.0 - value_range.start]
		} else {
			// Cursor is before the value, use empty needle
			""
		};
		let replace_range = value_range.clone().map_unit(|unit| ByteOffset(unit + relative_offset));
		match record_field {
			RefKind::Ref(relation) => {
				let model_filters = some!(model_filter);
				// For Ref relation, we expect a single model
				let model_key = some!(model_filters.first());
				let model = some!(_G(model_key));
				let relation = {
					let fields = some!(self.models.populate_properties(model.into(), &[])).downgrade();
					let fields = some!(fields.fields.as_ref());
					let relation = some!(_G(relation));
					let Some(Field {
						kind: FieldKind::Relational(relation),
						..
					}) = fields.get(&relation.into()).map(Arc::as_ref)
					else {
						return Ok(None);
					};
					*relation
				};
				self.complete_xml_id(
					needle,
					replace_range,
					rope,
					Some(&[ImStr::from(_R(relation))]),
					current_module,
					&mut items,
				)?
			}
			RefKind::Model => {
				let range = rope_conv(replace_range, rope)?;
				self.complete_model(needle, range, &mut items)?
			}
			ref ref_kind @ RefKind::PropertyName(ref access) | ref ref_kind @ RefKind::MethodName(ref access) => {
				let model_filters = some!(model_filter);
				// For property/method access, we expect a single model
				let mut model_filter = some!(model_filters.first()).clone();
				let mapped = format!(
					"{}.{needle}",
					access.iter().map(|(_, field)| *field).collect::<Vec<_>>().join(".")
				);
				if !access.is_empty() {
					needle = mapped.as_str();
					let mut model = _I(&model_filter);
					some!(self.models.resolve_mapped(&mut model, &mut needle, None).ok());
					model_filter = ImStr::from(_R(model));
				}
				self.complete_property_name(
					needle,
					replace_range,
					model_filter,
					rope,
					Some(if matches!(ref_kind, RefKind::MethodName(_)) {
						PropertyKind::Method
					} else {
						PropertyKind::Field
					}),
					true,
					&mut items,
				)?;
			}
			RefKind::TInherit | RefKind::TCall => {
				let range = rope_conv(replace_range, rope)?;
				self.complete_template_name(needle, range, &mut items)?;
			}
			RefKind::PropOf(component) => {
				self.complete_component_prop(/*needle,*/ replace_range, rope, component, &mut items)?;
			}
			RefKind::Id => {
				self.complete_xml_id(
					needle,
					replace_range,
					rope,
					model_filter.as_deref(),
					current_module,
					&mut items,
				)?;
			}
			RefKind::PyExpr(py_offset) => 'expr: {
				let mut parser = Parser::new();
				parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
				let ast = some!(parser.parse(value, None));
				let contents = value.as_bytes();
				let Some((object, field, range)) =
					Backend::attribute_node_at_offset(py_offset, ast.root_node(), contents)
				else {
					// Suggest items in the current scope
					items.extend(scope.iter().map(|(ident, model)| {
						let Some(model) = self.try_resolve_model(model, &scope) else {
							return CompletionItem {
								label: ident.to_string(),
								kind: Some(CompletionItemKind::VARIABLE),
								..Default::default()
							};
						};
						let model_name = _R(model);
						let documentation = self.models.get(&model).map(|model| {
							Documentation::MarkupContent(MarkupContent {
								kind: MarkupKind::Markdown,
								value: self.model_docstring(&model, Some(model_name), Some(ident)),
							})
						});
						CompletionItem {
							label: ident.to_string(),
							kind: Some(CompletionItemKind::VARIABLE),
							detail: Some(model_name.to_string()),
							documentation,
							..Default::default()
						}
					}));
					break 'expr;
				};
				// in this case walk_scope eventually ends so just retrieve the scope under its control
				let (default_scope, scope) = Index::walk_scope(ast.root_node(), Some(scope.clone()), |scope, node| {
					self.build_scope(scope, node, range.end, contents)
				});
				let scope = scope.unwrap_or(default_scope);
				let model = some!(self.type_of(object, &scope, contents));
				let model = some!(self.try_resolve_model(&model, &scope));
				let needle_end = py_offset.saturating_sub(range.start);
				let mut needle = field.as_ref();
				if !needle.is_empty() && needle_end < needle.len() {
					needle = &needle[..needle_end];
				}
				let anchor = value_range.start + relative_offset;
				self.complete_property_name(
					needle,
					range.map_unit(|rel_unit| ByteOffset(rel_unit + anchor)),
					ImStr::from(_R(model)),
					rope,
					None, // Field would be better, but leave this here at least until @property is implemented
					false,
					&mut items,
				)?;
			}
			RefKind::Widget => {
				self.complete_widget(/*needle/, */ replace_range, rope, &mut items)?;
			}
			RefKind::ActionTag => {
				self.complete_action_tag(/*needle/, */ replace_range, rope, &mut items)?;
			}
			RefKind::TName | RefKind::Component => return Ok(None),
		}

		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: !items.has_space(),
			items: items.into_inner(),
		})))
	}
	/// The main function that determines all the information needed
	/// to resolve the symbol at the cursor.
	#[instrument(level = "trace", skip_all, ret)]
	fn gather_refs<'read>(
		&self,
		offset_at_cursor: ByteOffset,
		reader: &mut Tokenizer<'read>,
		slice: RopeSlice<'read>,
	) -> anyhow::Result<XmlRefs<'read>> {
		let mut tag = None;
		let mut ref_at_cursor = None::<(&str, core::ops::Range<usize>)>;
		let mut ref_kind = None;
		let mut model_filter = None;
		let mut template_mode = false;
		let offset_at_cursor = offset_at_cursor.0;

		let mut arch_model = None;
		// <field name="arch">..</field>
		let mut arch_mode = false;
		let mut arch_depth = 0;
		let mut depth = 0;
		let mut expect_model_string = false;
		let mut expect_template_string = false;
		let mut expect_action_tag = false;
		let mut button_type: Option<&str> = None;
		let mut scope = Scope::default();
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();

		let mut foreach_as = attr_pair("t-foreach", "t-as");
		let mut set_value = attr_pair("t-set", "t-value");

		struct FieldAccess<'a> {
			field: StrSpan<'a>,
			depth: u32,
		}
		let mut accesses: Vec<FieldAccess> = vec![];

		for token in reader {
			match token {
				Ok(Token::ElementStart { local, prefix, .. }) => {
					expect_model_string = false;
					depth += 1;
					match local.as_str() {
						"field" => tag = Some(Tag::Field),
						"button" => {
							tag = Some(Tag::Button);
							button_type = None;
						}
						"template" => {
							tag = Some(Tag::Template);
							model_filter = Some(vec![ImStr::from("ir.ui.view")]);
						}
						"record" => tag = Some(Tag::Record),
						"menuitem" => tag = Some(Tag::Menuitem),
						"templates" => {
							template_mode = true;
						}
						component if template_mode && component.starts_with(|c| char::is_ascii_uppercase(&c)) => {
							tag = Some(Tag::TComponent(component));
							if prefix.is_empty() && local.range().contains_end(offset_at_cursor) {
								ref_at_cursor = Some((local.as_str(), local.range()));
								ref_kind = Some(RefKind::Component);
							}
						}
						_ if arch_mode => tag = None,
						_ => {}
					}
				}
				// <field name=.. ref=.. />
				Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Field)) => {
					let value_in_range = value.range().contains_end(offset_at_cursor);
					match local.as_str() {
						"ref" if value_in_range => {
							ref_at_cursor = Some((value.as_str(), value.range()));
						}
						"name" if value_in_range => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::PropertyName(
								accesses
									.iter()
									.map(|access| (access.field.start(), access.field.as_str()))
									.collect(),
							));
						}
						"name" if matches!(value.as_str(), "model" | "res_model") => {
							// contents should be a model string like res.partner
							// `res_model` is on ir.actions.act_window
							expect_model_string = true;
						}
						"name" if value.as_str() == "report_name" => {
							// string reference to a template (ir.ui.view)
							expect_template_string = true;
						}
						"name" if value.as_str() == "tag" => {
							// ir.actions tag for a client action (on the `actions` registry)
							expect_action_tag = true;
						}
						"name" if value.as_str() == "arch" => {
							arch_mode = true;
							arch_depth = depth
						}
						"name" => {
							// <field ref=.. name=.. />
							ref_kind = Some(RefKind::Ref(value.as_str()));
							if arch_mode {
								accesses.push(FieldAccess { field: value, depth });
							}
						}
						"position" => {
							// in <field name=.. position=.. />
							// don't register as access
							// TODO: If position comes first, this does not work.
							match accesses.last() {
								Some(access) if depth == access.depth => {
									accesses.pop();
								}
								_ => {}
							}
						}
						"groups" if value_in_range => {
							ref_kind = Some(RefKind::Id);
							model_filter = Some(vec![ImStr::from("res.groups")]);
							arch_model = None;
							determine_csv_xmlid_subgroup_of_xmlspan(&mut ref_at_cursor, value, offset_at_cursor);
						}
						"widget" if value_in_range && arch_depth > 0 => {
							ref_kind = Some(RefKind::Widget);
							ref_at_cursor = Some((value.as_str(), value.range()));
						}
						_ => {}
					}
				}
				// <button name=.. />
				Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Button)) => match local.as_str() {
					"type" => {
						button_type = Some(value.as_str());
					}
					"name" if value.range().contains_end(offset_at_cursor) => {
						if value.as_str().starts_with("%(") {
							if value.as_str().ends_with(")d") {
								let inner = &value.as_str()[2..value.as_str().len() - 2];
								let start_offset = value.range().start + 2;
								let end_offset = value.range().end - 2;
								ref_at_cursor = Some((inner, start_offset..end_offset));
							} else {
								let inner = &value.as_str()[2..];
								let start_offset = value.range().start + 2;
								let end_offset = value.range().end;
								ref_at_cursor = Some((inner, start_offset..end_offset));
							}
							ref_kind = Some(RefKind::Id);
							model_filter = Some(ACTION_MODELS.iter().map(|&s| ImStr::from_static(s)).collect());
						} else if button_type == Some("action") {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::Id);
							model_filter = Some(ACTION_MODELS.iter().map(|&s| ImStr::from_static(s)).collect());
						} else {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::MethodName(vec![]));
						}
					}
					_ => {}
				},
				// <template inherit_id=.. />
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Template))
						&& value.range().contains_end(offset_at_cursor)
						&& matches!(local.as_str(), "inherit_id" | "t-call") =>
				{
					ref_at_cursor = Some((value.as_str(), value.range()));
					ref_kind = Some(RefKind::Ref("inherit_id"));
				}
				// <record model=.. />
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Record)) && local.as_str() == "model" =>
				{
					if value.range().contains_end(offset_at_cursor) {
						ref_at_cursor = Some((value.as_str(), value.range()));
						ref_kind = Some(RefKind::Model);
					} else {
						model_filter = Some(vec![ImStr::from(value.as_str())]);
					}
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Record | Tag::Template | Tag::Menuitem))
						&& local == "id" && value.range().contains_end(offset_at_cursor) =>
				{
					ref_at_cursor = Some((value.as_str(), value.range()));
					ref_kind = Some(RefKind::Id);
					arch_model = None;
					match tag {
						Some(Tag::Template) => {
							model_filter = Some(vec![ImStr::from("ir.ui.view")]);
						}
						Some(Tag::Menuitem) => {
							model_filter = Some(vec![ImStr::from("ir.ui.menu")]);
						}
						_ => {}
					}
				}
				// <menuitem parent=.. action=.. />
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Menuitem)) && value.range().contains_end(offset_at_cursor) =>
				{
					match local.as_str() {
						"parent" => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::Ref("parent_id"));
							model_filter = Some(vec![ImStr::from("ir.ui.menu")]);
						}
						"action" => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::Id);
							model_filter = Some(ACTION_MODELS.iter().map(|&s| ImStr::from_static(s)).collect());
						}
						"groups" => {
							ref_kind = Some(RefKind::Id);
							arch_model = None;
							model_filter = Some(vec![ImStr::from_static("res.groups")]);
							determine_csv_xmlid_subgroup_of_xmlspan(&mut ref_at_cursor, value, offset_at_cursor);
						}
						_ => {}
					}
				}
				// <Component prop=.. />
				Ok(Token::Attribute { local, .. })
					if matches!(tag, Some(Tag::TComponent(..))) && local.range().contains_end(offset_at_cursor) =>
				{
					let Some(Tag::TComponent(component)) = tag else {
						unreachable!()
					};
					ref_at_cursor = Some((local.as_str(), local.range()));
					ref_kind = Some(RefKind::PropOf(component));
				}
				// catchall cases
				Ok(Token::Attribute { local, value, .. }) if value.range().contains_end(offset_at_cursor) => {
					match local.as_str() {
						"t-name" => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::TName);
						}
						"t-inherit" => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::TInherit);
						}
						"t-call" => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::TCall);
						}
						// TODO: Limit cases of Python expressions
						t_attr if t_attr.starts_with("t-") => {
							ref_at_cursor = Some((value.as_str(), value.range()));
							ref_kind = Some(RefKind::PyExpr(offset_at_cursor.saturating_sub(value.start())))
						}
						"groups" => {
							ref_kind = Some(RefKind::Id);
							model_filter = Some(vec![ImStr::from("res.groups")]);
							arch_model = None;
							determine_csv_xmlid_subgroup_of_xmlspan(&mut ref_at_cursor, value, offset_at_cursor);
						}
						_ => {}
					}
				}
				// general bookkeeping
				Ok(Token::Attribute { local, value, .. }) => {
					(foreach_as.accept(local.as_str(), value)).and_then(|((foreach, _), (as_, _))| {
						let contents = foreach.as_bytes();
						let ast = parser.parse(contents, None)?;
						self.insert_in_scope(&mut scope, &as_, ast.root_node(), contents)
							.inspect_err(|err| {
								debug!("(gather_refs) foreach_as failed: {err}");
							})
							.ok()
					});
					(set_value.accept(local.as_str(), value)).and_then(|((set, _), (value, _))| {
						let contents = value.as_bytes();
						let ast = parser.parse(contents, None)?;
						self.insert_in_scope(&mut scope, &set, ast.root_node(), contents)
							.inspect_err(|err| {
								debug!("(gather_refs) set_value failed: {err}");
							})
							.ok()
					});
					if local.as_str() == "t-name" && !template_mode {
						template_mode = true;
					}
				}
				Ok(Token::Text { text }) if expect_model_string => {
					expect_model_string = false;
					if text.range().contains_end(offset_at_cursor) {
						ref_at_cursor = Some((text.as_str(), text.range()));
						ref_kind = Some(RefKind::Model);
					} else {
						arch_model = Some(text);
					}
				}
				Ok(Token::Text { text }) if expect_template_string => {
					expect_template_string = false;
					if text.range().contains_end(offset_at_cursor) {
						ref_at_cursor = Some((text.as_str(), text.range()));
						ref_kind = Some(RefKind::Ref("inherit_id"));
						model_filter = Some(vec![ImStr::from("ir.ui.view")]);
					}
				}
				Ok(Token::Text { text }) if expect_action_tag => {
					expect_action_tag = false;
					if text.range().contains_end(offset_at_cursor) {
						ref_at_cursor = Some((text.as_str(), text.range()));
						ref_kind = Some(RefKind::ActionTag);
					}
				}
				Ok(Token::ElementEnd { end, span }) => {
					foreach_as.reset();
					set_value.reset();
					if let ElementEnd::Close(..) | ElementEnd::Empty = end {
						if depth == arch_depth {
							arch_mode = false;
						}
						match accesses.last() {
							Some(access) if access.depth == depth => {
								accesses.pop();
							}
							_ => {}
						}
						if matches!(tag, Some(Tag::Button)) {
							button_type = None;
							tag = None;
						}
						depth -= 1;
					}
					if template_mode
						&& matches!(end, ElementEnd::Open | ElementEnd::Empty)
						&& span.start() > offset_at_cursor
						&& let Some(c) = slice.get_byte(offset_at_cursor)
						&& c.is_ascii_whitespace()
						&& let Some(Tag::TComponent(component)) = tag
					{
						// edge case: completion trigger in the middle of tag start
						ref_at_cursor = Some(("", offset_at_cursor..offset_at_cursor));
						ref_kind = Some(RefKind::PropOf(component));
					}
					if ref_at_cursor.is_some() {
						break;
					}
					if let Some(Tag::TComponent(..)) = &tag {
						_ = tag.take();
					}
					// no need to turn off template mode yet
					// match end {
					// 	ElementEnd::Open => depth += 1,
					// 	ElementEnd::Close(..) => {
					// 		depth = depth.saturating_sub(1);
					// 		if depth <= 1 {
					// 			template_mode = false;
					// 		}
					// 	}
					// 	_ => {}
					// }
				}
				Ok(Token::Comment { text, .. }) if text.trim_start().starts_with("@type") => {
					let annotation = text.trim().strip_prefix("@type").unwrap();
					let Some((identifier, model)) = annotation.trim().split_once(|c: char| c.is_ascii_whitespace())
					else {
						continue;
					};
					scope.insert(identifier.trim().to_string(), Type::Model(model.trim().into()));
				}
				// HACK: The lexer can't deal with naked attributes, so we try to parse them manually.
				Err(Error::InvalidAttribute(StreamError::InvalidChar(_, b'=', mut expected_eq_pos), start_pos)) => {
					let Some(Tag::TComponent(component)) = tag else {
						break;
					};

					// <Component prop />
					// move one place back to get the attribute name
					expected_eq_pos.col = expected_eq_pos.col.saturating_sub(1);
					let start_pos: ByteOffset = rope_conv(span_conv(start_pos), slice)?;
					let expected_eq_pos: ByteOffset = rope_conv(span_conv(expected_eq_pos), slice)?;

					// may be an invalid attribute, but no point in checking
					let mut range = (start_pos..expected_eq_pos).erase();
					if !range.contains_end(offset_at_cursor) {
						break;
					}
					let Cow::Borrowed(mut attr) = Cow::from(slice.byte_slice(range.clone())) else {
						panic!("(gather_refs) doesn't support cross-chunked boundaries");
					};
					let trimmed = attr.trim_end_matches(|c: char| !c.is_ascii_alphanumeric());
					if trimmed != attr {
						attr = trimmed;
						range = range.start..range.start + trimmed.len();
					}
					let trimmed = attr.trim_start_matches(|c: char| !c.is_ascii_alphanumeric());
					if trimmed != attr {
						range = range.start + (attr.len() - trimmed.len())..range.end;
						attr = trimmed;
					}

					ref_at_cursor = Some((attr, range));
					ref_kind = Some(RefKind::PropOf(component));
					break;
				}
				Err(_) => break,
				Ok(token) => {
					if token_span(&token).start() > offset_at_cursor {
						break;
					}
				}
			}
		}
		let model_filter = if arch_mode {
			// For action references (%(...)d patterns), keep the specific model_filter
			// Don't override with arch_model for action references
			if matches!(ref_kind, Some(RefKind::Id))
				&& model_filter
					.as_ref()
					.map(|v| v.iter().any(|m| m.as_str() == "ir.actions.act_window"))
					.unwrap_or(false)
			{
				model_filter
			} else {
				arch_model.map(|span| vec![ImStr::from(span.as_str())]).or(model_filter)
			}
		} else {
			model_filter
		};
		Ok(XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
			scope,
		})
	}
	/// [Type::Value] may be inserted by this method.
	fn insert_in_scope(
		&self,
		scope: &mut Scope,
		identifier: &str,
		mut root: tree_sitter::Node,
		contents: &[u8],
	) -> anyhow::Result<()> {
		normalize(&mut root);
		let type_ = self.type_of(root, scope, contents).unwrap_or(Type::Value);
		let type_ = self
			.try_resolve_model(&type_, scope)
			.map(|model| Type::Model(_R(model).into()))
			.unwrap_or(type_);
		scope.insert(identifier.to_string(), type_);
		Ok(())
	}
}

#[derive(Debug)]
struct XmlRefs<'a> {
	ref_at_cursor: Option<(&'a str, core::ops::Range<usize>)>,
	ref_kind: Option<RefKind<'a>>,
	model_filter: Option<Vec<ImStr>>,
	/// used for Python inline expressions
	scope: Scope,
}

#[inline]
fn determine_csv_xmlid_subgroup_of_xmlspan<'text>(
	ref_at_cursor: &mut Option<(&'text str, core::ops::Range<usize>)>,
	value: StrSpan<'text>,
	offset_at_cursor: usize,
) {
	determine_csv_xmlid_subgroup(ref_at_cursor, (value.as_str(), value.range()), offset_at_cursor);
}

/// - `ref_at_cursor`: out-reference for the subgroup in cursor alongside its absolute range
/// - `value`: a pair of (text, byterange) of the groups CSV string
pub(crate) fn determine_csv_xmlid_subgroup<'text>(
	ref_at_cursor: &mut Option<(&'text str, core::ops::Range<usize>)>,
	value: (&'text str, core::ops::Range<usize>),
	offset_at_cursor: usize,
) {
	let (value, range) = value;
	let mut start = range.start;
	let mut csv_groups = value;
	if !csv_groups.contains(',') {
		*ref_at_cursor = Some((csv_groups, range));
		return;
	}
	loop {
		let mut last_subgroup = false;
		let subgroup = match csv_groups.split_once(',') {
			Some((subgroup, rest)) => {
				csv_groups = rest;
				subgroup
			}
			None => {
				last_subgroup = true;
				csv_groups
			}
		};
		let range = start..start + subgroup.len();
		if range.contains_end(offset_at_cursor) {
			*ref_at_cursor = Some((subgroup, range));
			break;
		}
		start += subgroup.len() + 1;
		if last_subgroup {
			break;
		}
	}
}

struct AttrPair<'a> {
	lhs: &'a str,
	rhs: &'a str,
	lhs_val: Option<(ImStr, usize)>,
	rhs_val: Option<(ImStr, usize)>,
}
fn attr_pair<'a>(lhs: &'a str, rhs: &'a str) -> AttrPair<'a> {
	AttrPair {
		lhs,
		rhs,
		lhs_val: None,
		rhs_val: None,
	}
}

impl AttrPair<'_> {
	#[must_use]
	fn accept(&mut self, local: &str, value: StrSpan) -> Option<((ImStr, usize), (ImStr, usize))> {
		if local == self.lhs {
			self.lhs_val = Some((value.as_str().into(), value.start()));
		} else if local == self.rhs {
			self.rhs_val = Some((value.as_str().into(), value.start()));
		}
		if let Some((lhs, lhs_start)) = &self.lhs_val
			&& let Some((rhs, rhs_start)) = &self.rhs_val
		{
			let res = Some(((lhs.clone(), *lhs_start), (rhs.clone(), *rhs_start)));
			self.reset();
			return res;
		}
		None
	}
	fn reset(&mut self) {
		self.lhs_val = None;
		self.rhs_val = None;
	}
}
