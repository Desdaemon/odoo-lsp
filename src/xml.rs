use crate::{Backend, Text};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;

use lasso::{Spur, ThreadedRodeo};
use log::{debug, warn};
use miette::diagnostic;
use odoo_lsp::index::interner;
use odoo_lsp::model::{Field, FieldKind};
use odoo_lsp::template::gather_templates;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{StrSpan, Token, Tokenizer};

use odoo_lsp::record::Record;
use odoo_lsp::{some, utils::*};

enum RefKind {
	Ref(Spur),
	Model,
	Id,
	FieldName,
	TName,
	TInherit,
	TCall,
}

enum Tag {
	Field,
	Template,
	Record,
	Menuitem,
}

impl Backend {
	pub async fn on_change_xml(&self, text: &Text, uri: &Url, rope: &Rope) -> miette::Result<()> {
		let interner = interner();
		let text = match text {
			Text::Full(full) => Cow::Borrowed(full.as_str()),
			// Assume rope is up to date
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let mut reader = Tokenizer::from(text.as_ref());
		let mut record_ranges = vec![];
		let current_module = self
			.index
			.module_of_path(Path::new(uri.path()))
			.ok_or_else(|| diagnostic!("module_of_path for {} failed", uri.path()))?;
		let mut record_prefix = self.index.records.by_prefix.write().await;
		let path_uri = interner.get_or_intern(uri.path());
		loop {
			match reader.next() {
				Some(Ok(Token::ElementStart { local, span, .. })) => {
					let offset = ByteOffset(span.start());
					if matches!(local.as_str(), "record" | "template" | "menuitem") {
						let record = match local.as_str() {
							"record" => {
								Record::from_reader(offset, current_module, path_uri, &mut reader, rope.clone())
							}
							"template" => Record::template(offset, current_module, path_uri, &mut reader, rope.clone()),
							"menuitem" => Record::menuitem(offset, current_module, path_uri, &mut reader, rope.clone()),
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
								if let Err(err) =
									gather_templates(path_uri, &mut reader, rope.clone(), &mut entries, true)
								{
									warn!("gather_templates failed: {err}");
									continue;
								}
								record_ranges.extend(entries.into_iter().map(|entry| {
									lsp_range_to_offset_range(entry.template.location.unwrap().range, &rope).unwrap()
								}));
								continue;
							}
							Err(err) => {
								warn!(
									"{} could not be completely parsed: {}\n{err}",
									local.as_str(),
									uri.path()
								);
								continue;
							}
						};
						let Some(range) = lsp_range_to_offset_range(record.location.range, &rope) else {
							log::debug!("(on_change_xml) no range for {}", record.id);
							continue;
						};
						record_ranges.push(range);
						self.index
							.records
							.insert(
								interner.get_or_intern(record.qualified_id(interner)).into(),
								record,
								Some(&mut record_prefix),
							)
							.await;
					} else if local.as_str() == "templates" {
						let mut entries = vec![];
						if let Err(err) = gather_templates(path_uri, &mut reader, rope.clone(), &mut entries, false) {
							warn!("gather_templates failed: {err}");
							continue;
						}
						record_ranges.extend(entries.into_iter().map(|entry| {
							lsp_range_to_offset_range(entry.template.location.unwrap().range, &rope).unwrap()
						}));
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
			.insert(uri.path().to_string(), record_ranges.into_boxed_slice());
		Ok(())
	}
	fn record_slice<'rope>(
		&self,
		rope: &'rope Rope,
		uri: &Url,
		position: Position,
	) -> miette::Result<(Cow<'rope, str>, ByteOffset, usize)> {
		let ranges = self
			.record_ranges
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build record ranges for {}", uri.path()))?;
		let mut offset_at_cursor = position_to_offset(position, rope).ok_or_else(|| diagnostic!("cursor"))?;
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
			let slice = Cow::from(rope.slice(..));
			return Ok((slice, offset_at_cursor, 0));
		};
		let record_range = &ranges.value()[record];
		debug!(
			"(record_slice) found {:?}, cursor={:?}",
			record_range.erase(),
			offset_at_cursor
		);
		let relative_offset = record_range.start.0;
		offset_at_cursor.0 = offset_at_cursor.0.saturating_sub(relative_offset);

		let slice = rope.byte_slice(record_range.clone().map_unit(|unit| unit.0));
		let slice = Cow::from(slice);

		Ok((slice, offset_at_cursor, relative_offset))
	}
	pub async fn xml_completions(
		&self,
		params: CompletionParams,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let (slice, offset_at_cursor, relative_offset) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);

		let current_module = self
			.index
			.module_of_path(Path::new(uri.path()))
			.expect("must be in a module");

		let mut items = MaxVec::new(Self::LIMIT);
		let XmlRefs {
			ref_at_cursor: cursor_value,
			ref_kind,
			model_filter,
			..
		} = gather_refs(offset_at_cursor, &mut reader, interner())?;
		let (Some(value), Some(record_field)) = (cursor_value, ref_kind) else {
			return Ok(None);
		};
		let needle = &value.as_str()[..offset_at_cursor.0 - value.range().start];
		let replace_range = value.range().map_unit(|unit| ByteOffset(unit + relative_offset));
		match record_field {
			RefKind::Ref(relation) => {
				let model_key = some!(model_filter);
				let model = some!(interner().get(&model_key));
				let fields = some!(self.index.models.populate_field_names(model.into(), &[]));
				let fields = some!(fields.fields.as_ref());
				let Some(Field {
					kind: FieldKind::Relational(relation),
					..
				}) = fields.get(&relation.into())
				else {
					return Ok(None);
				};
				self.complete_xml_id(
					needle,
					replace_range,
					rope.clone(),
					Some(interner().resolve(relation)),
					current_module,
					&mut items,
				)
				.await?
			}
			RefKind::Model => {
				self.complete_model(needle, replace_range, rope.clone(), &mut items)
					.await?
			}
			RefKind::FieldName => {
				self.complete_field_name(needle, replace_range, some!(model_filter), rope.clone(), &mut items)
					.await?
			}
			RefKind::TInherit | RefKind::TCall => {
				self.complete_template_name(needle, replace_range, rope.clone(), &mut items)
					.await?;
			}
			RefKind::Id | RefKind::TName => return Ok(None),
		}

		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: !items.has_space(),
			items: items.into_inner(),
		})))
	}
	pub async fn xml_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);
		let XmlRefs {
			ref_at_cursor: cursor_value,
			ref_kind,
			model_filter,
			..
		} = gather_refs(cursor_by_char, &mut reader, interner())?;

		let Some(cursor_value) = cursor_value else {
			return Ok(None);
		};
		match ref_kind {
			Some(RefKind::Ref(_)) => self.jump_def_xml_id(&cursor_value, uri),
			Some(RefKind::Model) => self.jump_def_model(&cursor_value),
			Some(RefKind::FieldName) => {
				let model = some!(model_filter);
				self.jump_def_field_name(&cursor_value, &model).await
			}
			Some(RefKind::TInherit) | Some(RefKind::TName) | Some(RefKind::TCall) => {
				self.jump_def_template_name(&cursor_value)
			}
			Some(RefKind::Id) | None => Ok(None),
		}
	}
	pub fn xml_references(&self, params: ReferenceParams, rope: Rope) -> miette::Result<Option<Vec<Location>>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);
		let XmlRefs {
			ref_at_cursor: cursor_value,
			ref_kind,
			..
		} = gather_refs(cursor_by_char, &mut reader, interner())?;

		let Some(cursor_value) = cursor_value else {
			return Ok(None);
		};
		let current_module = self
			.index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()));
		match ref_kind {
			Some(RefKind::Model) => {
				let model = some!(interner().get(cursor_value.as_str()));
				self.model_references(&model.into())
			}
			Some(RefKind::Ref(_)) | Some(RefKind::Id) => self.record_references(&cursor_value, current_module),
			Some(RefKind::TInherit) | Some(RefKind::TCall) => self.template_references(&cursor_value, true),
			Some(RefKind::TName) => self.template_references(&cursor_value, false),
			Some(RefKind::FieldName) | None => Ok(None),
		}
	}
	pub async fn xml_hover(&self, params: HoverParams, rope: Rope) -> miette::Result<Option<Hover>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
		} = gather_refs(cursor_by_char, &mut reader, interner())?;

		let Some(ref_at_cursor) = ref_at_cursor else {
			return Ok(None);
		};

		let lsp_range = offset_range_to_lsp_range(ref_at_cursor.range().map_unit(ByteOffset), rope.clone());
		match ref_kind {
			Some(RefKind::Model) => self.hover_model(&ref_at_cursor, lsp_range, false, None),
			Some(RefKind::Ref(_)) => {
				if ref_at_cursor.contains('.') {
					self.hover_record(&ref_at_cursor, lsp_range)
				} else {
					let current_module = self
						.index
						.module_of_path(Path::new(params.text_document_position_params.text_document.uri.path()));
					let current_module = some!(current_module);
					let xml_id = format!("{}.{}", interner().resolve(&current_module), ref_at_cursor);
					self.hover_record(&xml_id, lsp_range)
				}
			}
			Some(RefKind::FieldName) => {
				let model = some!(model_filter);
				self.hover_field_name(&ref_at_cursor, &model, lsp_range).await
			}
			Some(RefKind::TInherit) | Some(RefKind::TCall) | Some(RefKind::TName) | Some(RefKind::Id) | None => {
				Ok(None)
			}
		}
	}
}

struct XmlRefs<'a> {
	ref_at_cursor: Option<StrSpan<'a>>,
	ref_kind: Option<RefKind>,
	model_filter: Option<String>,
}

fn gather_refs<'read>(
	offset_at_cursor: ByteOffset,
	reader: &mut Tokenizer<'read>,
	interner: &ThreadedRodeo,
) -> miette::Result<XmlRefs<'read>> {
	let mut tag = None;
	let mut ref_at_cursor = None;
	let mut ref_kind = None;
	let mut model_filter = None;
	let offset_at_cursor = offset_at_cursor.0;
	for token in reader {
		match token {
			Ok(Token::ElementStart { local, .. }) => match local.as_str() {
				"field" => tag = Some(Tag::Field),
				"template" => {
					tag = Some(Tag::Template);
					model_filter = Some("ir.ui.view".to_string());
				}
				"record" => tag = Some(Tag::Record),
				"menuitem" => tag = Some(Tag::Menuitem),
				_ => {}
			},
			// <field name=.. ref=.. />
			Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Field)) => {
				let value_in_range = value.range().contains_end(offset_at_cursor);
				if local.as_str() == "ref" && value_in_range {
					ref_at_cursor = Some(value);
				} else if local.as_str() == "name" && value_in_range {
					ref_at_cursor = Some(value);
					ref_kind = Some(RefKind::FieldName);
				} else if local.as_str() == "name" {
					let relation = interner.get_or_intern(value.as_str());
					ref_kind = Some(RefKind::Ref(relation));
				}
			}
			// <template inherit_id=.. />
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Template))
					&& value.range().contains_end(offset_at_cursor)
					&& matches!(local.as_str(), "inherit_id" | "t-call") =>
			{
				ref_at_cursor = Some(value);
				ref_kind = Some(RefKind::Ref(interner.get_or_intern_static("inherit_id")));
			}
			// <record model=.. />
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Record)) && local.as_str() == "model" =>
			{
				if value.range().contains_end(offset_at_cursor) {
					ref_at_cursor = Some(value);
					ref_kind = Some(RefKind::Model);
				} else {
					model_filter = Some(value.as_str().to_string());
				}
			}
			// <record id=.. /> or <template id=.. />
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Record | Tag::Template))
					&& local == "id" && value.range().contains(&offset_at_cursor) =>
			{
				ref_at_cursor = Some(value);
				ref_kind = Some(RefKind::Id);
			}
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Menuitem)) && value.range().contains_end(offset_at_cursor) =>
			{
				match local.as_str() {
					"id" => {
						ref_at_cursor = Some(value);
						ref_kind = Some(RefKind::Id);
					}
					"parent" => {
						ref_at_cursor = Some(value);
						ref_kind = Some(RefKind::Ref(interner.get_or_intern_static("parent_id")));
						model_filter = Some("ir.ui.menu".to_string());
					}
					"action" => {
						ref_at_cursor = Some(value);
						ref_kind = Some(RefKind::Ref(interner.get_or_intern_static("action")));
						model_filter = Some("ir.ui.menu".to_string());
					}
					_ => {}
				}
			}
			// leftover cases
			Ok(Token::Attribute { local, value, .. }) if value.range().contains_end(offset_at_cursor) => {
				match local.as_str() {
					"t-name" => {
						ref_at_cursor = Some(value);
						ref_kind = Some(RefKind::TName);
					}
					"t-inherit" => {
						ref_at_cursor = Some(value);
						ref_kind = Some(RefKind::TInherit);
					}
					"t-call" => {
						ref_at_cursor = Some(value);
						ref_kind = Some(RefKind::TCall);
					}
					_ => {}
				}
			}
			Ok(Token::ElementEnd { .. }) if ref_at_cursor.is_some() => break,
			Err(_) => break,
			Ok(token) => {
				if token_span(&token).start() > offset_at_cursor {
					break;
				}
			}
		}
	}
	Ok(XmlRefs {
		ref_at_cursor,
		ref_kind,
		model_filter,
	})
}
