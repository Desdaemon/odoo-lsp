use crate::{Backend, Text};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;

use lasso::{Key, Spur, ThreadedRodeo};
use log::debug;
use miette::{diagnostic, IntoDiagnostic};
use odoo_lsp::model::Field;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{StrSpan, Token, Tokenizer};

use odoo_lsp::record::Record;
use odoo_lsp::{utils::*, ImStr};

enum RefKind {
	Ref(Spur),
	Model,
	Id,
	FieldName,
}

enum Tag {
	Field,
	Template,
	Record,
}

impl Backend {
	pub async fn on_change_xml(&self, text: &Text, uri: &Url, rope: Rope, diagnostics: &mut Vec<Diagnostic>) {
		let text = match text {
			Text::Full(full) => Cow::Borrowed(full.as_str()),
			// Assume rope is up to date
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let mut reader = Tokenizer::from(text.as_ref());
		let mut record_ranges = vec![];
		let Some(current_module) = self.module_index.module_of_path(Path::new(uri.path())) else {
			return;
		};
		let current_module = ImStr::from(current_module.as_str());
		let mut record_prefix = self.module_index.records.by_prefix.write().await;
		let path_uri = ImStr::from(uri.path());
		loop {
			match reader.next() {
				Some(Ok(Token::ElementStart { local, span, .. })) => {
					let offset = CharOffset(span.start());
					match local.as_str() {
						"record" => {
							let Ok(Some(record)) = Record::from_reader(
								offset,
								current_module.clone(),
								path_uri.clone(),
								&mut reader,
								rope.clone(),
							) else {
								continue;
							};
							let Some(range) = lsp_range_to_char_range(record.location.range, rope.clone()) else {
								continue;
							};
							record_ranges.push(range);
							self.module_index
								.records
								.insert(record.qualified_id().into(), record, Some(&mut record_prefix))
								.await;
						}
						"template" => {
							let Ok(Some(template)) = Record::template(
								offset,
								current_module.clone(),
								path_uri.clone(),
								&mut reader,
								rope.clone(),
							) else {
								continue;
							};
							let Some(range) = lsp_range_to_char_range(template.location.range, rope.clone()) else {
								continue;
							};
							record_ranges.push(range);
							self.module_index
								.records
								.insert(template.qualified_id().into(), template, Some(&mut record_prefix))
								.await;
						}
						_ => {}
					}
				}
				None => break,
				Some(Err(err)) => {
					let pos = err.pos();
					let pos = Position::new(pos.row - 1, pos.col - 1);
					diagnostics.push(Diagnostic {
						severity: Some(DiagnosticSeverity::WARNING),
						range: Range::new(pos, pos),
						message: format!("could not parse XML:\n{err}"),
						..Default::default()
					});
					// break;
				}
				_ => {}
			}
		}
		self.record_ranges
			.insert(uri.path().to_string(), record_ranges.into_boxed_slice());
	}
	fn record_slice<'rope>(
		&self,
		rope: &'rope Rope,
		uri: &Url,
		position: Position,
	) -> miette::Result<(Cow<'rope, str>, usize, usize)> {
		let ranges = self
			.record_ranges
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build record ranges"))?;
		let cursor = position_to_char(position, rope.clone()).ok_or_else(|| diagnostic!("cursor"))?;
		let mut cursor_by_char = rope.try_byte_to_char(cursor.0).into_diagnostic()?;
		let Ok(record) = ranges.value().binary_search_by(|range| {
			if cursor < range.start {
				Ordering::Greater
			} else if cursor > range.end {
				Ordering::Less
			} else {
				Ordering::Equal
			}
		}) else {
			debug!("(record_slice) fall back to full slice");
			let slice = Cow::from(rope.slice(..));
			return Ok((slice, cursor_by_char, 0));
		};
		let record_range = &ranges.value()[record];
		let relative_offset = rope.try_byte_to_char(record_range.start.0).into_diagnostic()?;
		cursor_by_char = cursor_by_char.saturating_sub(relative_offset);

		let slice = rope.byte_slice(record_range.clone().map_unit(|unit| unit.0));
		let slice = Cow::from(slice);

		Ok((slice, cursor_by_char, relative_offset))
	}
	pub async fn xml_completions(
		&self,
		params: CompletionParams,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let (slice, cursor_by_char, relative_offset) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);

		let current_module = self
			.module_index
			.module_of_path(Path::new(uri.path()))
			.expect("must be in a module");

		let mut items = vec![];
		let (_, cursor_value, ref_kind, model_filter) =
			gather_refs(cursor_by_char, &mut reader, &self.module_index.interner)?;
		let (Some(value), Some(record_field)) = (cursor_value, ref_kind) else {
			return Ok(None);
		};
		let needle = &value.as_str()[..cursor_by_char - value.range().start];
		let replace_range = value.range().map_unit(|unit| CharOffset(unit + relative_offset));
		match record_field {
			RefKind::Ref(relation) => {
				let Some(model) = model_filter else { return Ok(None) };
				let Some(mut entry) = self.module_index.models.get_mut(model.as_str()) else {
					return Ok(None);
				};
				// TODO: Extract into method
				let fields = if let Some(fields) = &entry.fields {
					fields
				} else {
					let range = char_range_to_lsp_range(replace_range.clone(), rope.clone()).unwrap();
					let fields = self.populate_field_names(&entry, None, range).await?;
					entry.fields.insert(fields)
				};
				let Some(Field::Relational(relation)) = dbg!(fields.get(dbg!(relation).into_usize() as u64)) else {
					return Ok(None);
				};
				self.complete_xml_id(
					needle,
					replace_range,
					rope.clone(),
					Some(self.module_index.interner.resolve(relation)),
					&current_module,
					&mut items,
				)
				.await?
			}
			RefKind::Model => {
				self.complete_model(needle, replace_range, rope.clone(), &mut items)
					.await?
			}
			RefKind::FieldName => {
				if let Some(model) = model_filter {
					let partial_token = params.partial_result_params.partial_result_token;
					self.complete_field_name(needle, replace_range, model, rope.clone(), partial_token, &mut items)
						.await?;
				}
			}
			RefKind::Id => return Ok(None),
		}

		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: items.len() >= Self::LIMIT,
			items,
		})))
	}
	pub fn xml_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);
		let (_, cursor_value, ref_, _) = gather_refs(cursor_by_char, &mut reader, &self.module_index.interner)?;

		let Some(cursor_value) = cursor_value else {
			return Ok(None);
		};
		match ref_ {
			Some(RefKind::Ref(_)) => self.jump_def_inherit_id(&cursor_value, uri),
			Some(RefKind::Model) => self.jump_def_model(&cursor_value),
			Some(RefKind::Id) | Some(RefKind::FieldName) | None => Ok(None),
		}
	}
	pub fn xml_references(&self, params: ReferenceParams, rope: Rope) -> miette::Result<Option<Vec<Location>>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(&rope, uri, position)?;
		let mut reader = Tokenizer::from(&slice[..]);
		let (_, cursor_value, ref_, _) = gather_refs(cursor_by_char, &mut reader, &self.module_index.interner)?;

		let Some(cursor_value) = cursor_value else {
			return Ok(None);
		};
		let current_module = self
			.module_index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
			.map(|ref_| ref_.to_string());
		match ref_ {
			Some(RefKind::Model) => self.model_references(&cursor_value),
			Some(RefKind::Ref(_)) | Some(RefKind::Id) => {
				self.record_references(&cursor_value, current_module.as_deref())
			}
			Some(RefKind::FieldName) | None => Ok(None),
		}
	}
}

fn gather_refs<'read>(
	cursor_by_char: usize,
	reader: &mut Tokenizer<'read>,
	interner: &ThreadedRodeo,
) -> miette::Result<(Option<Tag>, Option<StrSpan<'read>>, Option<RefKind>, Option<String>)> {
	let mut tag = None;
	let mut cursor_value = None;
	let mut ref_kind = None;
	let mut model_filter = None;
	for token in reader {
		match token {
			Ok(Token::ElementStart { local, .. }) => match local.as_str() {
				"field" => tag = Some(Tag::Field),
				"template" => {
					tag = Some(Tag::Template);
					model_filter = Some("ir.ui.view".to_string());
				}
				"record" => tag = Some(Tag::Record),
				_ => {}
			},
			Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Field)) => {
				let value_in_range = value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char;
				if local.as_str() == "ref" && value_in_range {
					cursor_value = Some(value);
				} else if local.as_str() == "name" && value_in_range {
					cursor_value = Some(value);
					ref_kind = Some(RefKind::FieldName);
				} else if local.as_str() == "name" {
					let relation = interner.get_or_intern(value.as_str());
					ref_kind = Some(RefKind::Ref(relation));
				}
			}
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Template))
					&& local.as_str() == "inherit_id"
					&& value.range().contains(&cursor_by_char) =>
			{
				let inherit_id = interner.get_or_intern("ir.ui.view");
				cursor_value = Some(value);
				ref_kind = Some(RefKind::Ref(inherit_id));
			}
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Record)) && local.as_str() == "model" =>
			{
				if value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char {
					cursor_value = Some(value);
					ref_kind = Some(RefKind::Model);
				} else {
					model_filter = Some(value.as_str().to_string());
				}
			}
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Record | Tag::Template))
					&& local.as_str() == "id"
					&& value.range().contains(&cursor_by_char) =>
			{
				cursor_value = Some(value);
				ref_kind = Some(RefKind::Id);
			}
			Ok(Token::ElementEnd { .. }) if cursor_value.is_some() => break,
			Err(_) => break,
			Ok(token) => {
				if token_span(&token).start() > cursor_by_char {
					break;
				}
			}
		}
	}
	Ok((tag, cursor_value, ref_kind, model_filter))
}
