use crate::{Backend, Text};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;

use faststr::FastStr;
use miette::{diagnostic, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{ElementEnd, StrSpan, Token, Tokenizer};

use odoo_lsp::record::Record;
use odoo_lsp::utils::*;

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
		let current_module = FastStr::from(current_module.to_string());
		loop {
			match reader.next() {
				Some(Ok(Token::ElementStart { local, span, .. })) => {
					let offset = CharOffset(span.start());
					match local.as_str() {
						"record" => {
							let Ok(Some(record)) = Record::from_reader(
								offset,
								current_module.clone(),
								uri.path(),
								&mut reader,
								rope.clone(),
							) else {
								continue;
							};
							let Some(range) = lsp_range_to_char_range(record.location.range, rope.clone()) else {
								continue;
							};
							record_ranges.push(range);
							self.module_index.records.insert(record.qualified_id(), record);
						}
						"template" => {
							let Ok(Some(template)) =
								Record::template(offset, current_module.clone(), uri.path(), &mut reader, rope.clone())
							else {
								continue;
							};
							let Some(range) = lsp_range_to_char_range(template.location.range, rope.clone()) else {
								continue;
							};
							record_ranges.push(range);
							self.module_index.records.insert(template.qualified_id(), template);
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
					break;
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
	) -> miette::Result<Option<(Cow<'rope, str>, usize, usize)>> {
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
			eprintln!(
				"Could not find record for cursor={cursor:?} ranges={:?}",
				ranges.value()
			);
			return Ok(None);
		};
		let record_range = &ranges.value()[record];
		let relative_offset = rope.try_byte_to_char(record_range.start.0).into_diagnostic()?;
		cursor_by_char = cursor_by_char.saturating_sub(relative_offset);

		let slice = rope.byte_slice(record_range.clone().map_unit(|unit| unit.0));
		let slice = Cow::from(slice);

		Ok(Some((slice, cursor_by_char, relative_offset)))
	}
	pub async fn xml_completions(
		&self,
		params: CompletionParams,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let Some((slice, cursor_by_char, relative_offset)) = self.record_slice(&rope, uri, position)? else {
			return Ok(None);
		};
		let reader = Tokenizer::from(&slice[..]);

		let current_module = self
			.module_index
			.module_of_path(Path::new(uri.path()))
			.expect("must be in a module");

		enum Tag {
			Record,
			Template,
			Field,
		}

		enum RecordField {
			InheritId,
		}

		let mut items = vec![];
		let mut model_filter = None;
		let mut tag = None::<Tag>;
		let mut record_field = None::<RecordField>;
		let mut cursor_value = None::<StrSpan>;

		for token in reader {
			match token {
				Ok(Token::ElementStart { local, .. }) => match local.as_str() {
					"record" => tag = Some(Tag::Record),
					"template" => tag = Some(Tag::Template),
					"field" => tag = Some(Tag::Field),
					_ => {}
				},
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Record)) && local.as_str() == "model" =>
				{
					model_filter = Some(value.as_str().to_string());
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Field)) && local.as_str() == "name" =>
				{
					match value.as_str() {
						"inherit_id" => record_field = Some(RecordField::InheritId),
						_ => {}
					}
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Field))
						&& local.as_str() == "ref"
						&& (value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char) =>
				{
					cursor_value = Some(value);
				}
				Ok(Token::ElementEnd {
					end: ElementEnd::Empty, ..
				}) if matches!(tag, Some(Tag::Field)) => {
					let (Some(value), Some(record_field)) = (cursor_value, record_field.take()) else {
						continue;
					};
					let needle = &value.as_str()[..cursor_by_char - value.range().start];
					let replace_range = value.range().map_unit(|unit| CharOffset(unit + relative_offset));
					match record_field {
						RecordField::InheritId => self.complete_inherit_id(
							needle,
							replace_range,
							rope.clone(),
							model_filter.as_deref(),
							&current_module,
							&mut items,
						)?,
					}
					break;
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Template))
						&& local.as_str() == "inherit_id"
						&& (value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char) =>
				{
					cursor_value = Some(value);
				}
				Ok(Token::ElementEnd { .. }) if matches!(tag, Some(Tag::Template)) => {
					let Some(value) = cursor_value else { continue };
					let needle = &value.as_str()[..cursor_by_char - value.range().start];
					let replace_range = value.range().map_unit(|unit| CharOffset(unit + relative_offset));
					self.complete_inherit_id(
						needle,
						replace_range,
						rope.clone(),
						model_filter.as_deref(),
						&current_module,
						&mut items,
					)?;
					break;
				}
				Err(err) => {
					eprintln!("bug:\n{err}\nin file:\n{}", slice);
					break;
				}
				_ => {}
			}
		}

		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: items.len() >= Self::LIMIT,
			items,
		})))
	}
	pub async fn xml_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let Some((slice, cursor_by_char, _)) = self.record_slice(&rope, uri, position)? else {
			return Ok(None);
		};
		let reader = Tokenizer::from(&slice[..]);

		enum RecordField {
			InheritId,
		}

		enum Tag {
			Field,
			Template,
		}

		let mut record_field = None::<RecordField>;
		let mut cursor_value = None::<StrSpan>;
		let mut tag = None::<Tag>;

		for token in reader {
			match token {
				Ok(Token::ElementStart { local, .. }) => match local.as_str() {
					"field" => tag = Some(Tag::Field),
					"template" => tag = Some(Tag::Template),
					_ => {}
				},
				Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Field)) => {
					if local.as_str() == "ref" && value.range().contains(&cursor_by_char) {
						cursor_value = Some(value);
					} else if local.as_str() == "name" && value.as_str() == "inherit_id" {
						record_field = Some(RecordField::InheritId);
					}
				}
				Ok(Token::ElementEnd { .. }) if matches!(tag, Some(Tag::Field)) => {
					let Some(cursor_value) = cursor_value else { continue };
					match record_field {
						Some(RecordField::InheritId) => {
							return self.jump_def_inherit_id(&cursor_value, uri);
						}
						None => {}
					}
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Template))
						&& local.as_str() == "inherit_id"
						&& value.range().contains(&cursor_by_char) =>
				{
					cursor_value = Some(value);
				}
				Ok(Token::ElementEnd { .. }) if matches!(tag, Some(Tag::Template)) => {
					let Some(cursor_value) = cursor_value else { continue };
					return self.jump_def_inherit_id(&cursor_value, uri);
				}
				Err(err) => {
					eprintln!("bug:\n{err}\nin file:\n{}", slice);
					break;
				}
				_ => {}
			}
		}

		Ok(None)
	}
}
