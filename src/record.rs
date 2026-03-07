//! XML [records][Record], as well as [`template`][Record::template] and [`menuitem`][Record::menuitem] shorthands.

use tower_lsp_server::ls_types::*;
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::prelude::*;

use crate::index::{_I, _R, ModuleName, PathSymbol, RecordId};
use crate::model::ModelName;
use crate::{ImStr, errloc, some};

#[derive(Debug)]
pub struct Record {
	pub deleted: bool,
	pub id: ImStr,
	pub module: ModuleName,
	pub model: Option<ModelName>,
	pub inherit_id: Option<RecordId>,
	pub location: MinLoc,
}

/// Used for data not essential to the record itself, but for other bookkeeping purposes.
pub enum RecordMetadata {
	View(ModelName),
}

impl Record {
	pub fn qualified_id(&self) -> String {
		format!("{}.{}", _R(self.module), self.id)
	}
	pub fn from_reader(
		offset: ByteOffset,
		module: ModuleName,
		path: PathSymbol,
		reader: &mut Tokenizer,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<(Self, Option<RecordMetadata>)>> {
		let mut id = None;
		let mut model = None;
		let mut inherit_id = None;
		let mut end = None;
		// nested records are a thing apparently
		let mut stack = 1;
		let mut in_record = true;
		let mut metadata = None;
		let start: Position = rope_conv(offset, rope);

		loop {
			match reader.next() {
				// TODO: No nested records yet
				Some(Ok(Token::Attribute { local, value, .. })) if stack == 1 && in_record => match local.as_str() {
					"id" => {
						if let Some((_, xml_id)) = value.split_once('.') {
							id = Some(xml_id.into());
						} else {
							id = Some(value.as_str().into());
						}
					}
					"model" => model = Some(_I(value.as_str()).into()),
					_ => {}
				},
				Some(Ok(Token::ElementStart { local, .. })) => {
					in_record = local.as_str() == "record";
					if in_record {
						stack += 1;
					}
					if local.as_str() == "field" {
						if let Some(maybe_inherit_id) = extract_inherit_id(reader, stack) {
							if maybe_inherit_id.contains('.') {
								inherit_id = Some(_I(maybe_inherit_id).into());
							} else {
								inherit_id = Some(_I(format!("{}.{maybe_inherit_id}", _R(module))).into());
							}
						} else if
						// model.is_some_and(|model| _R(model) == "ir.ui.view") &&
						let Some(viewmodel) = extract_model_text(reader) {
							metadata = Some(RecordMetadata::View(_I(viewmodel).into()));
						}
					}
				}
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Close(_, local),
					span,
					..
				})) if local.as_str() == "record" => {
					stack -= 1;
					if stack <= 0 {
						end = Some(ByteOffset(span.end()));
						break;
					}
				}
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Empty,
					span,
					..
				})) if in_record => {
					stack -= 1;
					if stack <= 0 {
						end = Some(ByteOffset(span.end()));
						break;
					}
				}
				None => break,
				Some(Err(err)) => {
					let pos = Position {
						line: err.pos().row - 1,
						character: err.pos().col - 1,
					};
					end = Some(rope_conv(pos, rope));
					break;
				}
				_ => {}
			}
		}
		let id = some!(id);
		let end = end.ok_or_else(|| errloc!("Unbound range for record"))?;
		let end = rope_conv(end, rope);
		let range = Range { start, end };

		Ok(Some((
			Self {
				deleted: false,
				id,
				module,
				model,
				inherit_id,
				location: MinLoc { path, range },
			},
			metadata,
		)))
	}
	pub fn template(
		offset: ByteOffset,
		module: ModuleName,
		path: PathSymbol,
		reader: &mut Tokenizer,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<(Self, Option<RecordMetadata>)>> {
		let start: Position = rope_conv(offset, rope);
		let mut id = None;
		let mut inherit_id = None;
		let mut end = None;
		// <template /> is a valid HTML tag, so we need to account for nesting.
		let mut stack = 1;
		let mut in_template = true;

		loop {
			match reader.next() {
				Some(Ok(Token::Attribute { local, value, .. })) if in_template => match local.as_str() {
					"id" => {
						if let Some((_, xml_id)) = value.split_once('.') {
							id = Some(xml_id.into());
						} else {
							id = Some(value.as_str().into());
						}
					}
					"inherit_id" => {
						if value.contains('.') {
							inherit_id = Some(_I(value.as_str()).into());
						} else {
							inherit_id = Some(_I(format!("{}.{value}", _R(module))).into())
						}
					}
					_ => {}
				},
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Empty,
					span,
					..
				})) if in_template => {
					stack -= 1;
					if stack <= 0 {
						end = Some(ByteOffset(span.end()));
						break;
					}
				}
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Open, ..
				})) if in_template && stack == 1 && id.is_none() => {
					return Ok(None);
				}
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Close(_, local),
					span,
					..
				})) if local.as_str() == "template" => {
					stack -= 1;
					if stack <= 0 {
						end = Some(ByteOffset(span.end()));
						break;
					}
				}
				Some(Ok(Token::ElementStart { local, .. })) => {
					in_template = local.as_str() == "template";
					if in_template {
						stack += 1
					}
				}
				None => break,
				Some(Err(err)) => {
					let pos = Position {
						line: err.pos().row - 1,
						character: err.pos().col - 1,
					};
					end = Some(rope_conv(pos, rope));
					break;
				}
				_ => {}
			}
		}
		let end = end.ok_or_else(|| errloc!("Unbound range for template"))?;
		let end = rope_conv(end, rope);
		let range = Range { start, end };

		Ok(Some((
			Self {
				id: some!(id),
				deleted: false,
				model: Some(_I("ir.ui.view").into()),
				module,
				inherit_id,
				location: MinLoc { path, range },
			},
			None,
		)))
	}
	pub fn menuitem(
		offset: ByteOffset,
		module: ModuleName,
		path: PathSymbol,
		reader: &mut Tokenizer,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<(Self, Option<RecordMetadata>)>> {
		let mut id = None;
		let mut end = None;
		let start = rope_conv(offset, rope);

		loop {
			match reader.next() {
				Some(Ok(Token::Attribute { local, value, .. })) if local.as_str() == "id" => {
					if let Some((_, xml_id)) = value.split_once('.') {
						id = Some(xml_id.into());
					} else {
						id = Some(value.as_str().into());
					}
				}
				Some(Ok(Token::ElementEnd { span, .. })) => {
					end = Some(ByteOffset(span.end()));
					break;
				}
				None => break,
				Some(Err(err)) => {
					let pos = Position {
						line: err.pos().row - 1,
						character: err.pos().col - 1,
					};
					end = Some(rope_conv(pos, rope));
					break;
				}
				_ => {}
			}
		}

		let id = some!(id);
		let end = end.ok_or_else(|| errloc!("Unbound range for menuitem"))?;
		let end = rope_conv(end, rope);
		let range = Range { start, end };

		Ok(Some((
			Self {
				id,
				deleted: false,
				model: Some(_I("ir.ui.menu").into()),
				module,
				inherit_id: None,
				location: MinLoc { path, range },
			},
			None,
		)))
	}
}

fn extract_inherit_id<'text>(reader: &Tokenizer<'text>, stack: i32) -> Option<&'text str> {
	let mut reader = reader.clone();
	let mut is_inherit_id = false;
	let mut maybe_inherit_id = None;
	loop {
		match reader.next() {
			Some(Ok(Token::Attribute { local, value, .. }))
				if local.as_str() == "name" && value.as_str() == "inherit_id" =>
			{
				is_inherit_id = true
			}
			Some(Ok(Token::Attribute { local, value, .. })) if local.as_str() == "ref" => {
				maybe_inherit_id = Some(value.as_str());
			}
			Some(Ok(Token::ElementEnd { .. })) => break,
			None | Some(Err(_)) => break,
			_ => {}
		}
	}
	if !is_inherit_id || stack > 1 {
		return None;
	}
	maybe_inherit_id
}

fn extract_model_text<'text>(reader: &Tokenizer<'text>) -> Option<&'text str> {
	let mut reader = reader.clone();
	let mut is_model_field = false;
	loop {
		match reader.next() {
			Some(Ok(Token::Attribute { local, value, .. }))
				if local.as_str() == "name" && value.as_str() == "model" =>
			{
				is_model_field = true;
			}
			Some(Ok(Token::Text { text })) if is_model_field => {
				return Some(text.as_str());
			}
			Some(Ok(Token::ElementEnd {
				end: ElementEnd::Close(..) | ElementEnd::Empty,
				..
			})) => break,
			None | Some(Err(_)) => break,
			_ => {}
		}
	}
	None
}

#[cfg(test)]
mod tests {
	use pretty_assertions::assert_eq;
	use xmlparser::Tokenizer;

	use crate::record::extract_model_text;

	#[test]
	fn test_extract_model_text() {
		let reader = Tokenizer::from(
			r#"
				<field name="model">blah</field>
			"#,
		);
		assert_eq!(extract_model_text(&reader), Some("blah"));
	}
}
