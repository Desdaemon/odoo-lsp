//! XML [records][Record], as well as [`template`][Record::template] and [`menuitem`][Record::menuitem] shorthands.

use tower_lsp_server::lsp_types::*;
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
	) -> anyhow::Result<Option<Self>> {
		let mut id = None;
		let mut model = None;
		let mut inherit_id = None;
		let mut end = None;
		// nested records are a thing apparently
		let mut stack = 1;
		let mut in_record = true;
		let start: Position = ok!(rope_conv(offset, rope), "{}", path);

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
						let Some(maybe_inherit_id) = extract_inherit_id(reader, stack) else {
							continue;
						};
						if maybe_inherit_id.contains('.') {
							inherit_id = Some(_I(maybe_inherit_id).into());
						} else {
							inherit_id = Some(_I(format!("{}.{maybe_inherit_id}", _R(module))).into());
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
					end = rope_conv(pos, rope).ok();
					break;
				}
				_ => {}
			}
		}
		let id = some!(id);
		let end = end.ok_or_else(|| errloc!("Unbound range for record"))?;
		let end = ok!(rope_conv(end, rope), "{}", path);
		let range = Range { start, end };

		Ok(Some(Self {
			deleted: false,
			id,
			module,
			model,
			inherit_id,
			location: MinLoc { path, range },
		}))
	}
	pub fn template(
		offset: ByteOffset,
		module: ModuleName,
		path: PathSymbol,
		reader: &mut Tokenizer,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Self>> {
		let start: Position = ok!(rope_conv(offset, rope), "{}", path);
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
					end = rope_conv(pos, rope).ok();
					break;
				}
				_ => {}
			}
		}
		let end = end.ok_or_else(|| errloc!("Unbound range for template"))?;
		let end = ok!(rope_conv(end, rope), "{}", path);
		let range = Range { start, end };

		Ok(Some(Self {
			id: some!(id),
			deleted: false,
			model: Some(_I("ir.ui.view").into()),
			module,
			inherit_id,
			location: MinLoc { path, range },
		}))
	}
	pub fn menuitem(
		offset: ByteOffset,
		module: ModuleName,
		path: PathSymbol,
		reader: &mut Tokenizer,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Self>> {
		let mut id = None;
		let mut end = None;
		let start = ok!(rope_conv(offset, rope), "{}", path);

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
					end = rope_conv(pos, rope).ok();
					break;
				}
				_ => {}
			}
		}

		let id = some!(id);
		let end = end.ok_or_else(|| errloc!("Unbound range for menuitem"))?;
		let end = ok!(rope_conv(end, rope), "{}", path);
		let range = Range { start, end };

		Ok(Some(Self {
			id,
			deleted: false,
			model: Some(_I("ir.ui.menu").into()),
			module,
			inherit_id: None,
			location: MinLoc { path, range },
		}))
	}
}

fn extract_inherit_id<'text>(reader: &mut Tokenizer<'text>, stack: i32) -> Option<&'text str> {
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
