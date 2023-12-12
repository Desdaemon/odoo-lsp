use lasso::{Spur, ThreadedRodeo};
use miette::diagnostic;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::index::{interner, ModuleName};
use crate::model::ModelName;
use crate::utils::{offset_to_position, position_to_offset};
use crate::utils::{ByteOffset, MinLoc};
use crate::{some, ImStr};

#[derive(Debug)]
pub struct Record {
	pub deleted: bool,
	pub id: ImStr,
	pub module: ModuleName,
	pub model: Option<ModelName>,
	/// (inherit_module?, xml_id)
	pub inherit_id: Option<(Option<ModelName>, ImStr)>,
	pub location: MinLoc,
}

impl Record {
	pub fn qualified_id(&self, interner: &ThreadedRodeo) -> String {
		format!("{}.{}", interner.resolve(&self.module), self.id)
	}
	pub fn from_reader(
		offset: ByteOffset,
		module: ModuleName,
		path: Spur,
		reader: &mut Tokenizer,
		rope: Rope,
	) -> miette::Result<Option<Self>> {
		let mut id = None;
		let mut model = None;
		let mut inherit_id = None;
		let mut end = None;
		// nested records are a thing apparently
		let mut stack = 1;
		let mut in_record = true;
		let start = offset_to_position(offset, rope.clone())
			.ok_or_else(|| diagnostic!("fail(start): {}", interner().resolve(&path)))?;

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
					"model" => model = Some(interner().get_or_intern(value.as_str()).into()),
					_ => {}
				},
				Some(Ok(Token::ElementStart { local, .. })) => {
					in_record = local.as_str() == "record";
					if in_record {
						stack += 1;
					}
					if local.as_str() == "field" {
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
							continue;
						}
						let Some(maybe_inherit_id) = maybe_inherit_id else {
							continue;
						};
						if let Some((module, xml_id)) = maybe_inherit_id.split_once('.') {
							inherit_id = Some((Some(interner().get_or_intern(module).into()), xml_id.into()));
						} else {
							inherit_id = Some((None, maybe_inherit_id.into()));
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
					end = position_to_offset(pos, &rope);
					break;
				}
				_ => {}
			}
		}
		let id = some!(id);
		let end = end.ok_or_else(|| diagnostic!("Unbound range for record"))?;
		let end = offset_to_position(end, rope.clone())
			.ok_or_else(|| diagnostic!("fail(end): {}", interner().resolve(&path)))?;
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
		path: Spur,
		reader: &mut Tokenizer,
		rope: Rope,
	) -> miette::Result<Option<Self>> {
		let start = offset_to_position(offset, rope.clone())
			.ok_or_else(|| diagnostic!("fail(start,template): {}", interner().resolve(&path)))?;
		let mut id = None;
		let mut inherit_id = None;
		let mut end = None;
		// <template /> is a valid HTML tag, so we need to account for nesting.
		let mut stack = 1;
		let mut in_template = true;

		loop {
			match reader.next() {
				Some(Ok(Token::Attribute { local, value, .. })) if in_template => match local.as_bytes() {
					b"id" => {
						if let Some((_, xml_id)) = value.split_once('.') {
							id = Some(xml_id.into());
						} else {
							id = Some(value.as_str().into());
						}
					}
					b"inherit_id" => match value.split_once('.') {
						Some((module, xml_id)) => {
							inherit_id = Some((Some(interner().get_or_intern(module).into()), xml_id.into()))
						}
						None => inherit_id = Some((None, value.as_str().into())),
					},
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
					end = position_to_offset(pos, &rope);
					break;
				}
				_ => {}
			}
		}
		let end = end.ok_or_else(|| diagnostic!("Unbound range for template"))?;
		let end = offset_to_position(end, rope)
			.ok_or_else(|| diagnostic!("fail(end,template): {}", interner().resolve(&path)))?;
		let range = Range { start, end };

		Ok(Some(Self {
			id: some!(id),
			deleted: false,
			model: Some(interner().get_or_intern_static("ir.ui.view").into()),
			module,
			inherit_id,
			location: MinLoc { path, range },
		}))
	}
	pub fn menuitem(
		offset: ByteOffset,
		module: ModuleName,
		path: Spur,
		reader: &mut Tokenizer,
		rope: Rope,
	) -> miette::Result<Option<Self>> {
		let mut id = None;
		let mut end = None;
		let start = offset_to_position(offset, rope.clone())
			.ok_or_else(|| diagnostic!("fail(start,menuitem): {}", interner().resolve(&path)))?;

		loop {
			match reader.next() {
				Some(Ok(Token::Attribute { local, value, .. })) => match local.as_str() {
					"id" => {
						if let Some((_, xml_id)) = value.split_once('.') {
							id = Some(xml_id.into());
						} else {
							id = Some(value.as_str().into());
						}
					}
					_ => {}
				},
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
					end = position_to_offset(pos, &rope);
					break;
				}
				_ => {}
			}
		}

		let id = some!(id);
		let end = end.ok_or_else(|| diagnostic!("Unbound range for menuitem"))?;
		let end = offset_to_position(end, rope)
			.ok_or_else(|| diagnostic!("fail(end,menuitem): {}", interner().resolve(&path)))?;
		let range = Range { start, end };

		Ok(Some(Self {
			id,
			deleted: false,
			model: Some(interner().get_or_intern_static("ir.ui.menu").into()),
			module,
			inherit_id: None,
			location: MinLoc { path, range },
		}))
	}
}
