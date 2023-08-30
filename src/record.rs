use lasso::ThreadedRodeo;
use miette::diagnostic;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::index::ModuleName;
use crate::utils::MinLoc;
use crate::utils::{char_to_position, position_to_char, CharOffset};
use crate::ImStr;

#[macro_export]
macro_rules! unwrap_or_none {
	($opt:expr) => {
		match $opt {
			Some(it) => it,
			None => return Ok(None),
		}
	};
}

#[derive(Debug)]
pub struct Record {
	pub deleted: bool,
	pub id: ImStr,
	pub module: ModuleName,
	pub model: Option<ImStr>,
	/// (inherit_module?, xml_id)
	pub inherit_id: Option<(Option<ImStr>, ImStr)>,
	pub location: MinLoc,
}

impl Record {
	pub fn qualified_id(&self, interner: &ThreadedRodeo) -> String {
		format!("{}.{}", interner.resolve(&self.module), self.id)
	}
	pub fn from_reader(
		offset: CharOffset,
		module: ModuleName,
		path: ImStr,
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
		let start =
			char_to_position(offset, rope.clone()).ok_or_else(|| diagnostic!("Failed to parse start location"))?;

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
					"model" => model = Some(value.to_string().into()),
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
									maybe_inherit_id = Some(value.as_str().to_string());
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
							inherit_id = Some((Some(module.to_string().into()), xml_id.into()));
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
						end = Some(CharOffset(span.end()));
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
						end = Some(CharOffset(span.end()));
						break;
					}
				}
				// None | Some(Err(_)) => break,
				None => break,
				Some(Err(err)) => {
					let pos = Position {
						line: err.pos().row,
						character: err.pos().col,
					};
					end = position_to_char(pos, rope.clone());
					break;
				}
				_ => {}
			}
		}
		let id = unwrap_or_none!(id);
		let end = end.ok_or_else(|| diagnostic!("Unbound range for record"))?;
		let end = char_to_position(end, rope.clone()).ok_or_else(|| diagnostic!("Failed to parse end location"))?;
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
		offset: CharOffset,
		module: ModuleName,
		path: ImStr,
		reader: &mut Tokenizer,
		rope: Rope,
	) -> miette::Result<Option<Self>> {
		let start = char_to_position(offset, rope.clone())
			.ok_or_else(|| diagnostic!("(template) Failed to parse start location"))?;
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
							id = Some(xml_id.to_string().into());
						} else {
							id = Some(value.to_string().into());
						}
					}
					b"inherit_id" => match value.split_once('.') {
						Some((module, xml_id)) => inherit_id = Some((Some(module.into()), xml_id.into())),
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
						end = Some(CharOffset(span.end()));
						break;
					}
				}
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Close(_, local),
					span,
					..
				})) if local.as_str() == "template" => {
					stack -= 1;
					if stack <= 0 {
						end = Some(CharOffset(span.end()));
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
						line: err.pos().row,
						character: err.pos().col,
					};
					end = position_to_char(pos, rope.clone());
					break;
				}
				_ => {}
			}
		}
		let end = end.ok_or_else(|| diagnostic!("Unbound range for template"))?;
		let end = char_to_position(end, rope).ok_or_else(|| diagnostic!("(template) Failed to parse end location"))?;
		let range = Range { start, end };

		Ok(Some(Self {
			id: unwrap_or_none!(id),
			deleted: false,
			model: Some("ir.ui.view".into()),
			module,
			inherit_id,
			location: MinLoc { path, range },
		}))
	}
}
