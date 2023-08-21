use faststr::FastStr;
use miette::{diagnostic, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::utils::{char_offset_to_position, CharOffset};

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
	pub id: FastStr,
	pub module: FastStr,
	pub model: Option<FastStr>,
	/// (inherit_module?, xml_id)
	pub inherit_id: Option<(Option<FastStr>, FastStr)>,
	pub location: Location,
}

impl Record {
	pub fn qualified_id(&self) -> String {
		format!("{}.{}", self.module, self.id)
	}
	pub fn from_reader(
		offset: CharOffset,
		module: FastStr,
		uri: &str,
		reader: &mut Tokenizer,
		rope: &Rope,
	) -> miette::Result<Option<Self>> {
		let mut id = None;
		let mut model = None;
		let mut inherit_id = None;
		let mut end = None;
		let uri = format!("file://{uri}").parse().into_diagnostic()?;
		let start =
			char_offset_to_position(offset.0, rope).ok_or_else(|| diagnostic!("Failed to parse start location"))?;

		loop {
			match reader.next() {
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Close(_, close),
					span,
					..
				})) if close.as_str() == "record" => {
					end = Some(span.end());
					break;
				}
				Some(Ok(Token::Attribute { local, value, .. })) => match local.as_str() {
					"id" => id = Some(value.as_str().to_string().into()),
					"model" => {
						// TODO: Limit which records need to be indexed
						if value.as_str() != "ir.ui.view" {
							return Ok(None);
						}
						model = Some(value.as_str().to_string().into())
					}
					_ => {}
				},
				Some(Ok(Token::ElementStart { local, .. })) if local.as_str() == "field" => {
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
							Some(Ok(Token::ElementEnd {
								end: ElementEnd::Empty, ..
							})) => break,
							Some(Ok(Token::ElementEnd {
								end: ElementEnd::Close(_, local),
								..
							})) if local.as_str() == "field" => break,
							None | Some(Err(_)) => break,
							_ => {}
						}
					}
					if !is_inherit_id {
						continue;
					}
					let Some(maybe_inherit_id) = maybe_inherit_id else {
						continue;
					};
					if let Some((module, xml_id)) = maybe_inherit_id.split_once('.') {
						inherit_id = Some((Some(module.to_string().into()), xml_id.to_string().into()));
					} else {
						inherit_id = Some((None, maybe_inherit_id.into()));
					}
				}
				None | Some(Err(_)) => break,
				_ => {}
			}
		}
		let id = unwrap_or_none!(id);
		let end = end.ok_or_else(|| diagnostic!("Unbound range for record"))?;
		let end = char_offset_to_position(end, rope).ok_or_else(|| diagnostic!("Failed to parse end location"))?;
		let range = Range { start, end };

		Ok(Some(Self {
			deleted: false,
			id,
			module,
			model,
			inherit_id,
			location: Location { uri, range },
		}))
	}
	pub fn template(
		offset: CharOffset,
		module: FastStr,
		uri: &str,
		reader: &mut Tokenizer,
		rope: &Rope,
	) -> miette::Result<Option<Self>> {
		let uri: Url = format!("file://{uri}").parse().into_diagnostic()?;
		let start = char_offset_to_position(offset.0, rope)
			.ok_or_else(|| diagnostic!("(template) Failed to parse start location"))?;
		let mut id = None;
		let mut inherit_id = None;
		let mut end = None;
		// <template /> is a valid HTML tag, so we need to account for nesting.
		let mut stack = 0;
		let mut in_template = true;

		loop {
			match reader.next() {
				Some(Ok(Token::Attribute { local, value, .. })) if in_template => match local.as_bytes() {
					b"id" => id = Some(value.as_str().to_string().into()),
					b"inherit_id" => match value.as_str().split_once('.') {
						Some((module, xml_id)) => {
							inherit_id = Some((Some(module.to_string().into()), xml_id.to_string().into()))
						}
						None => inherit_id = Some((None, value.to_string().into())),
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
						end = Some(span.end());
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
						end = Some(span.end());
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
					eprintln!("error parsing template {}:\n{err}", uri.path());
					break;
				}
				_ => {}
			}
		}
		let end = end.ok_or_else(|| diagnostic!("Unbound range for template"))?;
		let end =
			char_offset_to_position(end, rope).ok_or_else(|| diagnostic!("(template) Failed to parse end location"))?;
		let range = Range { start, end };

		Ok(Some(Self {
			id: unwrap_or_none!(id),
			deleted: false,
			model: Some("ir.ui.view".into()),
			module,
			inherit_id,
			location: Location { uri, range },
		}))
	}
}
