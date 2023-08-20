use faststr::FastStr;
use miette::{diagnostic, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::utils::{char_offset_to_position, CharOffset};

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
	pub fn from_reader<S>(
		offset: CharOffset,
		module: S,
		uri: &str,
		reader: &mut Tokenizer,
		rope: &Rope,
	) -> miette::Result<Option<Self>>
	where
		S: Into<FastStr>,
	{
		let mut id = None;
		let mut model = None;
		let start =
			char_offset_to_position(offset.0, rope).ok_or_else(|| diagnostic!("Failed to parse start location"))?;
		let mut inherit_id = None;
		let mut end = None;

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
		let end = end.ok_or_else(|| diagnostic!("Unbound range for record"))?;
		let end = char_offset_to_position(end, rope).ok_or_else(|| diagnostic!("Failed to parse end location"))?;

		Ok(Some(Self {
			deleted: false,
			id: id.ok_or_else(|| diagnostic!("record without `id`"))?,
			module: module.into(),
			model,
			inherit_id,
			location: Location {
				uri: format!("file://{uri}").parse().into_diagnostic()?,
				range: Range { start, end },
			},
		}))
	}
}
