//! XML [records][Record], as well as [`template`][Record::template] and [`menuitem`][Record::menuitem] shorthands.

use tower_lsp_server::ls_types::*;
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::prelude::*;

use crate::index::{_I, _R, ModuleName, PathSymbol, RecordId};
use crate::model::ModelName;
use crate::{ImStr, errloc, some};

#[rustfmt::skip]
query! {
	/// Matches `{'xml_id': '<module>.<name>', ...}` dict entries inside the `eval`
	/// expression of `ir.model.data._update_xmlids` function calls.
	UpdateXmlIds(XmlId);
((pair
  (string (string_content) @_key)
  (string (string_content) @XML_ID))
  (#eq? @_key "xml_id"))
}

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
#[derive(Debug)]
pub enum RecordMetadata {
	View(ModelName),
	/// `<record model="ir.config_parameter"><field name="key">..</field></record>`
	ConfigParameterKey {
		key: ImStr,
		location: MinLoc,
	},
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
						} else if model.is_some_and(|model: ModelName| _R(*model) == "ir.config_parameter")
							&& let Some((key, key_range)) = extract_key_text(reader)
							&& !key.is_empty()
						{
							metadata = Some(RecordMetadata::ConfigParameterKey {
								key: key.into(),
								location: MinLoc {
									path,
									range: rope_conv(key_range.map_unit(ByteOffset), rope),
								},
							});
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
	/// Parses a `<function model="ir.model.data" name="_update_xmlids">` element, which
	/// programmatically creates XML IDs via a list of `{'xml_id': .., 'record': .., ..}` dicts
	/// passed to a `<value eval=".."/>` child. The reader must be positioned just after the
	/// `<function>` [`Token::ElementStart`]; it is consumed up to and including the matching
	/// `</function>`.
	pub fn from_update_xmlids(
		module: ModuleName,
		path: PathSymbol,
		reader: &mut Tokenizer,
		rope: RopeSlice<'_>,
	) -> Vec<(Self, Option<RecordMetadata>)> {
		let mut records = vec![];
		let mut is_update_xmlids = false;
		// whether the `<function>` open tag has been closed (i.e. we are now reading children)
		let mut function_open = false;
		// whether the current element whose attributes we are reading is a `<value>`
		let mut in_value = false;
		// nesting depth of `<function>` elements
		let mut depth = 1;

		loop {
			match reader.next() {
				Some(Ok(Token::Attribute { local, value, .. })) => {
					if !function_open {
						if local.as_str() == "name" && value.as_str() == "_update_xmlids" {
							is_update_xmlids = true;
						}
					} else if in_value && is_update_xmlids && local.as_str() == "eval" {
						parse_update_xmlids(value.as_str(), value.range().start, module, path, rope, &mut records);
					}
				}
				Some(Ok(Token::ElementStart { local, .. })) => {
					if local.as_str() == "function" {
						depth += 1;
					}
					in_value = local.as_str() == "value";
				}
				Some(Ok(Token::ElementEnd { end, .. })) => match end {
					ElementEnd::Open => function_open = true,
					ElementEnd::Empty => {
						if !function_open {
							// self-closing `<function .. />`
							break;
						}
						in_value = false;
					}
					ElementEnd::Close(_, local) => {
						if local.as_str() == "function" {
							depth -= 1;
							if depth <= 0 {
								break;
							}
						}
					}
				},
				None | Some(Err(_)) => break,
				_ => {}
			}
		}

		records
	}
}

/// Extracts each `'xml_id': '<module>.<name>'` entry from the `eval` Python expression `eval`,
/// pushing a [`Record`] for each. `base_offset` is the byte offset of `eval` within the document.
fn parse_update_xmlids(
	eval: &str,
	base_offset: usize,
	module: ModuleName,
	path: PathSymbol,
	rope: RopeSlice<'_>,
	out: &mut Vec<(Record, Option<RecordMetadata>)>,
) {
	let mut parser = Parser::new();
	if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
		return;
	}
	let Some(ast) = parser.parse(eval, None) else {
		return;
	};
	let mut cursor = QueryCursor::new();
	let mut captures = cursor.captures(UpdateXmlIds::query(), ast.root_node(), eval.as_bytes());
	while let Some((match_, idx)) = captures.next() {
		let capture = match_.captures[*idx];
		let Some(UpdateXmlIds::XmlId) = UpdateXmlIds::from(capture.index) else {
			continue;
		};
		let byte_range = capture.node.byte_range();
		let xml_id = &eval[byte_range.clone()];
		let (module, id) = match xml_id.split_once('.') {
			Some((prefix, id)) => (_I(prefix).into(), id.into()),
			None => (module, xml_id.into()),
		};
		let range = rope_conv(byte_range.map_unit(|unit| ByteOffset(unit + base_offset)), rope);
		out.push((
			Record {
				deleted: false,
				id,
				module,
				model: None,
				inherit_id: None,
				location: MinLoc { path, range },
			},
			None,
		));
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

fn extract_key_text<'text>(reader: &Tokenizer<'text>) -> Option<(&'text str, core::ops::Range<usize>)> {
	let mut reader = reader.clone();
	let mut is_key_field = false;
	loop {
		match reader.next() {
			Some(Ok(Token::Attribute { local, value, .. })) if local.as_str() == "name" && value.as_str() == "key" => {
				is_key_field = true;
			}
			Some(Ok(Token::Text { text })) if is_key_field => {
				return Some((text.as_str(), text.range()));
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

	use crate::record::{extract_key_text, extract_model_text};

	#[test]
	fn test_extract_model_text() {
		let reader = Tokenizer::from(
			r#"
				<field name="model">blah</field>
			"#,
		);
		assert_eq!(extract_model_text(&reader), Some("blah"));
	}

	#[test]
	fn test_extract_key_text() {
		let contents = r#"
				<field name="key">some.key</field>
			"#;
		let reader = Tokenizer::from(contents);
		let (text, range) = extract_key_text(&reader).unwrap();
		assert_eq!(text, "some.key");
		assert_eq!(&contents[range], "some.key");

		let reader = Tokenizer::from(
			r#"
				<field name="value">blah</field>
			"#,
		);
		assert_eq!(extract_key_text(&reader), None);
	}

	#[test]
	fn test_from_update_xmlids() {
		use crate::index::{_I, _R, PathSymbol};
		use ropey::Rope;
		use xmlparser::Token;

		let xml = r#"<function model="ir.model.data" name="_update_xmlids">
	<value model="base" eval="[{
		'xml_id': 'stock.stock_location_stock',
		'record': obj().env.ref('stock.warehouse0').lot_stock_id,
		'noupdate': True,
	}, {
		'xml_id': 'stock.picking_type_internal',
		'record': obj().env.ref('stock.warehouse0').int_type_id,
		'noupdate': True,
	}]"/>
</function>"#;
		let rope = Rope::from_str(xml);
		let module = _I("stock").into();
		let path = PathSymbol::strip_root(_I("/fake"), std::path::Path::new("/fake/stock/data/stock_data.xml"));

		let mut reader = Tokenizer::from(xml);
		// advance to just past the <function> ElementStart, mirroring the caller
		for token in reader.by_ref() {
			if matches!(token, Ok(Token::ElementStart { local, .. }) if local.as_str() == "function") {
				break;
			}
		}

		let records = super::Record::from_update_xmlids(module, path, &mut reader, rope.slice(..));
		assert_eq!(records.len(), 2, "expected two records, got {records:?}");

		let qualified: Vec<_> = records.iter().map(|(r, _)| r.qualified_id()).collect();
		assert_eq!(
			qualified,
			vec!["stock.stock_location_stock", "stock.picking_type_internal"]
		);

		// module prefix must be split off from the current module
		assert_eq!(_R(records[0].0.module), "stock");
		assert_eq!(records[0].0.id.as_str(), "stock_location_stock");

		// location must point at the xml_id string content within the eval
		let range = records[0].0.location.range;
		let start: crate::utils::ByteOffset = crate::utils::rope_conv(range.start, rope.slice(..));
		let end: crate::utils::ByteOffset = crate::utils::rope_conv(range.end, rope.slice(..));
		assert_eq!(&xml[start.0..end.0], "stock.stock_location_stock");
	}
}
