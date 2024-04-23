use crate::{Backend, Text};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;
use std::sync::Arc;

use lasso::Spur;
use log::{debug, warn};
use miette::diagnostic;
use odoo_lsp::index::{interner, PathSymbol};
use odoo_lsp::model::{Field, FieldKind};
use odoo_lsp::template::gather_templates;
use ropey::{Rope, RopeSlice};
use tower_lsp::lsp_types::*;
use xmlparser::{ElementEnd, Error, StrSpan, StreamError, Token, Tokenizer};

use odoo_lsp::record::Record;
use odoo_lsp::{some, utils::*};

#[derive(Debug)]
enum RefKind<'a> {
	/// `<field name=bar ref=".."/>`
	Ref(&'a str),
	Model,
	Id,
	FieldName,
	TName,
	TInherit,
	TCall,
	/// ref'd value is a prop of this component.
	PropOf(&'a str),
}

enum Tag<'a> {
	Field,
	Template,
	Record,
	Menuitem,
	TComponent(&'a str),
}

impl Backend {
	pub async fn update_xml(
		&self,
		root: Spur,
		text: &Text,
		uri: &Url,
		rope: &Rope,
		did_save: bool,
	) -> miette::Result<()> {
		let text = match text {
			Text::Full(full) => Cow::Borrowed(full.as_str()),
			// Assume rope is up to date
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let mut reader = Tokenizer::from(text.as_ref());
		let mut record_ranges = vec![];
		let interner = interner();
		let current_module = self
			.index
			.module_of_path(Path::new(uri.path()))
			.ok_or_else(|| diagnostic!("module_of_path for {} failed", uri.path()))?;
		let mut record_prefix = if did_save {
			Some(self.index.records.by_prefix.write().await)
		} else {
			None
		};
		let path_uri = PathSymbol::strip_root(root, Path::new(uri.path()));
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
									warn!("{err}");
									continue;
								}
								record_ranges.extend(entries.into_iter().map(|entry| {
									lsp_range_to_offset_range(entry.template.location.unwrap().range, &rope).unwrap()
								}));
								continue;
							}
							Err(err) => {
								warn!(
									target: "on_change_xml",
									"{local} could not be completely parsed: {}\n{err}",
									uri.path()
								);
								continue;
							}
						};
						let Some(range) = lsp_range_to_offset_range(record.location.range, &rope) else {
							debug!("no range for {}", record.id);
							continue;
						};
						record_ranges.push(range);
						if let Some(mut prefix) = record_prefix.as_mut() {
							self.index
								.records
								.insert(
									interner.get_or_intern(record.qualified_id(interner)).into(),
									record,
									Some(&mut prefix),
								)
								.await;
						}
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
	) -> miette::Result<(RopeSlice<'rope>, ByteOffset, usize)> {
		let ranges = self
			.record_ranges
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build record ranges for {}", uri.path()))?;
		let mut offset_at_cursor = position_to_offset(position, rope).ok_or_else(|| diagnostic!("offset_at_cursor"))?;
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
			// let slice = Cow::from(rope.slice(..));
			return Ok((rope.slice(..), offset_at_cursor, 0));
		};
		let record_range = &ranges.value()[record];
		debug!(
			"(record_slice) found {:?}, cursor={:?}",
			record_range.erase(),
			offset_at_cursor
		);
		let relative_offset = record_range.start.0;
		offset_at_cursor.0 = offset_at_cursor.0.saturating_sub(relative_offset);

		let slice = rope.byte_slice(record_range.erase());

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
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());

		let current_module = self
			.index
			.module_of_path(Path::new(uri.path()))
			.expect("must be in a module");

		let mut items = MaxVec::new(Self::LIMIT);
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
			..
		} = gather_refs(offset_at_cursor, &mut reader, &slice)?;
		let (Some((value, value_range)), Some(record_field)) = (ref_at_cursor, ref_kind) else {
			return Ok(None);
		};
		let needle = &value[..offset_at_cursor.0 - value_range.start];
		let replace_range = value_range.map_unit(|unit| ByteOffset(unit + relative_offset));
		match record_field {
			RefKind::Ref(relation) => {
				let model_key = some!(model_filter);
				let model = some!(interner().get(&model_key));
				let fields = some!(self.index.models.populate_field_names(model.into(), &[]));
				let fields = some!(fields.fields.as_ref());
				let relation = some!(interner().get(relation));
				let Some(Field {
					kind: FieldKind::Relational(relation),
					..
				}) = fields.get(&relation.into()).map(Arc::as_ref)
				else {
					return Ok(None);
				};
				self.complete_xml_id(
					needle,
					replace_range,
					rope.clone(),
					Some(interner().resolve(&relation)),
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
			RefKind::PropOf(component) => {
				self.complete_component_prop(/*needle,*/ replace_range, rope.clone(), component, &mut items)?;
			}
			RefKind::Id => {
				self.complete_xml_id(
					needle,
					replace_range,
					rope.clone(),
					model_filter.as_deref(),
					current_module,
					&mut items,
				)
				.await?;
			}
			RefKind::TName => return Ok(None),
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
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
			..
		} = gather_refs(cursor_by_char, &mut reader, &slice)?;

		let Some((cursor_value, _)) = ref_at_cursor else {
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
			Some(RefKind::PropOf(component)) => self.jump_def_component_prop(component, cursor_value),
			Some(RefKind::Id) => self.jump_def_xml_id(&cursor_value, uri),
			None => Ok(None),
		}
	}
	pub fn xml_references(&self, params: ReferenceParams, rope: Rope) -> miette::Result<Option<Vec<Location>>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let (slice, cursor_by_char, _) = self.record_slice(&rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let XmlRefs {
			ref_at_cursor: cursor_value,
			ref_kind,
			..
		} = gather_refs(cursor_by_char, &mut reader, &slice)?;

		let Some((cursor_value, _)) = cursor_value else {
			return Ok(None);
		};
		let current_module = self
			.index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()));
		match ref_kind {
			Some(RefKind::Model) => {
				let model = some!(interner().get(cursor_value));
				self.model_references(&model.into())
			}
			Some(RefKind::Ref(_)) | Some(RefKind::Id) => self.record_references(&cursor_value, current_module),
			Some(RefKind::TInherit) | Some(RefKind::TCall) => self.template_references(&cursor_value, true),
			Some(RefKind::TName) => self.template_references(&cursor_value, false),
			Some(RefKind::FieldName) | Some(RefKind::PropOf(..)) | None => Ok(None),
		}
	}
	pub async fn xml_hover(&self, params: HoverParams, rope: Rope) -> miette::Result<Option<Hover>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let (slice, offset_at_cursor, relative_offset) = self.record_slice(&rope, uri, position)?;
		let slice_str = Cow::from(slice);
		let mut reader = Tokenizer::from(slice_str.as_ref());
		let XmlRefs {
			ref_at_cursor,
			ref_kind,
			model_filter,
		} = gather_refs(offset_at_cursor, &mut reader, &slice)?;

		let Some((ref_at_cursor, ref_range)) = ref_at_cursor else {
			return Ok(None);
		};

		let lsp_range = offset_range_to_lsp_range(
			ref_range.map_unit(|unit| ByteOffset(unit + relative_offset)),
			rope.clone(),
		);
		let current_module = self
			.index
			.module_of_path(Path::new(params.text_document_position_params.text_document.uri.path()));
		match ref_kind {
			Some(RefKind::Model) => self.hover_model(&ref_at_cursor, lsp_range, false, None),
			Some(RefKind::Ref(_)) => {
				if ref_at_cursor.contains('.') {
					self.hover_record(&ref_at_cursor, lsp_range)
				} else {
					let current_module = some!(current_module);
					let xml_id = format!("{}.{}", interner().resolve(&current_module), ref_at_cursor);
					self.hover_record(&xml_id, lsp_range)
				}
			}
			Some(RefKind::FieldName) => {
				let model = some!(model_filter);
				self.hover_field_name(&ref_at_cursor, &model, lsp_range).await
			}
			Some(RefKind::Id) => {
				let current_module = some!(current_module);
				let xml_id = if ref_at_cursor.contains('.') {
					Cow::from(ref_at_cursor)
				} else {
					format!("{}.{}", interner().resolve(&current_module), ref_at_cursor).into()
				};
				self.hover_record(&xml_id, lsp_range)
			}
			Some(RefKind::TInherit)
			| Some(RefKind::TCall) => {
				Ok(self.hover_template(ref_at_cursor, lsp_range))
			}
			| Some(RefKind::TName)
			| Some(RefKind::PropOf(..))  // TODO
			| None => {
				#[cfg(not(debug_assertions))]
				return Ok(None);

				#[cfg(debug_assertions)]
				{
					let contents = format!("{:#?}", (ref_at_cursor, ref_kind, model_filter));
					Ok(Some(Hover {
						contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
							language: "rust".to_string(),
							value: contents,
						})),
						range: lsp_range,
					}))
				}
			}
		}
	}
}

struct XmlRefs<'a> {
	ref_at_cursor: Option<(&'a str, core::ops::Range<usize>)>,
	ref_kind: Option<RefKind<'a>>,
	model_filter: Option<String>,
}

fn determine_csv_xmlid_subgroup<'text>(
	ref_at_cursor: &mut Option<(&'text str, core::ops::Range<usize>)>,
	value: StrSpan<'text>,
	offset_at_cursor: usize,
) {
	let mut start = value.range().start;
	let mut csv_groups = value.as_str();
	if !csv_groups.contains(',') {
		*ref_at_cursor = Some((csv_groups, value.range()));
		return;
	}
	loop {
		let mut last_subgroup = false;
		let subgroup = match csv_groups.split_once(',') {
			Some((subgroup, rest)) => {
				csv_groups = rest;
				subgroup
			}
			None => {
				last_subgroup = true;
				csv_groups
			}
		};
		let range = start..start + subgroup.len();
		if range.contains_end(offset_at_cursor) {
			*ref_at_cursor = Some((subgroup, range));
			break;
		}
		start += subgroup.len() + 1;
		if last_subgroup {
			break;
		}
	}
}

/// `contents` is used for error recovery.
fn gather_refs<'read>(
	offset_at_cursor: ByteOffset,
	reader: &mut Tokenizer<'read>,
	slice: &'read RopeSlice<'read>,
) -> miette::Result<XmlRefs<'read>> {
	let mut tag = None;
	let mut ref_at_cursor = None::<(&str, core::ops::Range<usize>)>;
	let mut ref_kind = None;
	let mut model_filter = None;
	let mut template_mode = false;
	let offset_at_cursor = offset_at_cursor.0;

	let mut arch_model = None;
	// <field name="arch">..</field>
	let mut arch_mode = false;
	let mut arch_depth = 0;
	let mut depth = 0;
	let mut expect_model_string = false;

	for token in reader {
		match token {
			Ok(Token::ElementStart { local, .. }) => {
				expect_model_string = false;
				depth += 1;
				match local.as_str() {
					"field" => tag = Some(Tag::Field),
					"template" => {
						tag = Some(Tag::Template);
						model_filter = Some("ir.ui.view".to_string());
					}
					"record" => tag = Some(Tag::Record),
					"menuitem" => tag = Some(Tag::Menuitem),
					"templates" => {
						template_mode = true;
					}
					component if template_mode && component.starts_with(|c| char::is_ascii_uppercase(&c)) => {
						tag = Some(Tag::TComponent(component));
					}
					_ => {}
				}
			}
			// <field name=.. ref=.. />
			Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Field)) => {
				let value_in_range = value.range().contains_end(offset_at_cursor);
				if local.as_str() == "ref" && value_in_range {
					ref_at_cursor = Some((value.as_str(), value.range()));
				} else if local.as_str() == "name" && value_in_range {
					ref_at_cursor = Some((value.as_str(), value.range()));
					ref_kind = Some(RefKind::FieldName);
				} else if local.as_str() == "name" && value.as_str() == "model" {
					// contents should be a model string like res.partner
					expect_model_string = true;
				} else if local.as_str() == "name" && value.as_str() == "arch" {
					arch_mode = true;
					arch_depth = depth
				} else if local.as_str() == "name" {
					ref_kind = Some(RefKind::Ref(value.as_str()));
				} else if local.as_str() == "groups" && value_in_range {
					ref_kind = Some(RefKind::Id);
					model_filter = Some("res.groups".to_string());
					arch_model = None;
					determine_csv_xmlid_subgroup(&mut ref_at_cursor, value, offset_at_cursor);
				}
			}
			// <template inherit_id=.. />
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Template))
					&& value.range().contains_end(offset_at_cursor)
					&& matches!(local.as_str(), "inherit_id" | "t-call") =>
			{
				ref_at_cursor = Some((value.as_str(), value.range()));
				ref_kind = Some(RefKind::Ref("inherit_id"));
			}
			// <record model=.. />
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Record)) && local.as_str() == "model" =>
			{
				if value.range().contains_end(offset_at_cursor) {
					ref_at_cursor = Some((value.as_str(), value.range()));
					ref_kind = Some(RefKind::Model);
				} else {
					model_filter = Some(value.as_str().to_string());
				}
			}
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Record | Tag::Template | Tag::Menuitem))
					&& local == "id" && value.range().contains_end(offset_at_cursor) =>
			{
				ref_at_cursor = Some((value.as_str(), value.range()));
				ref_kind = Some(RefKind::Id);
				arch_model = None;
				match tag {
					Some(Tag::Template) => {
						model_filter = Some("ir.ui.view".to_string());
					}
					Some(Tag::Menuitem) => {
						model_filter = Some("ir.ui.menu".to_string());
					}
					_ => {}
				}
			}
			// <menuitem parent=.. action=.. />
			Ok(Token::Attribute { local, value, .. })
				if matches!(tag, Some(Tag::Menuitem)) && value.range().contains_end(offset_at_cursor) =>
			{
				match local.as_str() {
					"parent" => {
						ref_at_cursor = Some((value.as_str(), value.range()));
						ref_kind = Some(RefKind::Ref("parent_id"));
						model_filter = Some("ir.ui.menu".to_string());
					}
					"action" => {
						ref_at_cursor = Some((value.as_str(), value.range()));
						ref_kind = Some(RefKind::Ref("action"));
						model_filter = Some("ir.ui.menu".to_string());
					}
					"groups" => {
						ref_kind = Some(RefKind::Id);
						arch_model = None;
						model_filter = Some("res.groups".to_string());
						determine_csv_xmlid_subgroup(&mut ref_at_cursor, value, offset_at_cursor);
					}
					_ => {}
				}
			}
			// <Component prop=.. />
			Ok(Token::Attribute { local, .. })
				if matches!(tag, Some(Tag::TComponent(..))) && local.range().contains_end(offset_at_cursor) =>
			{
				let Some(Tag::TComponent(component)) = tag else {
					unreachable!()
				};
				ref_at_cursor = Some((local.as_str(), local.range()));
				ref_kind = Some(RefKind::PropOf(component));
			}
			// catchall cases
			Ok(Token::Attribute { local, value, .. }) if value.range().contains_end(offset_at_cursor) => {
				match local.as_str() {
					"t-name" => {
						ref_at_cursor = Some((value.as_str(), value.range()));
						ref_kind = Some(RefKind::TName);
					}
					"t-inherit" => {
						ref_at_cursor = Some((value.as_str(), value.range()));
						ref_kind = Some(RefKind::TInherit);
					}
					"t-call" => {
						ref_at_cursor = Some((value.as_str(), value.range()));
						ref_kind = Some(RefKind::TCall);
					}
					"groups" => {
						ref_kind = Some(RefKind::Id);
						model_filter = Some("res.groups".to_string());
						arch_model = None;
						determine_csv_xmlid_subgroup(&mut ref_at_cursor, value, offset_at_cursor);
					}
					_ => {}
				}
			}
			// general bookkeeping
			Ok(Token::Attribute { local, .. }) => {
				if local.as_str() == "t-name" && !template_mode {
					template_mode = true;
				}
			}
			Ok(Token::Text { text }) if expect_model_string => {
				expect_model_string = false;
				if text.range().contains_end(offset_at_cursor) {
					ref_at_cursor = Some((text.as_str(), text.range()));
					ref_kind = Some(RefKind::Model);
				} else {
					arch_model = Some(text);
				}
			}
			Ok(Token::ElementEnd { end, span }) => {
				if let ElementEnd::Close(..) | ElementEnd::Empty = end {
					depth -= 1;
					if depth == arch_depth {
						arch_mode = false;
					}
				}
				if template_mode
					&& matches!(end, ElementEnd::Open | ElementEnd::Empty)
					&& span.start() > offset_at_cursor
					&& slice
						.get_byte(offset_at_cursor)
						.map(|c| c.is_ascii_whitespace())
						.unwrap()
				{
					if let Some(Tag::TComponent(component)) = tag {
						// edge case: completion trigger in the middle of tag start
						ref_at_cursor = Some(("", offset_at_cursor..offset_at_cursor));
						ref_kind = Some(RefKind::PropOf(component));
					}
				}
				if ref_at_cursor.is_some() {
					break;
				}
				if let Some(Tag::TComponent(..)) = &tag {
					_ = tag.take();
				}
				// no need to turn off template mode yet
				// match end {
				// 	ElementEnd::Open => depth += 1,
				// 	ElementEnd::Close(..) => {
				// 		depth = depth.saturating_sub(1);
				// 		if depth <= 1 {
				// 			template_mode = false;
				// 		}
				// 	}
				// 	_ => {}
				// }
			}
			// HACK: The lexer can't deal with naked attributes, so we try to parse them manually.
			Err(Error::InvalidAttribute(StreamError::InvalidChar(_, b'=', mut expected_eq_pos), start_pos)) => {
				let Some(Tag::TComponent(component)) = tag else {
					break;
				};

				// <Component prop />
				// move one place back to get the attribute name
				expected_eq_pos.col = expected_eq_pos.col.saturating_sub(1);
				let start_pos = position_to_offset_slice(xml_position_to_lsp_position(start_pos), &slice).unwrap();
				let expected_eq_pos =
					position_to_offset_slice(xml_position_to_lsp_position(expected_eq_pos), &slice).unwrap();

				// may be an invalid attribute, but no point in checking
				let mut range = (start_pos..expected_eq_pos).erase();
				if !range.contains_end(offset_at_cursor) {
					break;
				}
				let Cow::Borrowed(mut attr) = Cow::from(slice.byte_slice(range.clone())) else {
					panic!("(gather_refs) doesn't support cross-chunked boundaries");
				};
				let trimmed = attr.trim_end_matches(|c: char| !c.is_ascii_alphanumeric());
				if trimmed != attr {
					attr = trimmed;
					range = range.start..range.start + trimmed.len();
				}
				let trimmed = attr.trim_start_matches(|c: char| !c.is_ascii_alphanumeric());
				if trimmed != attr {
					range = range.start + (attr.len() - trimmed.len())..range.end;
					attr = trimmed;
				}

				ref_at_cursor = Some((attr, range));
				ref_kind = Some(RefKind::PropOf(component));
				break;
			}
			Err(_) => break,
			Ok(token) => {
				if token_span(&token).start() > offset_at_cursor {
					break;
				}
			}
		}
	}
	let model_filter = if arch_mode {
		arch_model.map(|span| span.to_string()).or(model_filter)
	} else {
		model_filter
	};
	Ok(XmlRefs {
		ref_at_cursor,
		ref_kind,
		model_filter,
	})
}
