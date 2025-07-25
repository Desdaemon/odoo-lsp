use tower_lsp_server::lsp_types::{CompletionList, CompletionResponse};
use tree_sitter::{Node, QueryMatch};

use std::borrow::Cow;
use std::sync::atomic::Ordering::Relaxed;

use tower_lsp_server::{UriExt, lsp_types::*};
use tracing::{debug, warn};
use tree_sitter::Tree;

use crate::prelude::*;

use crate::backend::Backend;
use crate::index::{_G, _I, _R, symbol::Symbol};
use crate::model::{FieldKind, ModelEntry, ModelName, PropertyKind};
use crate::some;
use crate::utils::MaxVec;
use crate::utils::*;
use crate::xml::determine_csv_xmlid_subgroup;

use super::{Mapped, PyCompletions, ThisModel, extract_string_needle_at_offset, top_level_stmt};

impl Backend {
	pub(crate) async fn python_completions(
		&self,
		params: CompletionParams,
		ast: Tree,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<CompletionResponse>> {
		let Ok(ByteOffset(offset)) = rope_conv(params.text_document_position.position, rope) else {
			warn!("invalid position {:?}", params.text_document_position.position);
			return Ok(None);
		};
		let path = some!(params.text_document_position.text_document.uri.to_file_path());
		let Some(current_module) = self.index.find_module_of(&path) else {
			debug!("no current module");
			return Ok(None);
		};
		let mut cursor = tree_sitter::QueryCursor::new();
		let contents = Cow::from(rope);
		let contents = contents.as_bytes();
		let query = PyCompletions::query();
		let completions_limit = self
			.workspaces
			.find_workspace_of(&path, |_, ws| ws.completions.limit)
			.unwrap_or_else(|| self.project_config.completions_limit.load(Relaxed));
		let mut this_model = ThisModel::default();
		// FIXME: This hack is necessary to drop !Send locals before await points.
		let mut early_return = EarlyReturn::<anyhow::Result<_>>::default();
		{
			let root = some!(top_level_stmt(ast.root_node(), offset));
			'match_: for match_ in cursor.matches(query, root, contents) {
				let mut model_filter = None;
				let mut field_descriptors = vec![];
				let mut field_descriptor_in_offset = None;
				let mut field_model = None;

				for capture in match_.captures {
					let range = capture.node.byte_range();
					match PyCompletions::from(capture.index) {
						Some(PyCompletions::Request) => {
							model_filter = Some("ir.ui.view");
						}
						Some(PyCompletions::ForXmlId) => {
							let model = || {
								let model = capture.node.prev_named_sibling()?;
								let model = self.index.model_of_range(
									root,
									model.byte_range().map_unit(ByteOffset),
									contents,
								)?;
								Some(_R(model))
							};
							model_filter = model()
						}
						Some(PyCompletions::XmlId) if range.contains_end(offset) => {
							let range = range.shrink(1);
							let needle = String::from_utf8_lossy(&contents[range.start..offset]);
							let range = range.map_unit(ByteOffset);
							early_return.lift(move || async move {
								let mut items = MaxVec::new(completions_limit);
								self.index.complete_xml_id(
									&needle,
									range,
									rope,
									model_filter.map(|m| vec![ImStr::from(m)]).as_deref(),
									current_module,
									&mut items,
								)?;
								Ok(Some(CompletionResponse::List(CompletionList {
									is_incomplete: !items.has_space(),
									items: items.into_inner(),
								})))
							});
							break 'match_;
						}
						Some(PyCompletions::Model) => {
							if range.contains_end(offset) {
								let (needle, byte_range) = some!(extract_string_needle_at_offset(rope, range, offset));
								let range = ok!(rope_conv(byte_range, rope));
								early_return.lift(move || async move {
									let mut items = MaxVec::new(completions_limit);
									self.index.complete_model(&needle, range, &mut items)?;
									Ok(Some(CompletionResponse::List(CompletionList {
										is_incomplete: !items.has_space(),
										items: items.into_inner(),
									})))
								});
								break 'match_;
							}

							// capture a model for later use
							if match_
								.nodes_for_capture_index(PyCompletions::Prop as _)
								.next()
								.is_none()
							{
								continue;
							}

							if range.end < offset
								&& let Some(field) =
									match_.nodes_for_capture_index(PyCompletions::FieldType as _).next()
							{
								if field_model.is_none()
									&& matches!(&contents[field.byte_range()], b"Many2one" | b"One2many" | b"Many2many")
								{
									field_model = Some(&contents[capture.node.byte_range().shrink(1)]);
								}
							} else {
								this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
							}
						}
						Some(PyCompletions::Mapped) => {
							if range.contains_end(offset) {
								// Check if this is an ERROR node with broken syntax
								tracing::debug!(
									"Mapped capture node kind: {}, range: {:?}, offset: {}",
									capture.node.kind(),
									range,
									offset
								);
								if capture.node.kind() == "ERROR" {
									// This might be a string without a colon in a dictionary
									let error_text = &contents[capture.node.byte_range()];
									if error_text.starts_with(b"'") || error_text.starts_with(b"\"") {
										// Extract the partial text
										let quote_char = error_text[0];
										let end_quote = error_text.iter().rposition(|&b| b == quote_char);
										let needle = if let Some(end) = end_quote {
											String::from_utf8_lossy(&error_text[1..end])
										} else if offset > capture.node.start_byte() + 1 {
											String::from_utf8_lossy(&error_text[1..offset - capture.node.start_byte()])
										} else {
											Cow::Borrowed("")
										};

										// Try to determine the model from context
										// First check if we have a MappedTarget in the match
										let mut field_model: Option<Symbol<ModelEntry>> = None;
										// Try to get the model from MappedTarget
										if let Some(target_node) =
											match_.nodes_for_capture_index(PyCompletions::MappedTarget as _).next()
											&& let Some(model_) = self.index.model_of_range(
												root,
												target_node.byte_range().map_unit(ByteOffset),
												contents,
											) {
											field_model = Some(model_);
										}

										// If we didn't find it from MappedTarget, look for the commandlist pattern in the parent nodes
										if field_model.is_none() {
											let mut current = capture.node;
											while let Some(parent) = current.parent() {
												// Check if we're in a list that's part of a mapped/commandlist
												if parent.kind() == "list" {
													// Try to find the full expression this list is part of
													if let Some(expr_parent) = parent.parent() {
														if expr_parent.kind() == "call"
															|| expr_parent.kind() == "attribute"
														{
															break;
														}
													}
												}

												if parent.kind() == "dictionary" {
													// Found the dictionary, now look for the field assignment
													if let Some(list_parent) = parent.parent()
														&& list_parent.kind() == "list" && let Some(pair_parent) =
														list_parent.parent() && pair_parent.kind() == "pair"
														&& let Some(key) = pair_parent.child_by_field_name("key")
														&& key.kind() == "string"
													{
														let field_name = String::from_utf8_lossy(
															&contents[key.byte_range().shrink(1)],
														);

														if let Some(model_bytes) = &this_model.inner {
															let model_str = String::from_utf8_lossy(model_bytes);
															let model_key = ModelName::from(_I(&model_str));

															if let Some(props) =
																self.index.models.populate_properties(model_key, &[])
																&& let Some(fields) = &props.fields && let Some(
																field_key,
															) = _G(&field_name) && let Some(field_info) =
																fields.get(&field_key.into())
															{
																// Check if this field has a relational type
																if let FieldKind::Relational(relation) =
																	&field_info.kind
																{
																	field_model = Some((*relation).into());
																}
															}
														}
													}
													break;
												}
												current = parent;
											}
										}

										if let Some(model) = field_model {
											let range = if let Some(end) = end_quote {
												ByteRange {
													start: ByteOffset(capture.node.start_byte() + 1),
													end: ByteOffset(capture.node.start_byte() + end),
												}
											} else {
												ByteRange {
													start: ByteOffset(capture.node.start_byte() + 1),
													end: ByteOffset(offset),
												}
											};

											let mut items = MaxVec::new(completions_limit);
											self.index.complete_property_name(
												&needle,
												range,
												_R(model).into(),
												rope,
												Some(PropertyKind::Field),
												true,
												false,
												&mut items,
											)?;
											return Ok(Some(CompletionResponse::List(CompletionList {
												is_incomplete: !items.has_space(),
												items: items.into_inner(),
											})));
										}
									}
								}

								// Normal case - not an ERROR node
								return self.python_completions_for_prop(
									root,
									&match_,
									offset,
									capture.node,
									this_model.inner,
									contents,
									completions_limit,
									Some(PropertyKind::Field),
									rope,
								);
							} else if let Some(cmdlist) = capture.node.next_named_sibling()
								&& Backend::is_commandlist(cmdlist, offset)
								&& let Some((needle, range, model)) = self.gather_commandlist(
									cmdlist,
									root,
									&match_,
									offset,
									range,
									this_model.inner,
									contents,
									true,
								) {
								let mut items = MaxVec::new(completions_limit);
								self.index.complete_property_name(
									&needle,
									range,
									ImStr::from(_R(model)),
									rope,
									Some(PropertyKind::Field),
									true,
									false,
									&mut items,
								)?;
								return Ok(Some(CompletionResponse::List(CompletionList {
									is_incomplete: !items.has_space(),
									items: items.into_inner(),
								})));
								// If gather_commandlist returns None, continue to next match
							}
						}
						Some(PyCompletions::FieldDescriptor) => {
							let Some(desc_value) = capture.node.next_named_sibling() else {
								continue;
							};

							let descriptor = &contents[capture.node.byte_range()];
							if desc_value.byte_range().contains_end(offset) {
								match descriptor {
									b"compute" | b"search" | b"inverse" | b"related" => {
										let prop_kind = if descriptor == b"related" {
											PropertyKind::Field
										} else {
											PropertyKind::Method
										};
										return self.python_completions_for_prop(
											root,
											&match_,
											offset,
											desc_value,
											this_model.inner,
											contents,
											completions_limit,
											Some(prop_kind),
											rope,
										);
									}
									b"comodel_name" => {
										// same as model
										let range = desc_value.byte_range();
										let (needle, byte_range) =
											some!(extract_string_needle_at_offset(rope, range, offset));
										let range = ok!(rope_conv(byte_range, rope));
										early_return.lift(move || async move {
											let mut items = MaxVec::new(completions_limit);
											self.index.complete_model(&needle, range, &mut items)?;
											Ok(Some(CompletionResponse::List(CompletionList {
												is_incomplete: !items.has_space(),
												items: items.into_inner(),
											})))
										});
										break 'match_;
									}
									b"groups" => {
										// complete res.groups records
										let range = desc_value.byte_range().shrink(1);
										let value = Cow::from(some!(rope.get_byte_slice(range.clone())));
										let mut ref_ = None;
										determine_csv_xmlid_subgroup(&mut ref_, (&value, range), offset);
										let (needle, range) = some!(ref_);
										let needle = needle[..offset - range.start - 1].to_string();
										early_return.lift(move || async move {
											let mut items = MaxVec::new(completions_limit);
											self.index.complete_xml_id(
												&needle,
												range.map_unit(ByteOffset),
												rope,
												Some(&[ImStr::from_static("res.groups")]),
												current_module,
												&mut items,
											)?;
											Ok(Some(CompletionResponse::List(CompletionList {
												is_incomplete: !items.has_space(),
												items: items.into_inner(),
											})))
										});
										break 'match_;
									}
									_ => {}
								}
							}

							if matches!(descriptor, b"comodel_name" | b"domain" | b"groups") {
								field_descriptors.push((descriptor, desc_value));
							}
							if desc_value.byte_range().contains_end(offset) {
								field_descriptor_in_offset = Some((descriptor, desc_value));
							}
						}
						Some(PyCompletions::Depends)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::XmlId)
						| Some(PyCompletions::Prop)
						| Some(PyCompletions::Scope)
						| Some(PyCompletions::ReadFn)
						| Some(PyCompletions::FieldType)
						| None => {}
					}
				}
				if let Some((b"domain", value)) = field_descriptor_in_offset {
					let mut domain_node = value;
					if domain_node.kind() == "lambda" {
						let Some(body) = domain_node.child_by_field_name("body") else {
							continue;
						};
						domain_node = body;
					}
					if domain_node.kind() != "list" {
						continue;
					}
					let comodel_name = field_descriptors
						.iter()
						.find_map(|&(desc, node)| {
							(desc == b"comodel_name").then(|| &contents[node.byte_range().shrink(1)])
						})
						.or(field_model);

					let Some(mapped) = domain_node.named_children(&mut domain_node.walk()).find_map(|domain| {
						// find the mapped domain element that contains the offset
						// [("id", "=", 123)]
						//    ^ this one
						if domain.kind() != "tuple" {
							return None;
						}
						let mapped = domain.named_child(0)?;
						mapped.byte_range().contains(&offset).then_some(mapped)
					}) else {
						continue;
					};

					return self.python_completions_for_prop(
						root,
						&match_,
						offset,
						mapped,
						comodel_name,
						contents,
						completions_limit,
						Some(PropertyKind::Field),
						rope,
					);
				}
			}
			if early_return.is_none() {
				// Check if we're in a broken syntax situation (string without colon in dictionary)
				let cursor_node = root.descendant_for_byte_range(offset, offset);
				if let Some(node) = cursor_node {
					// Check if we're in a string that's part of an ERROR node
					let mut current = node;
					while let Some(parent) = current.parent() {
						if parent.kind() == "ERROR" {
							// Check if the ERROR's parent is a dictionary
							if let Some(grandparent) = parent.parent()
								&& grandparent.kind() == "dictionary"
							{
								// For broken syntax (string without colon), we want to show all fields
								// So we use an empty needle
								let needle = Cow::Borrowed("");

								// Try to determine the model from the context
								// Look for the field assignment this dictionary belongs to
								let mut field_model: Option<Symbol<ModelEntry>> = None;
								let mut dict_parent = grandparent;

								while let Some(parent) = dict_parent.parent() {
									if parent.kind() == "list"
										&& let Some(list_parent) = parent.parent()
										&& list_parent.kind() == "pair"
									{
										if let Some(key) = list_parent.child_by_field_name("key")
											&& key.kind() == "string" && let Some(model_bytes) = &this_model.inner
										{
											let field_name =
												String::from_utf8_lossy(&contents[key.byte_range().shrink(1)]);

											let model_str = String::from_utf8_lossy(model_bytes);
											let model_key = ModelName::from(_I(&model_str));

											// Check if this field has a relational type
											if let Some(props) = self.index.models.populate_properties(model_key, &[])
												&& let Some(fields) = &props.fields && let Some(field_key) =
												_G(&field_name) && let Some(field_info) = fields.get(&field_key.into())
												&& let FieldKind::Relational(relation) = field_info.kind
											{
												field_model = Some(relation.into());
											}
										}
										break;
									}
									dict_parent = parent;
								}

								if let Some(model) = field_model {
									// For broken syntax, we want to replace the whole string
									let range = if node.kind() == "string_content" {
										// Find the parent string node to get the full range
										if let Some(string_parent) = node.parent() {
											if string_parent.kind() == "string" {
												string_parent.byte_range().shrink(1).map_unit(ByteOffset)
											} else {
												node.byte_range().map_unit(ByteOffset)
											}
										} else {
											node.byte_range().map_unit(ByteOffset)
										}
									} else {
										node.byte_range().shrink(1).map_unit(ByteOffset)
									};
									let mut items = MaxVec::new(completions_limit);
									self.index.complete_property_name(
										&needle,
										range,
										_R(model).into(),
										rope,
										Some(PropertyKind::Field),
										true,
										false,
										&mut items,
									)?;
									return Ok(Some(CompletionResponse::List(CompletionList {
										is_incomplete: !items.has_space(),
										items: items.into_inner(),
									})));
								}
							}
						}
						current = parent;
					}
				}

				// Fallback to regular attribute completion
				let (model, needle, range) = some!(self.attribute_at_offset(offset, root, contents));
				let mut items = MaxVec::new(completions_limit);
				self.index.complete_property_name(
					&needle,
					range.map_unit(ByteOffset),
					ImStr::from(model),
					rope,
					None,
					false,
					false,
					&mut items,
				)?;
				return Ok(Some(CompletionResponse::List(CompletionList {
					is_incomplete: !items.has_space(),
					items: items.into_inner(),
				})));
			}
		}
		let result = some!(early_return.call());
		result.await
	}
	/// `range` is the entire range of the mapped **string**, quotes included.
	fn python_completions_for_prop(
		&self,
		root: Node,
		match_: &QueryMatch,
		offset: usize,
		node: Node,
		this_model: Option<&[u8]>,
		contents: &[u8],
		completions_limit: usize,
		prop_type: Option<PropertyKind>,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<CompletionResponse>> {
		let Mapped {
			needle,
			model,
			single_field,
			range,
		} = some!(self.gather_mapped(
			root,
			match_,
			Some(offset),
			node.byte_range(),
			this_model,
			contents,
			true,
			matches!(prop_type, Some(PropertyKind::Method)).then_some(true)
		));

		// range:  foo.bar.baz
		// needle: foo.ba
		let mut range = range;
		let mut items = MaxVec::new(completions_limit);
		let mut needle = needle.as_ref();
		let mut model = some!(_G(model));

		if !single_field {
			some!(
				(self.index.models)
					.resolve_mapped(&mut model, &mut needle, Some(&mut range))
					.ok()
			);
		}
		let model_name = _R(model);
		self.index.complete_property_name(
			needle,
			range,
			ImStr::from(model_name),
			rope,
			prop_type,
			node.kind() == "string",
			false,
			&mut items,
		)?;
		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: !items.has_space(),
			items: items.into_inner(),
		})))
	}
}
