use ropey::Rope;
use tower_lsp_server::lsp_types::{CompletionList, CompletionResponse};
use tree_sitter::{Node, QueryMatch};

use std::borrow::Cow;
use std::sync::atomic::Ordering::Relaxed;

use tower_lsp_server::{UriExt, lsp_types::*};
use tracing::{debug, warn};
use tree_sitter::Tree;

use crate::backend::Backend;
use crate::index::{_G, _R};
use crate::model::PropertyKind;
use crate::some;
use crate::utils::MaxVec;
use crate::utils::*;

use super::{Mapped, PyCompletions, ThisModel, top_level_stmt};

impl Backend {
	pub(crate) async fn python_completions(
		&self,
		params: CompletionParams,
		ast: Tree,
		rope: Rope,
	) -> anyhow::Result<Option<CompletionResponse>> {
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, &rope) else {
			warn!("invalid position {:?}", params.text_document_position.position);
			return Ok(None);
		};
		let path = some!(params.text_document_position.text_document.uri.to_file_path());
		let Some(current_module) = self.index.find_module_of(&path) else {
			debug!("no current module");
			return Ok(None);
		};
		let mut cursor = tree_sitter::QueryCursor::new();
		let contents = Cow::from(rope.clone());
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
							let rope = rope.clone();
							early_return.lift(move || async move {
								let mut items = MaxVec::new(completions_limit);
								self.complete_xml_id(&needle, range, rope, model_filter, current_module, &mut items)?;
								Ok(Some(CompletionResponse::List(CompletionList {
									is_incomplete: !items.has_space(),
									items: items.into_inner(),
								})))
							});
							break 'match_;
						}
						Some(PyCompletions::Model) => {
							if range.contains_end(offset) {
								let slice = some!(rope.get_byte_slice(range.clone()));
								let relative_offset = range.start;
								let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
								let range = some!(offset_range_to_lsp_range(
									range.shrink(1).map_unit(ByteOffset),
									rope.clone()
								));
								early_return.lift(move || async move {
									let mut items = MaxVec::new(completions_limit);
									self.complete_model(&needle, range, &mut items)?;
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
								return self.python_completions_for_prop(
									root,
									&match_,
									offset,
									capture.node,
									this_model.inner,
									contents,
									completions_limit,
									Some(PropertyKind::Field),
									rope.clone(),
								);
							} else if let Some(cmdlist) = capture.node.next_named_sibling()
								&& Backend::is_commandlist(cmdlist, offset)
							{
								let (needle, range, model) = some!(self.gather_commandlist(
									cmdlist,
									root,
									&match_,
									offset,
									range,
									this_model.inner,
									contents,
									true,
								));
								let mut items = MaxVec::new(completions_limit);
								self.complete_property_name(
									&needle,
									range,
									_R(model).to_string(),
									rope.clone(),
									Some(PropertyKind::Field),
									true,
									&mut items,
								)?;
								return Ok(Some(CompletionResponse::List(CompletionList {
									is_incomplete: !items.has_space(),
									items: items.into_inner(),
								})));
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
										let slice = some!(rope.get_byte_slice(range.clone()));
										let relative_offset = range.start;
										let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
										let range = some!(offset_range_to_lsp_range(
											range.shrink(1).map_unit(ByteOffset),
											rope.clone()
										));
										early_return.lift(move || async move {
											let mut items = MaxVec::new(completions_limit);
											self.complete_model(&needle, range, &mut items)?;
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

							if matches!(descriptor, b"comodel_name" | b"domain") {
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
				if let Some((descriptor, value)) = field_descriptor_in_offset {
					if descriptor == b"domain" {
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
			}
			if early_return.is_none() {
				let (model, needle, range) = some!(self.attribute_at_offset(offset, root, contents));
				let rope = rope.clone();
				let mut items = MaxVec::new(completions_limit);
				self.complete_property_name(
					&needle,
					range.map_unit(ByteOffset),
					model.to_string(),
					rope,
					None,
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
		rope: Rope,
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
		self.complete_property_name(
			needle,
			range,
			model_name.to_string(),
			rope,
			prop_type,
			node.kind() == "string",
			&mut items,
		)?;
		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: !items.has_space(),
			items: items.into_inner(),
		})))
	}
}
