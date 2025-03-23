use std::{borrow::Cow, cmp::Ordering, ops::ControlFlow};

use ropey::Rope;
use tower_lsp_server::lsp_types::{Diagnostic, DiagnosticSeverity};
use tracing::{debug, warn};
use tree_sitter::{Node, QueryCursor, QueryMatch};

use crate::utils::{
	lsp_range_to_offset_range, offset_range_to_lsp_range, ts_range_to_lsp_range, ByteOffset, ByteRange, Erase, RangeExt,
};
use crate::{
	analyze::{determine_scope, Scope, Type, MODEL_METHODS},
	backend::Backend,
	index::{Index, _G, _I, _R},
	model::ResolveMappedError,
};

use super::{top_level_stmt, Mapped, PyCompletions, ThisModel};

impl Backend {
	pub fn diagnose_python(
		&self,
		path: &str,
		rope: &Rope,
		damage_zone: Option<ByteRange>,
		diagnostics: &mut Vec<Diagnostic>,
	) {
		let Some(ast) = self.ast_map.get(path) else {
			warn!("Did not build AST for {path}");
			return;
		};
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let query = PyCompletions::query();
		let mut root = ast.root_node();
		// TODO: Limit range of diagnostics with new heuristics
		if let Some(zone) = damage_zone.as_ref() {
			root = top_level_stmt(root, zone.end.0).unwrap_or(root);
			diagnostics.retain(|diag| {
				// If we couldn't get a range here, rope has changed significantly so just toss the diag.
				let Some(range) = lsp_range_to_offset_range(diag.range, rope) else {
					return false;
				};
				!root.byte_range().contains(&range.start.0)
			});
		} else {
			// There is no damage zone, assume everything has been reset.
			diagnostics.clear();
		}
		let in_active_root =
			|range: core::ops::Range<usize>| damage_zone.as_ref().map(|zone| zone.intersects(range)).unwrap_or(true);
		let top_level_ranges = root
			.named_children(&mut root.walk())
			.map(|node| node.byte_range())
			.collect::<Vec<_>>();
		let mut cursor = QueryCursor::new();
		let mut this_model = ThisModel::default();
		for match_ in cursor.matches(query, root, contents) {
			let mut field_descriptors = vec![];

			for capture in match_.captures {
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						let xml_id = String::from_utf8_lossy(&contents[capture.node.byte_range().shrink(1)]);
						let xml_id_key = _G(&xml_id);
						let mut id_found = false;
						if let Some(id) = xml_id_key {
							id_found = self.index.records.contains_key(&id.into());
						}
						if !id_found {
							diagnostics.push(Diagnostic {
								range: ts_range_to_lsp_range(capture.node.range()),
								message: format!("No XML record with ID `{xml_id}` found"),
								severity: Some(DiagnosticSeverity::WARNING),
								..Default::default()
							})
						}
					}
					Some(PyCompletions::Model) => {
						match capture.node.parent() {
							Some(subscript) if subscript.kind() == "subscript" => {
								// diagnose only, do not tag
								let range = capture.node.byte_range().shrink(1);
								let model = String::from_utf8_lossy(&contents[range.clone()]);
								let model_key = _G(&model);
								let has_model = model_key.map(|model| self.index.models.contains_key(&model.into()));
								if !has_model.unwrap_or(false) {
									diagnostics.push(Diagnostic {
										range: offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone())
											.unwrap(),
										message: format!("`{model}` is not a valid model name"),
										severity: Some(DiagnosticSeverity::ERROR),
										..Default::default()
									})
								}
								continue;
							}
							_ => {}
						}
						let Ok(idx) = top_level_ranges.binary_search_by(|range| {
							let needle = capture.node.end_byte();
							if needle < range.start {
								Ordering::Greater
							} else if needle > range.end {
								Ordering::Less
							} else {
								Ordering::Equal
							}
						}) else {
							debug!("binary search for top-level range failed");
							continue;
						};
						this_model.tag_model(capture.node, &match_, top_level_ranges[idx].clone(), contents);
					}
					Some(PyCompletions::FieldDescriptor) => {
						// fields.Many2one(field_descriptor=...)

						let Some(desc_value) = capture.node.next_named_sibling() else {
							continue;
						};

						let descriptor = &contents[capture.node.byte_range()];
						if matches!(
							descriptor,
							b"comodel_name" | b"domain" | b"compute" | b"search" | b"inverse" | b"related"
						) {
							field_descriptors.push((descriptor, desc_value));
						}
					}
					Some(PyCompletions::Mapped) => {
						self.diagnose_mapped(
							rope,
							diagnostics,
							contents,
							root,
							this_model.inner,
							&match_,
							capture.node.byte_range(),
							true,
						);
					}
					Some(PyCompletions::Scope) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						self.diagnose_python_scope(root, capture.node, contents, diagnostics);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| None => {}
				}
			}

			// post-process for field_descriptors
			for &(descriptor, node) in &field_descriptors {
				match descriptor {
					b"compute" | b"search" | b"inverse" | b"related" => self.diagnose_mapped(
						rope,
						diagnostics,
						contents,
						root,
						this_model.inner,
						&match_,
						node.byte_range(),
						descriptor == b"related",
					),
					b"comodel_name" => {
						let range = node.byte_range().shrink(1);
						let model = String::from_utf8_lossy(&contents[range.clone()]);
						let model_key = _G(&model);
						let has_model = model_key.map(|model| self.index.models.contains_key(&model.into()));
						if !has_model.unwrap_or(false) {
							diagnostics.push(Diagnostic {
								range: offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone()).unwrap(),
								message: format!("`{model}` is not a valid model name"),
								severity: Some(DiagnosticSeverity::ERROR),
								..Default::default()
							})
						}
					}
					b"domain" => {
						let mut domain_node = node;
						if domain_node.kind() == "lambda" {
							let Some(body) = domain_node.child_by_field_name("body") else {
								continue;
							};
							domain_node = body;
						}
						if domain_node.kind() != "list" {
							continue;
						}
						let Some(comodel_name) = field_descriptors
							.iter()
							.find_map(|&(desc, node)| (desc == b"comodel_name").then_some(node))
						else {
							continue;
						};
						let comodel_name = Some(&contents[comodel_name.byte_range().shrink(1)]);

						// TODO: walk subdomains (`any` and `not any`)
						for domain in domain_node.named_children(&mut domain_node.walk()) {
							if domain.kind() != "tuple" {
								continue;
							}

							let Some(mapped) = domain.named_child(0) else { continue };
							if mapped.kind() != "string" {
								continue;
							}

							self.diagnose_mapped(
								rope,
								diagnostics,
								contents,
								root,
								comodel_name,
								&match_,
								mapped.byte_range(),
								true,
							);
						}
					}
					_ => {}
				}
			}
		}
	}
	fn diagnose_python_scope(&self, root: Node, node: Node, contents: &[u8], diagnostics: &mut Vec<Diagnostic>) {
		// Most of these steps are similar to what is done inside model_of_range.
		let offset = node.start_byte();
		let Some((self_type, fn_scope, self_param)) = determine_scope(root, contents, offset) else {
			return;
		};
		let mut scope = Scope::default();
		let self_type = match self_type {
			Some(type_) => &contents[type_.byte_range().shrink(1)],
			None => &[],
		};
		scope.super_ = Some(self_param.as_ref().into());
		scope.insert(
			self_param.into_owned(),
			Type::Model(String::from_utf8_lossy(self_type).as_ref().into()),
		);
		let scope_end = fn_scope.end_byte();
		Index::walk_scope(fn_scope, Some(scope), |scope, node| {
			let entered = (self.index).build_scope(scope, node, scope_end, contents)?;

			let attribute = node.child_by_field_name("attribute");
			if node.kind() != "attribute" || attribute.as_ref().unwrap().kind() != "identifier" {
				return ControlFlow::Continue(entered);
			}

			let attribute = attribute.unwrap();
			static MODEL_BUILTINS: phf::Set<&str> = phf::phf_set!(
				"env",
				"id",
				"ids",
				"display_name",
				"create_date",
				"write_date",
				"create_uid",
				"write_uid",
				"pool",
				"record",
				"flush_model",
				"mapped",
				"fields_get",
				"user_has_groups",
			);
			let prop = String::from_utf8_lossy(&contents[attribute.byte_range()]);
			if prop.starts_with('_') || MODEL_BUILTINS.contains(&prop) || MODEL_METHODS.contains(prop.as_bytes()) {
				return ControlFlow::Continue(entered);
			}

			let Some(lhs_t) = (self.index).type_of(node.child_by_field_name("object").unwrap(), scope, contents) else {
				return ControlFlow::Continue(entered);
			};

			let Some(model_name) = (self.index).try_resolve_model(&lhs_t, scope) else {
				return ControlFlow::Continue(entered);
			};

			if (self.index).has_attribute(&lhs_t, &contents[attribute.byte_range()], scope) {
				return ControlFlow::Continue(entered);
			}

			// HACK: fix this issue where the model name is just empty
			if _R(model_name).is_empty() {
				return ControlFlow::Continue(entered);
			}

			diagnostics.push(Diagnostic {
				range: ts_range_to_lsp_range(attribute.range()),
				severity: Some(DiagnosticSeverity::ERROR),
				message: format!(
					"Model `{}` has no property `{}`",
					_R(model_name),
					String::from_utf8_lossy(&contents[attribute.byte_range()]),
				),
				..Default::default()
			});

			ControlFlow::Continue(entered)
		});
	}
	pub(crate) fn diagnose_mapped(
		&self,
		rope: &Rope,
		diagnostics: &mut Vec<Diagnostic>,
		contents: &[u8],
		root: Node<'_>,
		model: Option<&[u8]>,
		match_: &QueryMatch<'_, '_>,
		mapped_range: std::ops::Range<usize>,
		expect_field: bool,
	) {
		let Some(Mapped {
			needle,
			model,
			single_field,
			mut range,
		}) = self.gather_mapped(
			root,
			match_,
			None,
			mapped_range,
			model,
			contents,
			false,
			(!expect_field).then_some(true),
		)
		else {
			return;
		};
		let mut model = _I(&model);
		let mut needle = needle.as_ref();
		if single_field {
			if let Some(dot) = needle.find('.') {
				let message_range = range.start.0 + dot..range.end.0;
				diagnostics.push(Diagnostic {
					range: offset_range_to_lsp_range(message_range.map_unit(ByteOffset), rope.clone()).unwrap(),
					severity: Some(DiagnosticSeverity::ERROR),
					message: "Dotted access is not supported in this context".to_string(),
					..Default::default()
				});
				needle = &needle[..dot];
				range = (range.start.0..range.start.0 + dot).map_unit(ByteOffset);
			}
		} else {
			match (self.index.models).resolve_mapped(&mut model, &mut needle, Some(&mut range)) {
				Ok(()) => {}
				Err(ResolveMappedError::NonRelational) => {
					diagnostics.push(Diagnostic {
						range: offset_range_to_lsp_range(range, rope.clone()).unwrap(),
						severity: Some(DiagnosticSeverity::ERROR),
						message: format!("`{needle}` is not a relational field"),
						..Default::default()
					});
					return;
				}
			}
		}
		if needle.is_empty() {
			// Nothing to compare yet, keep going.
			return;
		}
		let mut has_property = false;
		if self.index.models.contains_key(&model.into()) {
			let Some(entry) = self.index.models.populate_properties(model.into(), &[]) else {
				return;
			};
			static MAPPED_BUILTINS: phf::Set<&str> = phf::phf_set!(
				"id",
				"display_name",
				"create_date",
				"write_date",
				"create_uid",
				"write_uid"
			);
			if MAPPED_BUILTINS.contains(needle) {
				return;
			}
			if let Some(key) = _G(needle) {
				if expect_field {
					let Some(fields) = entry.fields.as_ref() else { return };
					has_property = fields.contains_key(&key.into())
				} else {
					let Some(methods) = entry.methods.as_ref() else { return };
					has_property = methods.contains_key(&key.into());
				}
			}
		}
		if !has_property {
			diagnostics.push(Diagnostic {
				range: offset_range_to_lsp_range(range, rope.clone()).unwrap(),
				severity: Some(DiagnosticSeverity::ERROR),
				message: format!(
					"Model `{}` has no {} `{needle}`",
					_R(model),
					if expect_field { "field" } else { "method" }
				),
				..Default::default()
			});
		}
	}
}
