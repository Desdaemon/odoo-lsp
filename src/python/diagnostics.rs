use std::{borrow::Cow, cmp::Ordering, ops::ControlFlow};

use tower_lsp_server::lsp_types::{Diagnostic, DiagnosticSeverity};
use tracing::{debug, warn};
use tree_sitter::{Node, QueryCursor, QueryMatch};

use crate::index::{_R, Index};
use crate::prelude::*;

use crate::{
	analyze::{MODEL_METHODS, Scope, Type, determine_scope},
	backend::Backend,
	model::ResolveMappedError,
};

use super::{Mapped, PyCompletions, PyImports, ThisModel, top_level_stmt};

/// Python extensions.
impl Backend {
	pub fn diagnose_python(
		&self,
		path: &str,
		rope: RopeSlice<'_>,
		damage_zone: Option<ByteRange>,
		diagnostics: &mut Vec<Diagnostic>,
	) {
		let Some(ast) = self.ast_map.get(path) else {
			warn!("Did not build AST for {path}");
			return;
		};
		let contents = Cow::from(rope);
		let query = PyCompletions::query();
		let mut root = ast.root_node();
		// TODO: Limit range of diagnostics with new heuristics
		if let Some(zone) = damage_zone.as_ref() {
			root = top_level_stmt(root, zone.end.0).unwrap_or(root);
			let before_count = diagnostics.len();
			diagnostics.retain(|diag| {
				// If we couldn't get a range here, rope has changed significantly so just toss the diag.
				let Ok(range) = rope_conv::<_, ByteRange>(diag.range, rope) else {
					return false;
				};
				!root.byte_range().contains(&range.start.0)
			});
			debug!(
				"Retained {}/{} diagnostics after damage zone check",
				diagnostics.len(),
				before_count
			);
		} else {
			// There is no damage zone, assume everything has been reset.
			debug!("Clearing all diagnostics - no damage zone");
			diagnostics.clear();
		}
		let in_active_root =
			|range: core::ops::Range<usize>| damage_zone.as_ref().map(|zone| zone.intersects(range)).unwrap_or(true);

		// Diagnose missing imports
		self.diagnose_python_imports(diagnostics, &contents, ast.root_node());

		// Diagnose manifest dependencies if this is a __manifest__.py file
		if path.ends_with("__manifest__.py") {
			self.diagnose_manifest_dependencies(diagnostics, &contents, ast.root_node());
		}
		let top_level_ranges = root
			.named_children(&mut root.walk())
			.map(|node| node.byte_range())
			.collect::<Vec<_>>();
		let mut cursor = QueryCursor::new();
		let mut this_model = ThisModel::default();
		let mut matches = cursor.matches(query, root, contents.as_bytes());
		while let Some(match_) = matches.next() {
			let mut field_descriptors = vec![];
			let mut field_model = None;

			for capture in match_.captures {
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}

						let range = capture.node.byte_range().shrink(1);
						let mut slice = &contents[range.clone()];

						let mut xmlids = vec![];

						if match_
							.nodes_for_capture_index(PyCompletions::HasGroups as _)
							.next()
							.is_some()
						{
							let mut start = range.start;
							while let Some((xmlid, rest)) = slice.split_once(',') {
								let range = start..start + xmlid.len();
								start = range.end + 1;
								xmlids.push((xmlid, range));
								slice = rest;
							}
						} else {
							xmlids.push((slice, range));
						}
						for (xmlid, range) in xmlids {
							let mut id_found = false;
							if let Some(id) = _G(xmlid) {
								id_found = self.index.records.contains_key(&id.into());
							}

							if !id_found {
								let range = rope_conv(range.map_unit(ByteOffset), rope)
									.expect(format_loc!("failed to get range for xmlid diag"));
								diagnostics.push(Diagnostic {
									range,
									message: format!("No XML record with ID `{xmlid}` found"),
									severity: Some(DiagnosticSeverity::WARNING),
									..Default::default()
								})
							}
						}
					}
					Some(PyCompletions::Model) => {
						match capture.node.parent() {
							Some(subscript) if subscript.kind() == "subscript" => {
								// diagnose only, do not tag
								let range = capture.node.byte_range().shrink(1);
								let model = &contents[range.clone()];
								let model_key = _G(model);
								let has_model = model_key.map(|model| self.index.models.contains_key(&model.into()));
								if !has_model.unwrap_or(false) {
									diagnostics.push(Diagnostic {
										range: rope_conv(range.map_unit(ByteOffset), rope).unwrap(),
										message: format!("`{model}` is not a valid model name"),
										severity: Some(DiagnosticSeverity::ERROR),
										..Default::default()
									})
								}
								continue;
							}
							_ => {}
						}
						if let Some(field_type) = match_.nodes_for_capture_index(PyCompletions::FieldType as _).next() {
							if !matches!(
								&contents[field_type.byte_range()],
								"One2many" | "Many2one" | "Many2many"
							) {
								continue;
							}
							let range = capture.node.byte_range().shrink(1);
							let model = &contents[range.clone()];
							let model_key = _G(model);
							let has_model = model_key.map(|model| self.index.models.contains_key(&model.into()));
							if !has_model.unwrap_or(false) {
								diagnostics.push(Diagnostic {
									range: rope_conv(range.map_unit(ByteOffset), rope).unwrap(),
									message: format!("`{model}` is not a valid model name"),
									severity: Some(DiagnosticSeverity::ERROR),
									..Default::default()
								})
							} else if field_model.is_none() {
								field_model = Some(&contents[range]);
							}
							continue;
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
						this_model.tag_model(capture.node, match_, top_level_ranges[idx].clone(), &contents);
					}
					Some(PyCompletions::FieldDescriptor) => {
						// fields.Many2one(field_descriptor=...)

						let Some(desc_value) = capture.node.next_named_sibling() else {
							continue;
						};

						let descriptor = &contents[capture.node.byte_range()];
						if matches!(
							descriptor,
							"comodel_name" | "domain" | "compute" | "search" | "inverse" | "related"
						) {
							field_descriptors.push((descriptor, desc_value));
						}
					}
					Some(PyCompletions::Mapped) => self.diagnose_mapped(
						rope,
						diagnostics,
						&contents,
						root,
						this_model.inner,
						match_,
						capture.node.byte_range(),
						true,
					),
					Some(PyCompletions::Scope) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						self.diagnose_python_scope(root, capture.node, &contents, diagnostics);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::HasGroups)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}

			// post-process for field_descriptors
			for &(descriptor, node) in &field_descriptors {
				match descriptor {
					"compute" | "search" | "inverse" | "related" => self.diagnose_mapped(
						rope,
						diagnostics,
						&contents,
						root,
						this_model.inner,
						match_,
						node.byte_range(),
						descriptor == "related",
					),
					"comodel_name" => {
						let range = node.byte_range().shrink(1);
						let model = &contents[range.clone()];
						let model_key = _G(model);
						let has_model = model_key.map(|model| self.index.models.contains_key(&model.into()));
						if !has_model.unwrap_or(false) {
							diagnostics.push(Diagnostic {
								range: rope_conv(range.map_unit(ByteOffset), rope).unwrap(),
								message: format!("`{model}` is not a valid model name"),
								severity: Some(DiagnosticSeverity::ERROR),
								..Default::default()
							})
						}
					}
					"domain" => {
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

						let Some(comodel_name) = field_model.or_else(|| {
							field_descriptors.iter().find_map(|&(desc, node)| {
								(desc == "comodel_name").then(|| &contents[node.byte_range().shrink(1)])
							})
						}) else {
							continue;
						};

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
								&contents,
								root,
								Some(comodel_name),
								match_,
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
	fn diagnose_python_scope(&self, root: Node, node: Node, contents: &str, diagnostics: &mut Vec<Diagnostic>) {
		// Most of these steps are similar to what is done inside model_of_range.
		let offset = node.start_byte();
		let Some((self_type, fn_scope, self_param)) = determine_scope(root, contents, offset) else {
			return;
		};
		let mut scope = Scope::default();
		let self_type = match self_type {
			Some(type_) => ImStr::from(&contents[type_.byte_range().shrink(1)]),
			None => ImStr::from_static(""),
		};
		scope.super_ = Some(self_param.into());
		scope.insert(self_param.to_string(), Type::Model(self_type));
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
			let prop = &contents[attribute.byte_range()];
			if prop.starts_with('_') || MODEL_BUILTINS.contains(prop) || MODEL_METHODS.contains(prop) {
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

			// Check if the attribute might belong to an unloaded auto_install module
			let attr_name = &contents[attribute.byte_range()];
			debug!(
				"Checking unloaded auto_install for model: {} attribute: {}",
				_R(model_name),
				attr_name
			);
			let diagnostic_message = if let Some((module_name, missing_deps_with_chains)) =
				self.index.get_unloaded_auto_install_for_model(_R(model_name))
			{
				debug!(
					"Found unloaded auto_install module {} that extends model {}",
					_R(module_name),
					_R(model_name)
				);
				let deps_str = if missing_deps_with_chains.len() == 1 {
					let (missing_dep, chain) = &missing_deps_with_chains[0];
					if chain.len() > 2 {
						// Show the dependency chain
						let chain_str = chain.iter().map(|m| _R(*m)).collect::<Vec<_>>().join(" → ");
						format!("'{}' (via: {})", _R(*missing_dep), chain_str)
					} else {
						format!("'{}'", _R(*missing_dep))
					}
				} else {
					// Multiple missing deps, just list them
					missing_deps_with_chains
						.iter()
						.map(|(dep, _)| format!("'{}'", _R(*dep)))
						.collect::<Vec<_>>()
						.join(", ")
				};
				format!(
					"Model `{}` has no property `{}`. This might be because module '{}' is marked as auto_install but cannot be loaded due to missing dependencies: {}",
					_R(model_name),
					attr_name,
					_R(module_name),
					deps_str
				)
			} else {
				format!("Model `{}` has no property `{}`", _R(model_name), attr_name,)
			};

			diagnostics.push(Diagnostic {
				range: span_conv(attribute.range()),
				severity: Some(DiagnosticSeverity::ERROR),
				message: diagnostic_message,
				..Default::default()
			});

			ControlFlow::Continue(entered)
		});
	}

	fn diagnose_python_imports(&self, diagnostics: &mut Vec<Diagnostic>, contents: &str, root: Node) {
		let query = PyImports::query();
		let mut cursor = tree_sitter::QueryCursor::new();

		let mut matches = cursor.matches(query, root, contents.as_bytes());
		while let Some(match_) = matches.next() {
			let mut module_path = None;
			let mut import_name = None;
			let mut import_node = None;

			for capture in match_.captures {
				match PyImports::from(capture.index) {
					Some(PyImports::ImportModule) => {
						let capture_text = &contents[capture.node.byte_range()];
						module_path = Some(capture_text.to_string());
					}
					Some(PyImports::ImportName) => {
						let capture_text = &contents[capture.node.byte_range()];
						import_name = Some(capture_text.to_string());
						import_node = Some(capture.node);
					}
					Some(PyImports::ImportAlias) => {
						// We still want to check the original import name, not the alias
					}
					_ => {}
				}
			}

			if let (Some(name), Some(node)) = (import_name, import_node) {
				let full_module_path = if let Some(module) = module_path {
					module // For "from module import name", the module path is just the module
				} else {
					name.clone() // For "import name", the module path is the name itself
				};

				// Only check imports from odoo.addons.module_name pattern
				if !full_module_path.starts_with("odoo.addons.") {
					continue;
				}

				// Try to resolve the module path
				if self.index.resolve_py_module(&full_module_path).is_none() {
					diagnostics.push(Diagnostic {
						range: span_conv(node.range()),
						message: format!("Cannot resolve import '{name}'"),
						severity: Some(DiagnosticSeverity::ERROR),
						..Default::default()
					});
				}
			}
		}
	}
	pub(crate) fn diagnose_mapped(
		&self,
		rope: RopeSlice<'_>,
		diagnostics: &mut Vec<Diagnostic>,
		contents: &str,
		root: Node<'_>,
		model: Option<&str>,
		match_: &QueryMatch<'_, '_>,
		mapped_range: std::ops::Range<usize>,
		expect_field: bool,
	) {
		let Some(Mapped {
			mut needle,
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
		let mut model = _I(model);
		if single_field {
			if let Some(dot) = needle.find('.') {
				let message_range = range.start.0 + dot..range.end.0;
				diagnostics.push(Diagnostic {
					range: rope_conv(message_range.map_unit(ByteOffset), rope).unwrap(),
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
						range: rope_conv(range, rope).unwrap(),
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
				range: rope_conv(range, rope).unwrap(),
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

	fn diagnose_manifest_dependencies(&self, diagnostics: &mut Vec<Diagnostic>, contents: &str, root: Node) {
		use ts_macros::query;

		query! {
			ManifestDepsQuery(Dependency);

			((dictionary
				(pair
					(string (string_content) @_depends)
					(list
						(string) @DEPENDENCY
					)
				)
			) (#eq? @_depends "depends"))
		}

		// Get all available modules
		let all_available_modules = self.index.get_all_available_modules();

		let mut cursor = QueryCursor::new();
		let mut captures = cursor.captures(ManifestDepsQuery::query(), root, contents.as_bytes());

		while let Some((match_, idx)) = captures.next() {
			let capture = match_.captures[*idx];
			match ManifestDepsQuery::from(capture.index) {
				Some(ManifestDepsQuery::Dependency) => {
					let dep_node = capture.node;
					// Get the string content without quotes
					let dep_range = dep_node.byte_range();
					let dep_with_quotes = &contents[dep_range.clone()];

					// Skip if not a proper string
					if !dep_with_quotes.starts_with('"') && !dep_with_quotes.starts_with('\'') {
						continue;
					}

					// Extract the dependency name without quotes
					let dep_name = &contents[dep_range.shrink(1)];
					let dep_symbol = _I(dep_name).into();

					// Check if the dependency is available
					if !all_available_modules.contains(&dep_symbol) {
						// Adjust the range to start after the opening quote
						let mut range = dep_node.range();
						range.start_point.column += 2; // Skip quote and following space
						range.end_point.column -= 1;

						diagnostics.push(Diagnostic {
							range: span_conv(range),
							severity: Some(DiagnosticSeverity::ERROR),
							message: format!("Module '{dep_name}' is not available in your path"),
							..Default::default()
						});
					}
				}
				None => {}
			}
		}
	}
}
