use std::{borrow::Cow, cmp::Ordering, ops::ControlFlow};

use tower_lsp_server::lsp_types::{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location};
use tracing::{debug, warn};
use tree_sitter::{Node, QueryCursor, QueryMatch};

use crate::index::{_R, Index};
use crate::prelude::*;

use crate::{
	analyze::{MODEL_METHODS, Scope, Type, determine_scope},
	backend::Backend,
	model::{ModelName, ResolveMappedError},
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
				let ByteOffset(start) = rope_conv(diag.range.start, rope);
				!root.byte_range().contains(&start)
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
								let range = rope_conv(range.map_unit(ByteOffset), rope);
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
										range: rope_conv(range.map_unit(ByteOffset), rope),
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
									range: rope_conv(range.map_unit(ByteOffset), rope),
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

						let Some(desc_value) = python_next_named_sibling(capture.node) else {
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
						self.diagnose_python_scope(root, capture.node, &contents, diagnostics, path);
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
								range: rope_conv(range.map_unit(ByteOffset), rope),
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
	fn diagnose_python_scope(
		&self,
		root: Node,
		node: Node,
		contents: &str,
		diagnostics: &mut Vec<Diagnostic>,
		path: &str,
	) {
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
			#[rustfmt::skip]
			static MODEL_BUILTINS: phf::Set<&str> = phf::phf_set!(
				"env", "id", "ids", "display_name", "create_date", "write_date",
				"create_uid", "write_uid", "pool", "record", "flush_model", "mapped",
				"grouped", "_read_group", "filtered", "sorted", "_origin", "fields_get",
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
			let diagnostic_message = format!("Model `{}` has no property `{}`", _R(model_name), attr_name);

			// Build related information if this is an auto_install issue
			let related_information = if let Some((module_name, missing_deps_with_chains)) =
				self.index.get_unloaded_auto_install_for_model(_R(model_name))
			{
				self.build_auto_install_related_info(
					module_name,
					&missing_deps_with_chains,
					model_name,
					attr_name,
					path,
				)
			} else {
				None
			};

			diagnostics.push(Diagnostic {
				range: span_conv(attribute.range()),
				severity: Some(DiagnosticSeverity::ERROR),
				message: diagnostic_message,
				related_information,
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
					range: rope_conv(message_range.map_unit(ByteOffset), rope),
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
						range: rope_conv(range, rope),
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
				range: rope_conv(range, rope),
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

	/// Build related information for auto_install module diagnostics
	fn build_auto_install_related_info(
		&self,
		module_name: crate::index::ModuleName,
		_missing_deps_with_chains: &[(crate::index::ModuleName, Vec<crate::index::ModuleName>)],
		model_name: ModelName,
		attr_name: &str,
		current_path: &str,
	) -> Option<Vec<DiagnosticRelatedInformation>> {
		let mut related_info = Vec::new();

		// Try to find where the property is defined
		// First check if property is already in the index (for loaded modules)
		let mut property_found = false;
		if let Some(model_entry) = self.index.models.get(&model_name) {
			// Check fields
			if let Some(fields) = model_entry.fields.as_ref()
				&& let Some(field) = fields.get(&_I(attr_name).into())
				&& let Some(field_module) = self.index.find_module_of(&field.location.path.to_path())
				&& field_module == module_name
				&& let Some(uri) = Uri::from_file_path(field.location.path.to_path())
			{
				related_info.push(DiagnosticRelatedInformation {
					location: Location {
						uri,
						range: field.location.range,
					},
					message: format!("This field is defined in `{}`", _R(module_name)),
				});
				property_found = true;
			}

			// Check methods if field not found
			if !property_found
				&& let Some(methods) = model_entry.methods.as_ref()
				&& let Some(method) = methods.get(&_I(attr_name).into())
				&& let Some(loc) = method.locations.first()
				&& let Some(method_module) = self.index.find_module_of(&loc.path.to_path())
				&& method_module == module_name
				&& let Some(uri) = Uri::from_file_path(loc.path.to_path())
			{
				related_info.push(DiagnosticRelatedInformation {
					location: Location { uri, range: loc.range },
					message: format!("This method is defined in `{}`", _R(module_name)),
				});
				property_found = true;
			}
		}

		// If property not found in index, point to the module's models directory
		if !property_found && let Some(location) = self.get_module_models_location(module_name) {
			related_info.push(DiagnosticRelatedInformation {
				location,
				message: format!("This property is defined in `{}`", _R(module_name)),
			});
		}

		// Find current module's manifest to suggest adding dependency
		if let Some(location) = self.find_manifest_depends_location(current_path) {
			related_info.push(DiagnosticRelatedInformation {
				location,
				message: format!(
					"To expose this property, depend directly on `{}` or all of its reverse dependencies",
					_R(module_name)
				),
			});
		}

		// Find where auto_install is defined
		if let Some(location) = self.find_auto_install_location(module_name) {
			related_info.push(DiagnosticRelatedInformation {
				location,
				message: format!(
					"`{}` is defined as a bridge module here, alongside its reverse dependencies",
					_R(module_name)
				),
			});
		}

		debug!("Total related information entries: {}", related_info.len());
		for (i, info) in related_info.iter().enumerate() {
			debug!("  {}. {}", i + 1, info.message);
		}

		(!related_info.is_empty()).then_some(related_info)
	}

	/// Get the location of a module's models directory or main file
	fn get_module_models_location(&self, module_name: crate::index::ModuleName) -> Option<Location> {
		// Find the module in the index
		for root_entry in self.index.roots.iter() {
			let (root_path, modules) = root_entry.pair();
			if let Some(module_entry) = modules.get(&module_name) {
				let module_path = root_path.join(module_entry.path.as_str());

				// Try models/__init__.py first
				let models_init = module_path.join("models").join("__init__.py");
				if models_init.exists()
					&& let Some(uri) = Uri::from_file_path(&models_init)
				{
					return Some(Location {
						uri,
						range: Default::default(), // Point to start of file
					});
				}

				// Fallback to module directory
				if let Some(uri) = Uri::from_file_path(&module_path) {
					return Some(Location {
						uri,
						range: Default::default(),
					});
				}
			}
		}
		None
	}

	/// Find the depends location in the current module's manifest
	fn find_manifest_depends_location(&self, current_path: &str) -> Option<Location> {
		use tree_sitter::Parser;
		use ts_macros::query;
		tracing::warn!("find_manifest_depends_location called with path: {}", current_path);

		// Define a simple query for finding the depends list
		query! {
			DependsListQuery(DependsList);

			((dictionary
				(pair
					(string (string_content) @_depends)
					(list) @DEPENDS_LIST
				)
			) (#eq? @_depends "depends"))
		}

		let path_buf = std::path::PathBuf::from(current_path);
		if let Some(current_module) = self.index.find_module_of(&path_buf) {
			tracing::warn!("Found module: {:?}", current_module);
			// Find the module's manifest
			for root_entry in self.index.roots.iter() {
				let (root_path, modules) = root_entry.pair();
				if let Some(module_entry) = modules.get(&current_module) {
					let mut manifest_path = root_path.clone();
					manifest_path.push(module_entry.path.as_str());
					manifest_path.push("__manifest__.py");
					tracing::warn!("Manifest path: {:?}, exists: {}", manifest_path, manifest_path.exists());

					if let Ok(contents) = crate::test_utils::fs::read_to_string(&manifest_path) {
						let uri = Uri::from_file_path(&manifest_path).unwrap();
						// Try to parse and find depends
						let mut parser = Parser::new();
						if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_ok()
							&& let Some(ast) = parser.parse(&contents, None)
						{
							let mut cursor = QueryCursor::new();
							let mut captures =
								cursor.captures(DependsListQuery::query(), ast.root_node(), contents.as_bytes());

							if let Some((match_, idx)) = captures.next() {
								let capture = match_.captures[*idx];
								if let Some(DependsListQuery::DependsList) = DependsListQuery::from(capture.index) {
									return Some(Location {
										uri,
										range: span_conv(capture.node.range()),
									});
								}
							}
						}

						// If no depends found or parsing failed, just point to beginning of manifest
						return Some(Location {
							uri,
							range: Default::default(), // Points to start of file
						});
					}
					break;
				}
			}
		}
		None
	}

	/// Find where auto_install is defined in a module's manifest
	fn find_auto_install_location(&self, module_name: crate::index::ModuleName) -> Option<Location> {
		use tree_sitter::Parser;
		use ts_macros::query;

		// Define a query that handles Python's True (capital T) which is parsed as identifier
		query! {
			AutoInstallQuery(AutoInstallValue);

			((dictionary
				(pair
					(string (string_content) @_auto_install)
					[(true) (identifier)] @AUTO_INSTALL_VALUE
				)
			) (#eq? @_auto_install "auto_install"))
		}

		// Find the module's manifest
		for root_entry in self.index.roots.iter() {
			let (root_path, modules) = root_entry.pair();
			if let Some(module_entry) = modules.get(&module_name) {
				let mut manifest_path = root_path.clone();
				manifest_path.push(module_entry.path.as_str());
				manifest_path.push("__manifest__.py");

				if let Ok(contents) = crate::test_utils::fs::read_to_string(&manifest_path) {
					let uri = Uri::from_file_path(&manifest_path).unwrap();
					let mut parser = Parser::new();
					if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_ok()
						&& let Some(ast) = parser.parse(&contents, None)
					{
						let mut cursor = QueryCursor::new();
						let mut captures =
							cursor.captures(AutoInstallQuery::query(), ast.root_node(), contents.as_bytes());

						if let Some((match_, _)) = captures.next() {
							// Find the value capture
							for capture in match_.captures {
								if let Some(AutoInstallQuery::AutoInstallValue) = AutoInstallQuery::from(capture.index)
								{
									// Return the location of the value (True/true/1/"true"/etc)
									return Some(Location {
										uri,
										range: span_conv(capture.node.range()),
									});
								}
							}
						}
					}
				}
				break;
			}
		}
		None
	}
}
