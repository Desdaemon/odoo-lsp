use crate::{Backend, Text};
use odoo_lsp::analyze::{determine_scope, Scope, Type, MODEL_METHODS};

use std::borrow::Cow;
use std::cmp::Ordering;
use std::ops::ControlFlow;
use std::path::Path;
use std::sync::atomic::Ordering::Relaxed;

use lasso::Spur;
use odoo_lsp::index::{index_models, Index, PathSymbol, _G, _I, _R};
use ropey::Rope;
use tower_lsp_server::lsp_types::*;
use tracing::{debug, instrument, trace, warn};
use tree_sitter::{Node, Parser, QueryCursor, QueryMatch, Tree};

use odoo_lsp::model::{ModelName, ModelType, PropertyKind, ResolveMappedError};
use odoo_lsp::{errloc, utils::*};
use odoo_lsp::{format_loc, some};
use ts_macros::query;

#[rustfmt::skip]
query! {
	PyCompletions(Request, XmlId, Mapped, MappedTarget, Depends, ReadFn, Model, Prop, ForXmlId, Scope, FieldDescriptor, Compute);

(call [
  (attribute [
    (identifier) @_env
    (attribute (_) (identifier) @_env)] (identifier) @_ref)
  (attribute
    (identifier) @REQUEST (identifier) @_render)
  (attribute
    (_) (identifier) @FOR_XML_ID) ]
  (argument_list . (string) @XML_ID)
  (#eq? @_env "env")
  (#eq? @_ref "ref")
  (#eq? @REQUEST "request")
  (#eq? @_render "render")
  (#eq? @FOR_XML_ID "_for_xml_id"))

(subscript [
  (identifier) @_env
  (attribute (_) (identifier) @_env)]
  (string) @MODEL
  (#eq? @_env "env"))

((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @PROP [
        (string) @MODEL
        (list ((string) @MODEL ","?)*)
        (call
          (attribute
            (identifier) @_fields (identifier) (#eq? @_fields "fields"))
          (argument_list
            (keyword_argument [
              // alternatives for keyword arguments
              // if none of the alternatives match, the optional marker outside ensures that this field is still recognized
              ((identifier) @_related (#eq? @_related "related") (string) @MAPPED)
              ((identifier) @_compute_fn (#match? @_compute_fn "^(compute|search|inverse)$") (string) @COMPUTE) ])?)) ])))))

(call [
  (attribute
    (_) @MAPPED_TARGET (identifier) @_mapper)
  (attribute
    (identifier) @_api (identifier) @DEPENDS)]
  (argument_list (string) @MAPPED)
  (#match? @_mapper "^(mapp|filter|sort|group)ed$")
  (#eq? @_api "api")
  (#match? @DEPENDS "^(depends|constrains|onchange)$"))

(call [
  (identifier) @_Field
  (attribute (identifier) @_fields (identifier) @_Field) ]
  (argument_list
    . ((comment)* . (string) @MODEL)?
	((keyword_argument (identifier) @FIELD_DESCRIPTOR (_)) ","?)*)
  (#eq? @_fields "fields")
  (#match? @_Field "^(Many2one|One2many|Many2many)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @_search)
  (argument_list [
    (list [
      (tuple . (string) @MAPPED)
      (parenthesized_expression (string) @MAPPED)])
    (keyword_argument
      (identifier) @_domain
      (list [
        (tuple . (string) @MAPPED)
        (parenthesized_expression (string) @MAPPED)]))]))
  (#eq? @_domain "domain")
  (#match? @_search "^(search(_(read|count))?|_?read_group|filtered_domain|_where_calc)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @READ_FN)
  (argument_list [
    (list (string) @MAPPED)
    (keyword_argument
      (identifier) @_domain
      (list (string) @MAPPED)) ]))
  (#match? @_domain "^(groupby|aggregates)$")
  (#match? @READ_FN "^(_?read(_group)?|flush_model)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @DEPENDS)
  (argument_list . [
    (set (string) @MAPPED)
    (dictionary [
      (pair key: (string) @MAPPED)
      (ERROR (string) @MAPPED) ]) ]))
  (#match? @DEPENDS "^(write|copy)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @DEPENDS)
  (argument_list . [
    (set (string) @MAPPED)
    (dictionary [
      (pair key: (string) @MAPPED)
      (ERROR (string) @MAPPED) ])
    (_ [
      (set (string) @MAPPED)
      (dictionary [
        (pair key: (string) @MAPPED)
        (ERROR (string) @MAPPED) ]) ]) ]))
  (#eq? @DEPENDS "create"))

((class_definition
  (block [
    (function_definition) @SCOPE
    (decorated_definition
      (decorator
        (call
          (attribute (identifier) @_api (identifier) @_depends)
          (argument_list ((string) @MAPPED ","?)*)))
      (function_definition) @SCOPE) ]))
  (#eq? @_api "api")
  (#eq? @_depends "depends"))

(class_definition
  (block
    (decorated_definition
      (decorator (_) @_)
      (function_definition) @SCOPE)*)
  (#not-match? @_ "^api.depends"))
}

/// (module (_)*)
fn top_level_stmt(module: Node, offset: usize) -> Option<Node> {
	module
		.named_children(&mut module.walk())
		.find(|child| child.byte_range().contains_end(offset))
}

struct Mapped<'text> {
	needle: Cow<'text, str>,
	model: String,
	single_field: bool,
	range: ByteRange,
}

impl Backend {
	#[tracing::instrument(skip_all, fields(uri))]
	pub fn on_change_python(&self, text: &Text, uri: &Uri, rope: Rope, old_rope: Option<Rope>) -> anyhow::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(&tree_sitter_python::LANGUAGE.into())
			.expect("bug: failed to init python parser");
		self.update_ast(text, uri, rope.clone(), old_rope, parser)
	}
	pub async fn update_models(&self, text: Text, path: &Path, root: Spur, rope: Rope) -> anyhow::Result<()> {
		let text = match text {
			Text::Full(text) => Cow::from(text),
			// TODO: Limit range of possible updates based on delta
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let models = index_models(text.as_bytes())?;
		let path = PathSymbol::strip_root(root, path);
		self.index.models.append(path, true, &models).await;
		for model in models {
			match model.type_ {
				ModelType::Base { name, ancestors } => {
					let model_key = _G(&name).unwrap();
					let mut entry = self
						.index
						.models
						.try_get_mut(&model_key.into())
						.expect(format_loc!("deadlock"))
						.unwrap();
					entry
						.ancestors
						.extend(ancestors.into_iter().map(|sym| ModelName::from(_I(&sym))));
					drop(entry);
					self.index.models.populate_properties(model_key.into(), &[path]);
				}
				ModelType::Inherit(inherits) => {
					let Some(model) = inherits.first() else { continue };
					let model_key = _G(model).unwrap();
					self.index.models.populate_properties(model_key.into(), &[path]);
				}
			}
		}
		Ok(())
	}
	pub async fn did_save_python(&self, uri: Uri, root: Spur) -> anyhow::Result<()> {
		let path = uri.to_file_path().unwrap();
		let zone;
		_ = {
			let mut document = self
				.document_map
				.get_mut(uri.path().as_str())
				.ok_or_else(|| errloc!("(did_save) did not build document"))?;
			zone = document.damage_zone.take();
			let rope = document.rope.clone();
			let text = Cow::from(&document.rope).into_owned();
			self.update_models(Text::Full(text), &path, root, rope)
		}
		.await
		.inspect_err(|err| warn!("{err:?}"));
		if zone.is_some() {
			debug!("diagnostics");
			{
				let mut document = self.document_map.get_mut(uri.path().as_str()).unwrap();
				self.diagnose_python(
					uri.path().as_str(),
					&document.rope.clone(),
					zone,
					&mut document.diagnostics_cache,
				);
				let diags = document.diagnostics_cache.clone();
				self.client.publish_diagnostics(uri, diags, None)
			}
			.await;
		}

		Ok(())
	}
	pub async fn python_completions(
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
		let Some(current_module) = self.index.module_of_path(&path) else {
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
						Some(PyCompletions::XmlId) if range.contains(&offset) => {
							let range = range.shrink(1);
							let needle = String::from_utf8_lossy(&contents[range.start..offset]);
							let range = range.map_unit(ByteOffset);
							let rope = rope.clone();
							early_return.lift(move || async move {
								let mut items = MaxVec::new(completions_limit);
								self.complete_xml_id(&needle, range, rope, model_filter, current_module, &mut items)
									.await?;
								Ok(Some(CompletionResponse::List(CompletionList {
									is_incomplete: !items.has_space(),
									items: items.into_inner(),
								})))
							});
							break 'match_;
						}
						Some(PyCompletions::Model) => {
							// to be checked later down that it only belongs to (assignment)
							let is_inherit = match_
								.nodes_for_capture_index(PyCompletions::Prop as _)
								.next()
								.map(|prop| &contents[prop.byte_range()] == b"_inherit")
								.unwrap_or(true);
							if is_inherit && range.contains_end(offset) {
								let Some(slice) = rope.get_byte_slice(range.clone()) else {
									dbg!(&range);
									break 'match_;
								};
								let relative_offset = range.start;
								let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
								let range = some!(offset_range_to_lsp_range(
									range.shrink(1).map_unit(ByteOffset),
									rope.clone()
								));
								early_return.lift(move || async move {
									let mut items = MaxVec::new(completions_limit);
									self.complete_model(&needle, range, &mut items).await?;
									Ok(Some(CompletionResponse::List(CompletionList {
										is_incomplete: !items.has_space(),
										items: items.into_inner(),
									})))
								});
								break 'match_;
							} else if range.end < offset {
								this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
							}
						}
						Some(mapstr @ (PyCompletions::Mapped | PyCompletions::Compute))
							if range.contains_end(offset) =>
						{
							let Mapped {
								needle,
								model,
								single_field,
								range,
							} = some!(self.gather_mapped(
								root,
								&match_,
								Some(offset),
								range.clone(),
								this_model.inner,
								contents,
								true,
							));

							// range:  foo.bar.baz
							// needle: foo.ba
							let mut range = range;
							let mut items = MaxVec::new(completions_limit);
							let mut needle = needle.as_ref();
							let mut model = some!(_G(model));
							if !single_field {
								some!((self.index.models)
									.resolve_mapped(&mut model, &mut needle, Some(&mut range))
									.ok());
							}
							let model_name = _R(model);
							let prop_type = if matches!(mapstr, PyCompletions::Compute) {
								Some(PropertyKind::Method)
							} else {
								Some(PropertyKind::Field)
							};
							self.complete_property_name(
								needle,
								range,
								model_name.to_string(),
								rope,
								prop_type,
								&mut items,
							)?;
							return Ok(Some(CompletionResponse::List(CompletionList {
								is_incomplete: !items.has_space(),
								items: items.into_inner(),
							})));
						}
						Some(PyCompletions::FieldDescriptor) => {
							let Some(desc_value) = capture.node.next_named_sibling() else {
								continue;
							};

							let descriptor = &contents[capture.node.byte_range()];
							if matches!(descriptor, b"comodel_name" | b"domain") {
								field_descriptors.push((descriptor, desc_value));
							}
							if desc_value.byte_range().contains_end(offset) {
								field_descriptor_in_offset = Some((descriptor, desc_value));
							}
						}
						Some(PyCompletions::Depends)
						| Some(PyCompletions::MappedTarget)
						| Some(PyCompletions::Mapped)
						| Some(PyCompletions::Compute)
						| Some(PyCompletions::XmlId)
						| Some(PyCompletions::Prop)
						| Some(PyCompletions::Scope)
						| Some(PyCompletions::ReadFn)
						| None => {}
					}
				}
				if let Some((descriptor, value)) = field_descriptor_in_offset {
					let range = value.byte_range();
					match descriptor {
						b"comodel_name" => {
							// same as model
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let relative_offset = range.start;
							let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
							let range = some!(offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone()));
							early_return.lift(move || async move {
								let mut items = MaxVec::new(completions_limit);
								self.complete_model(&needle, range, &mut items).await?;
								Ok(Some(CompletionResponse::List(CompletionList {
									is_incomplete: !items.has_space(),
									items: items.into_inner(),
								})))
							});
							break 'match_;
						}
						b"domain" => {
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
								// FIXME: Only correct if this came from the unnamed field relation
								.or(this_model.inner);

							let Some(mapped) = domain_node.named_children(&mut domain_node.walk()).find_map(|domain| {
								// find the mapped domain element that contains the offset
								if domain.kind() != "tuple" {
									return None;
								}
								let mapped = domain.named_child(0)?;
								mapped.byte_range().contains(&offset).then_some(mapped)
							}) else {
								continue;
							};

							let Mapped {
								needle,
								model,
								single_field,
								range,
							} = some!(self.gather_mapped(
								root,
								&match_,
								Some(offset),
								mapped.byte_range(),
								comodel_name,
								contents,
								true,
							));

							// range:  foo.bar.baz
							// needle: foo.ba
							let mut range = range;
							let mut items = MaxVec::new(completions_limit);
							let mut needle = needle.as_ref();
							let mut model = some!(_G(model));
							if !single_field {
								some!(self
									.index
									.models
									.resolve_mapped(&mut model, &mut needle, Some(&mut range))
									.ok());
							}
							let model_name = _R(model);
							self.complete_property_name(
								needle,
								range,
								model_name.to_string(),
								rope,
								Some(PropertyKind::Field),
								&mut items,
							)?;
							return Ok(Some(CompletionResponse::List(CompletionList {
								is_incomplete: !items.has_space(),
								items: items.into_inner(),
							})));
						}
						_ => {}
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
	/// Gathers common information regarding a mapped access aka dot access.
	/// Only makes sense in [`PyCompletions`] queries.
	///
	/// Replacing:
	///
	///     "foo.bar.baz"
	///           ^cursor
	///      -----------range
	///      ------needle
	///
	/// Not replacing:
	///
	///     "foo.bar.baz"
	///           ^cursor
	///      -------range
	///      -------needle
	///
	fn gather_mapped<'text>(
		&self,
		root: Node,
		match_: &tree_sitter::QueryMatch,
		offset: Option<usize>,
		mut range: core::ops::Range<usize>,
		this_model: Option<&[u8]>,
		contents: &'text [u8],
		for_replacing: bool,
	) -> Option<Mapped<'text>> {
		let mut needle = if for_replacing {
			range = range.shrink(1);
			let offset = offset.unwrap_or(range.end);
			String::from_utf8_lossy(&contents[range.start..offset])
		} else {
			let slice = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
			let relative_start = range.start + 1;
			let offset = offset.unwrap_or((range.end - 1).max(relative_start + 1));
			// assert!(
			// 	offset >= relative_start,
			// 	"offset={} cannot be less than relative_start={}",
			// 	offset,
			// 	relative_start
			// );
			let slice_till_end = &slice[offset - relative_start..];
			// How many characters until the next period or end-of-string?
			let limit = slice_till_end.find('.').unwrap_or(slice_till_end.len());
			range = relative_start..offset + limit;
			// Cow::from(rope.get_byte_slice(range.clone())?)
			String::from_utf8_lossy(&contents[range.clone()])
		};
		if needle == "|" || needle == "&" {
			return None;
		}

		tracing::trace!(
			"(gather_mapped) {} matches={match_:?}",
			String::from_utf8_lossy(&contents[range.clone()])
		);

		let model;
		if let Some(local_model) = match_.nodes_for_capture_index(PyCompletions::MappedTarget as _).next() {
			let model_ = (self.index).model_of_range(root, local_model.byte_range().map_unit(ByteOffset), contents)?;
			model = _R(model_).to_string();
		} else if let Some(field_model) = match_.nodes_for_capture_index(PyCompletions::Model as _).next() {
			// A sibling @MODEL node; this is defined on the `fields.*(comodel_name='@MODEL', domain=[..])` pattern
			model = String::from_utf8_lossy(&contents[field_model.byte_range().shrink(1)]).into_owned();
		} else if let Some(this_model) = &this_model {
			model = String::from_utf8_lossy(this_model).to_string();
		} else {
			return None;
		}

		let mut single_field = false;
		if let Some(depends) = match_.nodes_for_capture_index(PyCompletions::Depends as _).next() {
			single_field = matches!(
				&contents[depends.byte_range()],
				b"write" | b"create" | b"constrains" | b"onchange"
			);
		} else if let Some(read_fn) = match_.nodes_for_capture_index(PyCompletions::ReadFn as _).next() {
			// read or read_group, fields only
			single_field = true;
			if contents[read_fn.byte_range()].ends_with(b"read_group") {
				// split off aggregate functions
				needle = match cow_split_once(needle, ":") {
					Err(unchanged) => unchanged,
					Ok((field, _)) => {
						range = range.start..range.start + field.len();
						field
					}
				}
			}
		} else if match_
			.nodes_for_capture_index(PyCompletions::Compute as _)
			.next()
			.is_some()
		{
			single_field = true;
		}

		Some(Mapped {
			needle,
			model,
			single_field,
			range: range.map_unit(ByteOffset),
		})
	}
	pub fn python_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> anyhow::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, &rope) else {
			return Err(errloc!("could not find offset for {}", uri.path().as_str()));
		};
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut this_model = ThisModel::default();
		'match_: for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let slice = Cow::from(slice);
						return self.jump_def_xml_id(&slice, &params.text_document_position_params.text_document.uri);
					}
					Some(PyCompletions::Model) => {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::Prop as _)
							.next()
							.map(|prop| matches!(&contents[prop.byte_range()], b"_name" | b"_inherit"))
							.unwrap_or(true);
						if is_meta && range.contains(&offset) {
							let range = range.shrink(1);
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							return self.jump_def_model(&slice);
						} else if range.end < offset {
							this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
						}
					}
					Some(PyCompletions::Mapped | PyCompletions::Compute) if range.contains_end(offset) => {
						if let Some(mapped) = self.gather_mapped(
							root,
							&match_,
							Some(offset),
							range.clone(),
							this_model.inner,
							contents,
							false,
						) {
							let mut needle = mapped.needle.as_ref();
							let mut model = _I(mapped.model);
							if !mapped.single_field {
								some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
							}
							let model = _R(model);
							return self.jump_def_property_name(needle, model);
						}
					}
					Some(PyCompletions::FieldDescriptor) => {
						// TODO
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::Mapped)
					| Some(PyCompletions::Compute)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| None => {}
				}
			}
		}

		let (model, prop, _) = some!(self.attribute_at_offset(offset, root, contents));
		self.jump_def_property_name(&prop, model)
	}
	/// Resolves the attribute and the object's model at the cursor offset
	/// using [`model_of_range`][Index::model_of_range].
	///
	/// Returns `(model, property, range)`.
	fn attribute_at_offset<'out>(
		&'out self,
		offset: usize,
		root: Node<'out>,
		contents: &'out [u8],
	) -> Option<(&'out str, Cow<'out, str>, core::ops::Range<usize>)> {
		let (lhs, field, range) = Self::attribute_node_at_offset(offset, root, contents)?;
		let model = (self.index).model_of_range(root, lhs.byte_range().map_unit(ByteOffset), contents)?;
		Some((_R(model), field, range))
	}
	/// Resolves the attribute at the cursor offset.
	/// Returns `(object, field, range)`
	#[instrument(level = "trace", skip_all, ret)]
	pub fn attribute_node_at_offset<'out>(
		mut offset: usize,
		root: Node<'out>,
		contents: &'out [u8],
	) -> Option<(Node<'out>, Cow<'out, str>, core::ops::Range<usize>)> {
		if contents.is_empty() {
			return None;
		}
		offset = offset.clamp(0, contents.len() - 1);
		let mut cursor_node = root.descendant_for_byte_range(offset, offset)?;
		let mut real_offset = None;
		if cursor_node.is_named() && !matches!(cursor_node.kind(), "attribute" | "identifier") {
			// We got our cursor left in the middle of nowhere.
			real_offset = Some(offset);
			offset = offset.saturating_sub(1);
			cursor_node = root.descendant_for_byte_range(offset, offset)?;
		}
		trace!(
			"(attribute_node_to_offset) {} cursor={}\n  sexp={}",
			String::from_utf8_lossy(&contents[cursor_node.byte_range()]),
			contents[offset] as char,
			cursor_node.to_sexp(),
		);
		let lhs;
		let rhs;
		let mut on_dot = false;
		if !cursor_node.is_named() {
			on_dot = true;
			// We landed on one of the punctuations inside the attribute.
			// Need to determine which one it is.
			let dot = cursor_node.descendant_for_byte_range(offset, offset)?;
			lhs = dot.prev_named_sibling()?;
			rhs = dot.next_named_sibling().and_then(|attr| match attr.kind() {
				"identifier" => Some(attr),
				"attribute" => attr.child_by_field_name("attribute"),
				_ => None,
			});
		} else if cursor_node.kind() == "attribute" {
			lhs = cursor_node.child_by_field_name("object")?;
			rhs = cursor_node.child_by_field_name("attribute");
		} else {
			match cursor_node.parent() {
				Some(parent) if parent.kind() == "attribute" => {
					lhs = parent.child_by_field_name("object")?;
					rhs = Some(cursor_node);
				}
				Some(parent) if parent.kind() == "ERROR" => {
					// (ERROR (_) @cursor_node)
					lhs = cursor_node;
					rhs = None;
				}
				_ => return None,
			}
		}
		trace!(
			"(attribute_node_to_offset) lhs={} rhs={:?}",
			String::from_utf8_lossy(&contents[lhs.byte_range()]),
			rhs.as_ref()
				.map(|rhs| String::from_utf8_lossy(&contents[rhs.byte_range()])),
		);
		if lhs == cursor_node {
			// We shouldn't recurse into cursor_node itself.
			return None;
		}
		let Some(rhs) = rhs else {
			// In single-expression mode, rhs could be empty in which case
			// we return an empty needle/range.
			let offset = real_offset.unwrap_or(offset) + if on_dot { 1 } else { 0 };
			return Some((lhs, Cow::from(""), offset..offset));
		};
		let (field, range) = if rhs.range().start_point.row != lhs.range().end_point.row {
			// tree-sitter has an issue with attributes spanning multiple lines
			// which is NOT valid Python, but allows it anyways because tree-sitter's
			// use cases don't require strict syntax trees.
			let offset = real_offset.unwrap_or(offset);
			(Cow::from(""), offset..offset)
		} else {
			let range = rhs.byte_range();
			(String::from_utf8_lossy(&contents[range.clone()]), range)
		};

		Some((lhs, field, range))
	}
	pub fn python_references(&self, params: ReferenceParams, rope: Rope) -> anyhow::Result<Option<Vec<Location>>> {
		let ByteOffset(offset) = some!(position_to_offset(params.text_document_position.position, &rope));
		let uri = &params.text_document_position.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let mut cursor = tree_sitter::QueryCursor::new();
		let path = some!(params.text_document_position.text_document.uri.to_file_path());
		let current_module = self.index.module_of_path(&path);
		let mut this_model = ThisModel::default();

		'match_: for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let Some(slice) = rope.get_byte_slice(range.clone()) else {
							dbg!(&range);
							break 'match_;
						};
						let slice = Cow::from(slice);
						return self.record_references(&path, &slice, current_module);
					}
					Some(PyCompletions::Model) => {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::Prop as _)
							.next()
							.map(|prop| matches!(&contents[prop.byte_range()], b"_name" | b"_inherit"))
							.unwrap_or(true);
						if is_meta && range.contains(&offset) {
							let range = range.shrink(1);
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							let slice = some!(_G(slice));
							return self.model_references(&path, &slice.into());
						} else if range.end < offset {
							this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
						}
					}
					Some(PyCompletions::Compute) => {
						let range = capture.node.byte_range();
						if !range.contains(&offset) {
							continue;
						}

						let model = String::from_utf8_lossy(some!(this_model.inner.as_ref()));
						let prop = String::from_utf8_lossy(&contents[range.shrink(1)]);
						return self.method_references(&prop, &model);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::Mapped)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::FieldDescriptor)
					| None => {}
				}
			}
		}

		let (model, prop, _) = some!(self.attribute_at_offset(offset, root, contents));
		self.method_references(&prop, model)
	}

	pub fn python_hover(&self, params: HoverParams, rope: Rope) -> anyhow::Result<Option<Hover>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let ast = self
			.ast_map
			.get(uri.path().as_str())
			.ok_or_else(|| errloc!("Did not build AST for {}", uri.path().as_str()))?;
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position_params.position, &rope) else {
			Err(errloc!("could not find offset for {}", uri.path().as_str()))?
		};

		let contents = Cow::from(rope.clone());
		let contents = contents.as_bytes();
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut this_model = ThisModel::default();
		'match_: for match_ in cursor.matches(query, root, contents) {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::Model) => {
						if range.contains(&offset) {
							let range = range.shrink(1);
							let lsp_range = ts_range_to_lsp_range(capture.node.range());
							let Some(slice) = rope.get_byte_slice(range.clone()) else {
								dbg!(&range);
								break 'match_;
							};
							let slice = Cow::from(slice);
							return self.hover_model(&slice, Some(lsp_range), false, None);
						} else if range.end < offset {
							this_model.tag_model(capture.node, &match_, root.byte_range(), contents);
						}
					}
					Some(PyCompletions::Mapped | PyCompletions::Compute) if range.contains(&offset) => {
						let mapped = some!(self.gather_mapped(
							root,
							&match_,
							Some(offset),
							range.clone(),
							this_model.inner,
							contents,
							false,
						));
						let mut needle = mapped.needle.as_ref();
						let mut model = _I(mapped.model);
						let mut range = mapped.range;
						if !mapped.single_field {
							some!(self
								.index
								.models
								.resolve_mapped(&mut model, &mut needle, Some(&mut range))
								.ok());
						}
						let model = _R(model);
						return self.hover_property_name(needle, model, offset_range_to_lsp_range(range, rope.clone()));
					}
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let xml_id = String::from_utf8_lossy(&contents[range.clone().shrink(1)]);
						return self.hover_record(&xml_id, offset_range_to_lsp_range(range.map_unit(ByteOffset), rope));
					}
					Some(PyCompletions::FieldDescriptor) => {
						// TODO
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::Mapped)
					| Some(PyCompletions::Compute)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| None => {}
				}
			}
		}
		if let Some((model, prop, range)) = self.attribute_at_offset(offset, root, contents) {
			let lsp_range = offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone());
			return self.hover_property_name(&prop, model, lsp_range);
		}

		// No matches, assume arbitrary expression.
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let lsp_range = ts_range_to_lsp_range(needle.range());
		let (type_, scope) =
			some!((self.index).type_of_range(root, needle.byte_range().map_unit(ByteOffset), contents));
		if let Some(model) = self.index.try_resolve_model(&type_, &scope) {
			let model = _R(model);
			let identifier =
				(needle.kind() == "identifier").then(|| String::from_utf8_lossy(&contents[needle.byte_range()]));
			return self.hover_model(model, Some(lsp_range), true, identifier.as_deref());
		}

		// not a model! we only have so many things we can hover...
		match type_ {
			Type::Method(model, method) => self.hover_property_name(&method, _R(model), Some(lsp_range)),
			_ => Ok(None),
		}
	}
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
						if matches!(descriptor, b"comodel_name" | b"domain") {
							field_descriptors.push((descriptor, desc_value));
						}
					}
					Some(mapstr @ (PyCompletions::Mapped | PyCompletions::Compute)) => {
						self.diagnose_mapped(
							rope,
							diagnostics,
							contents,
							root,
							this_model.inner,
							&match_,
							capture.node.byte_range(),
							matches!(mapstr, PyCompletions::Mapped),
						);
					}
					Some(PyCompletions::Scope) => {
						if !in_active_root(capture.node.byte_range()) {
							continue;
						}
						// Most of these steps are similar to what is done inside model_of_range.
						let offset = capture.node.start_byte();
						let Some((self_type, fn_scope, self_param)) = determine_scope(root, contents, offset) else {
							continue;
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
							if prop.starts_with('_')
								|| MODEL_BUILTINS.contains(&prop)
								|| MODEL_METHODS.contains(prop.as_bytes())
							{
								return ControlFlow::Continue(entered);
							}

							let Some(lhs_t) =
								(self.index).type_of(node.child_by_field_name("object").unwrap(), scope, contents)
							else {
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

	fn diagnose_mapped(
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
		}) = self.gather_mapped(root, match_, None, mapped_range, model, contents, false)
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

	#[allow(clippy::unused_async)] // reason: custom method
	pub(crate) async fn debug_inspect_type(
		&self,
		params: TextDocumentPositionParams,
	) -> tower_lsp_server::jsonrpc::Result<Option<String>> {
		let uri = &params.text_document.uri;
		let document = some!(self.document_map.get(uri.path().as_str()));
		let ast = some!(self.ast_map.get(uri.path().as_str()));
		let rope = &document.rope;
		let contents = Cow::from(rope);
		let ByteOffset(offset) = some!(position_to_offset(params.position, rope));
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let (type_, _) =
			some!((self.index).type_of_range(root, needle.byte_range().map_unit(ByteOffset), contents.as_bytes()));
		Ok(Some(format!("{type_:?}").replacen("Text::", "", 1)))
	}
}

#[derive(Default, Clone)]
struct ThisModel<'a> {
	inner: Option<&'a [u8]>,
	source: ThisModelKind,
	top_level_range: core::ops::Range<usize>,
}

#[derive(Default, Clone, Copy)]
enum ThisModelKind {
	Primary,
	#[default]
	Inherited,
}

impl<'this> ThisModel<'this> {
	/// Call this on captures of index [`PyCompletions::Model`].
	fn tag_model(
		&mut self,
		model: Node,
		match_: &QueryMatch,
		top_level_range: core::ops::Range<usize>,
		contents: &'this [u8],
	) {
		debug_assert_eq!(model.kind(), "string");
		let (is_name, mut is_inherit) = match_
			.nodes_for_capture_index(PyCompletions::Prop as _)
			.next()
			.map(|prop| {
				let prop = &contents[prop.byte_range()];
				(prop == b"_name", prop == b"_inherit")
			})
			.unwrap_or((false, false));
		let top_level_changed = top_level_range != self.top_level_range;
		// If still in same class AND _name already declared, skip.
		is_inherit = is_inherit && (top_level_changed || matches!(self.source, ThisModelKind::Inherited));
		if is_inherit {
			let parent = model.parent().expect(format_loc!("(tag_model) parent"));
			// _inherit = '..' OR _inherit = ['..']
			is_inherit = parent.kind() == "assignment" || parent.kind() == "list" && parent.named_child_count() == 1;
		}
		if is_inherit || is_name && top_level_changed {
			self.inner = Some(&contents[model.byte_range().shrink(1)]);
			self.top_level_range = top_level_range;
			if is_name {
				self.source = ThisModelKind::Primary;
			} else if is_inherit {
				self.source = ThisModelKind::Inherited;
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use odoo_lsp::model::ModelProperties;
	use pretty_assertions::assert_eq;

	/// Tricky behavior here. The query syntax must match the trailing comma between
	/// named arguments, and this test checks that. Furthermore, @help cannot be matched
	/// as a `(string)` since that would reify its shape and refuse subsequent matches.
	#[test]
	fn test_model_fields() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = br#"
class Foo(models.Model):
	foo = fields.Char('asd', help='asd')
	bar = fields.Many2one(comodel_name='asd', help='asd')

	@property
	def foobs(self):
		...

	haha = fields.Many2many('asd')
	what = fields.What(asd)

	def passer(self):
		...

	html = fields.Html(related='asd', foo=123, help='asdf')
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = ModelProperties::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[
			&["foo", "fields", "Char", "'asd'", "help", "'asd'"],
			&["bar", "fields", "Many2one", "comodel_name", "'asd'", "help", "'asd'"],
			&["def foobs(self):\n\t\t...", "foobs"],
			&["haha", "fields", "Many2many", "'asd'"],
			&["what", "fields", "What"],
			&["def passer(self):\n\t\t...", "passer"],
			&[
				"html", "fields", "Html", "related", "'asd'", "foo", "123", "help", "'asdf'",
			],
		];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}

	#[test]
	fn test_py_completions() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = br#"
self.env.ref('ref')
env['model']
request.render('template')
foo = fields.Char()
bar = fields.Many2one('positional', string='blah', domain="[('foo', '=', 'bar')]")
baz = fields.Many2many(comodel_name='named', domain=[('foo', '=', bar)])
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = PyCompletions::query();
		let mut cursor = QueryCursor::new();
		let expected = vec![
			(0, vec!["env", "ref", "'ref'"]),
			(1, vec!["env", "'model'"]),
			(0, vec!["request", "render", "'template'"]),
			(4, vec!["fields", "Many2one", "'positional'", "string", "domain"]),
			(4, vec!["fields", "Many2many", "comodel_name", "domain"]),
		];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				(
					match_.pattern_index,
					match_
						.captures
						.iter()
						.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
						.collect::<Vec<_>>(),
				)
			})
			.collect::<Vec<_>>();
		let actual = actual
			.iter()
			.map(|(index, captures)| (*index, captures.iter().map(|x| x.as_ref()).collect::<Vec<_>>()))
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}

	#[test]
	fn test_py_completions_class_scoped() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = br#"
class Foo(models.AbstractModel):
	_name = 'foo'
	_inherit = ['inherit_foo', 'inherit_bar']

	foo = fields.Char(related='related')

	@api.constrains('mapped', 'meh')
	def foo(self):
		what = self.sudo().mapped('ha.ha')

	foo = fields.Foo()

	@api.depends_context('uid')
	@api.depends('mapped2', 'mapped3')
	def another(self):
		pass

	def no_decorators(self):
		pass
"#;
		let ast = parser.parse(contents, None).unwrap();
		let query = PyCompletions::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[
			&["_name", "'foo'"],
			&["_inherit", "'inherit_foo'", "'inherit_bar'"],
			&["foo", "fields", "related", "'related'"],
			// api.constrains('mapped', 'meh')
			&["api", "constrains", "'mapped'"],
			&["api", "constrains", "'meh'"],
			// scope detection with no .depends
			// note that it goes later
			&["self.sudo()", "mapped", "'ha.ha'"],
			&["api.constrains('mapped', 'meh')", "<scope>"],
			&["foo", "fields"],
			// scope detection with both .depends and non-.depends
			// first, each of the original MAPPED rules are triggered
			&["api", "depends", "'mapped2'"],
			&["api", "depends", "'mapped3'"],
			&["api", "depends", "'mapped2'", "'mapped3'", "<scope>"],
			// no decorators
			&["<scope>"],
		];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.map(|capture| match PyCompletions::from(capture.index) {
						Some(PyCompletions::Scope) => Cow::from("<scope>"),
						_ => String::from_utf8_lossy(&contents[capture.node.byte_range()]),
					})
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}

	#[test]
	fn test_attribute_node_at_offset() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = "foo.mapped(lambda f: f.bar)";
		let offset = contents.find("bar").unwrap();
		let contents = contents.as_bytes();
		let ast = parser.parse(contents, None).unwrap();
		let (object, field, range) = Backend::attribute_node_at_offset(offset, ast.root_node(), contents).unwrap();
		assert_eq!(&contents[object.byte_range()], b"f");
		assert_eq!(field.as_ref(), "bar");
		assert_eq!(&contents[range], b"bar");
	}

	#[test]
	fn test_attribute_at_offset_2() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = "super().powerful()";
		let offset = contents.find("powerful").unwrap();
		let contents = contents.as_bytes();
		let ast = parser.parse(contents, None).unwrap();
		let (object, field, range) = Backend::attribute_node_at_offset(offset, ast.root_node(), contents).unwrap();
		assert_eq!(&contents[object.byte_range()], b"super()");
		assert_eq!(field.as_ref(), "powerful");
		assert_eq!(&contents[range], b"powerful");
	}
}
