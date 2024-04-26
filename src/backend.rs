//! This module contains all leaf methods for [Backend] that are not suitable
//! for inclusion in [main](super).
//!
//! This is the final destination in the flowchart.

use std::borrow::Cow;
use std::path::Path;
use std::sync::atomic::Ordering::Relaxed;
use std::sync::atomic::{AtomicBool, AtomicUsize};

use dashmap::{DashMap, DashSet};
use fomat_macros::fomat;
use globwalk::FileType;
use lasso::Spur;
use log::{debug, info, warn};
use miette::{diagnostic, miette};
use odoo_lsp::component::{Prop, PropDescriptor};
use ropey::Rope;
use serde_json::Value;
use tower_lsp::lsp_types::*;
use tower_lsp::Client;
use tree_sitter::{Parser, Tree};

use odoo_lsp::config::{Config, ModuleConfig, ReferencesConfig, SymbolsConfig};
use odoo_lsp::index::{interner, Component, Index, Interner, ModuleName, RecordId, Symbol, SymbolSet};
use odoo_lsp::model::{Field, FieldKind, ModelEntry, ModelLocation, ModelName};
use odoo_lsp::record::Record;
use odoo_lsp::{format_loc, some, utils::*};

pub struct Backend {
	pub client: Client,
	pub document_map: DashMap<String, Document>,
	pub record_ranges: DashMap<String, Box<[ByteRange]>>,
	pub ast_map: DashMap<String, Tree>,
	pub index: Index,
	pub roots: DashSet<String>,
	pub capabilities: Capabilities,
	pub root_setup: CondVar,
	pub symbols_limit: AtomicUsize,
	pub references_limit: AtomicUsize,
}

#[derive(Debug, Default)]
pub struct Capabilities {
	pub dynamic_config: AtomicBool,
	/// Whether the client is expected to explicitly request for diagnostics.
	pub pull_diagnostics: AtomicBool,
}

pub struct TextDocumentItem {
	pub uri: Url,
	pub text: Text,
	pub version: i32,
	pub language: Option<Language>,
	pub old_rope: Option<Rope>,
	pub open: bool,
}

pub enum Text {
	Full(String),
	Delta(Vec<TextDocumentContentChangeEvent>),
}

#[derive(Debug)]
pub enum Language {
	Python,
	Xml,
	Javascript,
}

pub struct Document {
	pub rope: Rope,
	/// Might be outdated.
	pub diagnostics_cache: Vec<Diagnostic>,
	/// The current range of unsynced changes.
	pub damage_zone: Option<ByteRange>,
}

impl Document {
	pub fn new(rope: Rope) -> Self {
		Self {
			rope,
			diagnostics_cache: vec![],
			damage_zone: None,
		}
	}
}

impl Backend {
	pub const LIMIT: usize = 80;

	/// Maximum number of descendants to show in docstring.
	const INHERITS_LIMIT: usize = 3;

	/// Maximum file line count to process diagnostics each on_change
	pub const DIAGNOSTICS_LINE_LIMIT: usize = 1200;

	pub fn find_root_of(&self, path: &str) -> Option<String> {
		for root_ in self.roots.iter() {
			if path.starts_with(root_.key()) {
				return Some(root_.key().to_string());
			}
		}
		None
	}

	pub async fn on_change(&self, params: TextDocumentItem) -> miette::Result<()> {
		let split_uri = params.uri.path().rsplit_once('.');
		let mut document = self
			.document_map
			.try_get_mut(params.uri.path())
			.expect(format_loc!("deadlock"))
			.expect(format_loc!("(on_change) did not build document"));
		match &params.text {
			Text::Full(full) => {
				document.rope = ropey::Rope::from_str(full);
				document.damage_zone = None;
			}
			Text::Delta(_) => {
				// Rope updates are handled by did_change
			}
		}
		let root = self
			.find_root_of(params.uri.path())
			.ok_or_else(|| miette!("file not under any root"))?;
		let root = interner().get_or_intern(&root);
		let rope = document.rope.clone();
		let eager_diagnostics = self.eager_diagnostics(params.open, &rope);
		match (split_uri, params.language) {
			(Some((_, "py")), _) | (_, Some(Language::Python)) => {
				self.on_change_python(&params.text, &params.uri, rope.clone(), params.old_rope)?;
				if eager_diagnostics {
					self.root_setup.wait().await;
					self.diagnose_python(
						params.uri.path(),
						&rope,
						params.text.damage_zone(&rope, None),
						&mut document.diagnostics_cache,
					);
				} else {
					document.damage_zone = params.text.damage_zone(&rope, document.damage_zone.take());
				}
			}
			(Some((_, "xml")), _) | (_, Some(Language::Xml)) => {
				self.update_xml(root, &params.text, &params.uri, &rope, false).await?;
			}
			(Some((_, "js")), _) | (_, Some(Language::Javascript)) => {
				self.on_change_js(&params.text, &params.uri, rope, params.old_rope)?;
			}
			other => return Err(miette!("Unhandled language: {other:?}")),
		}

		if eager_diagnostics {
			self.client
				.publish_diagnostics(params.uri, document.diagnostics_cache.clone(), Some(params.version))
				.await;
		}

		Ok(())
	}
	pub async fn did_save_impl(&self, params: DidSaveTextDocumentParams) -> miette::Result<()> {
		let uri = params.text_document.uri;
		debug!("{}", uri.path());
		let (_, extension) = uri.path().rsplit_once('.').ok_or_else(|| diagnostic!("no extension"))?;
		let root = self
			.find_root_of(uri.path())
			.ok_or_else(|| diagnostic!("out of root"))?;
		let root = interner().get_or_intern(&root);

		let mut document = self
			.document_map
			.try_get_mut(uri.path())
			.expect(format_loc!("deadlock"))
			.ok_or_else(|| diagnostic!("(did_save) did not build document"))?;

		match extension {
			"py" => self.did_save_python(uri, root, &mut document).await,
			"xml" => self.did_save_xml(uri, root, &mut document).await?,
			_ => {}
		}
		Ok(())
	}
	pub async fn did_save_python(&self, uri: Url, root: Spur, document: &mut Document) {
		let path = uri.path();
		let zone = document.damage_zone.take();
		let rope = &document.rope;
		let text = Cow::from(rope);
		_ = self
			.update_models(Text::Full(text.into_owned()), path, root, rope.clone())
			.await
			.inspect_err(|err| warn!("{err:?}"));
		if zone.is_some() {
			debug!("diagnostics");
			self.diagnose_python(path, &document.rope.clone(), zone, &mut document.diagnostics_cache);
			self.client
				.publish_diagnostics(uri, document.diagnostics_cache.clone(), None)
				.await;
		}
	}
	pub async fn did_save_xml(&self, uri: Url, root: Spur, document: &mut Document) -> miette::Result<()> {
		self.update_xml(root, &Text::Delta(vec![]), &uri, &document.rope, true)
			.await?;
		Ok(())
	}
	/// Whether diagnostics should be processed/pushed with each `on_change`.
	pub fn eager_diagnostics(&self, open: bool, rope: &Rope) -> bool {
		!self.capabilities.pull_diagnostics.load(Relaxed) && (open || rope.len_lines() < Self::DIAGNOSTICS_LINE_LIMIT)
	}
	pub fn update_ast(
		&self,
		text: &Text,
		uri: &Url,
		rope: Rope,
		old_rope: Option<Rope>,
		mut parser: Parser,
	) -> miette::Result<()> {
		let ast = self.ast_map.try_get_mut(uri.path()).expect(format_loc!("deadlock"));
		let ast = match (text, ast) {
			(Text::Full(full), _) => parser.parse(full, None),
			(Text::Delta(delta), Some(mut ast)) => {
				let old_rope = old_rope.ok_or_else(|| diagnostic!("delta requires old rope"))?;
				for change in delta {
					// TODO: Handle full text changes in delta
					let range = change.range.ok_or_else(|| diagnostic!("delta without range"))?;
					let start = position_to_offset(range.start, &rope).ok_or_else(|| diagnostic!("delta start"))?;
					// The old rope is used to calculate the *old* range-end, because
					// the diff may have caused it to fall out of the new rope's bounds.
					let end = position_to_offset(range.end, &old_rope).ok_or_else(|| diagnostic!("delta end"))?;
					let len_new = change.text.len();
					let start_position = tree_sitter::Point {
						row: range.start.line as usize,
						column: range.start.character as usize,
					};
					let old_end_position = tree_sitter::Point {
						row: range.end.line as usize,
						column: range.end.character as usize,
					};
					// calculate new_end_position using rope
					let new_end_offset = ByteOffset(start.0 + len_new);
					let new_end_position = offset_to_position(new_end_offset, rope.clone())
						.ok_or_else(|| diagnostic!("new_end_position"))?;
					let new_end_position = tree_sitter::Point {
						row: new_end_position.line as usize,
						column: new_end_position.character as usize,
					};
					// let it rip 🚀
					ast.edit(&tree_sitter::InputEdit {
						start_byte: start.0,
						old_end_byte: end.0,
						new_end_byte: start.0 + len_new,
						start_position,
						old_end_position,
						new_end_position,
					});
				}
				let slice = Cow::from(rope.slice(..));
				parser.parse(slice.as_bytes(), Some(&ast))
			}
			(Text::Delta(_), None) => Err(diagnostic!("(update_ast) got delta but no ast"))?,
		};
		let ast = ast.ok_or_else(|| diagnostic!("No AST was parsed"))?;
		self.ast_map.insert(uri.path().to_string(), ast);
		Ok(())
	}
	pub async fn complete_xml_id(
		&self,
		needle: &str,
		range: ByteRange,
		rope: Rope,
		model_filter: Option<&str>,
		current_module: ModuleName,
		items: &mut MaxVec<CompletionItem>,
	) -> miette::Result<()> {
		if !items.has_space() {
			return Ok(());
		}
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("(complete_xml_id) range"))?;
		let by_prefix = self.index.records.by_prefix.read().await;
		let model_filter = model_filter.and_then(|model| interner().get(model).map(ModelName::from));
		fn to_completion_items(
			record: &Record,
			current_module: ModuleName,
			range: Range,
			scoped: bool,
			interner: &Interner,
		) -> CompletionItem {
			let label = if record.module == current_module && !scoped {
				record.id.to_string()
			} else {
				record.qualified_id(interner)
			};
			let model = record.model.as_ref().map(|model| interner.resolve(model).to_string());
			CompletionItem {
				text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
					new_text: label.clone(),
					insert: range,
					replace: range,
				})),
				label,
				detail: model,
				kind: Some(CompletionItemKind::REFERENCE),
				..Default::default()
			}
		}
		let interner = interner();
		if let Some((module, needle)) = needle.split_once('.') {
			let Some(module) = interner.get(module).map(Into::into) else {
				return Ok(());
			};
			let completions = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, keys)| {
				keys.iter().flat_map(|key| {
					self.index.records.get(key).and_then(|record| {
						(record.module == module && (model_filter.is_none() || record.model == model_filter))
							.then(|| to_completion_items(&record, current_module, range, true, interner))
					})
				})
			});
			items.extend(completions);
		} else {
			let completions = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, keys)| {
				keys.iter().flat_map(|key| {
					self.index.records.get(key).and_then(|record| {
						(model_filter.is_none() || record.model == model_filter)
							.then(|| to_completion_items(&record, current_module, range, false, interner))
					})
				})
			});
			items.extend(completions);
		}
		Ok(())
	}
	pub async fn complete_field_name(
		&self,
		needle: &str,
		range: ByteRange,
		model: String,
		rope: Rope,
		items: &mut MaxVec<CompletionItem>,
	) -> miette::Result<()> {
		debug!("needle=`{needle}` model=`{model}`");
		if !items.has_space() {
			return Ok(());
		}
		let model_key = interner().get_or_intern(&model);
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("range"))?;
		let Some(model_entry) = self.index.models.populate_field_names(model_key.into(), &[]) else {
			return Ok(());
		};
		let completions = model_entry
			.fields_set
			.iter_prefix(needle.as_bytes())
			.map(|(field_name, _)| {
				// SAFETY: only utf-8 bytestrings from interner() are allowed
				let field_name = unsafe { core::str::from_utf8_unchecked(field_name).to_string() };
				CompletionItem {
					text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
						new_text: field_name.clone(),
						insert: range,
						replace: range,
					})),
					label: field_name,
					kind: Some(CompletionItemKind::FIELD),
					// TODO: Make this type-safe
					data: Some(Value::String(model.clone())),
					..Default::default()
				}
			});
		items.extend(completions);
		Ok(())
	}
	pub async fn complete_model(
		&self,
		needle: &str,
		range: ByteRange,
		rope: Rope,
		items: &mut MaxVec<CompletionItem>,
	) -> miette::Result<()> {
		if !items.has_space() {
			return Ok(());
		}
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("(complete_model) range"))?;
		let by_prefix = self.index.models.by_prefix.read().await;
		let matches = by_prefix
			.iter_prefix(needle.as_bytes())
			.flat_map(|(_, key)| self.index.models.get(key))
			.map(|model| {
				let label = interner().resolve(model.key()).to_string();
				let module = model.base.as_ref().and_then(|base| {
					let module = self.index.module_of_path(&base.0.path.as_path())?;
					Some(interner().resolve(&module).to_string())
				});
				CompletionItem {
					text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
						new_text: label.clone(),
						insert: range,
						replace: range,
					})),
					label,
					detail: module,
					kind: Some(CompletionItemKind::CLASS),
					..Default::default()
				}
			});
		items.extend(matches);
		Ok(())
	}
	pub async fn complete_template_name(
		&self,
		needle: &str,
		range: ByteRange,
		rope: Rope,
		items: &mut MaxVec<CompletionItem>,
	) -> miette::Result<()> {
		if !items.has_space() {
			return Ok(());
		}
		let range =
			offset_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("(complete_template_name) range"))?;
		let interner = interner();
		let by_prefix = self.index.templates.by_prefix.read().await;
		let matches = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, templates)| {
			templates.iter().flat_map(|key| {
				let label = interner.resolve(key).to_string();
				Some(CompletionItem {
					text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
						new_text: label.clone(),
						insert: range,
						replace: range,
					})),
					label,
					kind: Some(CompletionItemKind::REFERENCE),
					..Default::default()
				})
			})
		});
		items.extend(matches);
		Ok(())
	}
	pub fn complete_component_prop(
		&self,
		// needle: &str,
		range: ByteRange,
		rope: Rope,
		component: &str,
		items: &mut MaxVec<CompletionItem>,
	) -> miette::Result<()> {
		let component = interner()
			.get(component)
			.ok_or_else(|| diagnostic!("(complete_component_prop) component"))?;
		let component = self
			.index
			.components
			.get(&component.into())
			.ok_or_else(|| diagnostic!("component"))?;
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("(complete_prop_of) range"))?;
		let completions = component.props.iter().filter_map(|(prop, desc)| {
			let prop = interner().resolve(&prop);
			Some(CompletionItem {
				text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
					new_text: prop.to_string(),
					insert: range,
					replace: range,
				})),
				label: prop.to_string(),
				kind: Some(CompletionItemKind::PROPERTY),
				detail: Some(format!("{:?}", desc.type_)),
				..Default::default()
			})
		});
		items.extend(completions);
		Ok(())
	}
	pub fn jump_def_xml_id(&self, cursor_value: &str, uri: &Url) -> miette::Result<Option<Location>> {
		let mut value = Cow::from(cursor_value);
		if !value.contains('.') {
			'unscoped: {
				if let Some(module) = self.index.module_of_path(Path::new(uri.path())) {
					value = format!("{}.{value}", interner().resolve(&module)).into();
					break 'unscoped;
				}
				debug!(
					"Could not find a reference for {} in {}: could not infer module",
					cursor_value,
					uri.path()
				);
				return Ok(None);
			}
		}
		let record_id = some!(interner().get(value));
		return Ok((self.index.records.get(&record_id.into())).map(|record| record.location.clone().into()));
	}
	pub fn jump_def_model(&self, model: &str) -> miette::Result<Option<Location>> {
		let model = some!(interner().get(model));
		match (self.index.models.get(&model.into())).and_then(|model| model.base.as_ref().cloned()) {
			Some(ModelLocation(base, _)) => Ok(Some(base.into())),
			None => Ok(None),
		}
	}
	pub fn hover_model(
		&self,
		model_str_key: &str,
		range: Option<Range>,
		definition: bool,
		identifier: Option<&str>,
	) -> miette::Result<Option<Hover>> {
		let model_key = some!(interner().get(model_str_key));
		let model = some!(self.index.models.get(&model_key.into()));
		Ok(Some(Hover {
			range,
			contents: HoverContents::Markup(MarkupContent {
				kind: MarkupKind::Markdown,
				value: self.model_docstring(&model, definition.then_some(model_str_key), identifier),
			}),
		}))
	}
	pub fn hover_component(&self, name: &str, range: Option<Range>) -> Option<Hover> {
		let key = interner().get(name)?;
		let component = self.index.components.get(&key.into())?;
		let module = component
			.location
			.as_ref()
			.and_then(|loc| self.index.module_of_path(&loc.path.as_path()));
		let value = fomat!(
			"```js\n"
			"(component) class " (name) ";\n"
			"```"
			if let Some(module) = module {
				"\n*Defined in:* `" (interner().resolve(&module)) "`"
			}
		);
		Some(Hover {
			contents: HoverContents::Scalar(MarkedString::String(value)),
			range,
		})
	}
	pub fn hover_template(&self, name: &str, range: Option<Range>) -> Option<Hover> {
		let key = interner().get(name)?;
		let template = self.index.templates.get(&key.into())?;
		let module = template
			.location
			.as_ref()
			.and_then(|loc| self.index.module_of_path(&loc.path.as_path()));
		let value = fomat!(
			"```xml\n"
			"<t t-name=\"" (name) "\"/>\n"
			"```"
			if let Some(module) = module {
				"\n*Defined in:* `" (interner().resolve(&module)) "`"
			}
		);
		Some(Hover {
			contents: HoverContents::Scalar(MarkedString::String(value)),
			range,
		})
	}
	pub async fn jump_def_field_name(&self, field: &str, model: &str) -> miette::Result<Option<Location>> {
		let model_key = interner().get_or_intern(model);
		let entry = some!(self.index.models.populate_field_names(model_key.into(), &[]));
		let field = some!(interner().get(field));
		let field = some!(entry.fields.as_ref()).get(&field.into());
		Ok(Some(some!(field).location.clone().into()))
	}
	pub fn jump_def_template_name(&self, name: &str) -> miette::Result<Option<Location>> {
		let name = some!(interner().get(name));
		let entry = some!(self.index.templates.get(&name.into()));
		let location = some!(&entry.value().location);
		Ok(Some(location.clone().into()))
	}
	pub fn jump_def_component_prop(&self, component: &str, prop: &str) -> miette::Result<Option<Location>> {
		let component = some!(interner().get(component));
		let prop = some!(interner().get(prop));
		let prop = some!(self.find_prop_recursive(&component.into(), &prop.into()));
		Ok(Some(prop.location.into()))
	}
	fn find_prop_recursive(&self, component: &Symbol<Component>, prop: &Symbol<Prop>) -> Option<PropDescriptor> {
		let component = self.index.components.get(component)?;
		if let Some(prop) = component.props.get(prop) {
			return Some(prop.clone());
		}
		for ancestor in &component.ancestors {
			if let Some(prop) = self.find_prop_recursive(ancestor, prop) {
				return Some(prop);
			}
		}
		None
	}
	pub async fn hover_field_name(
		&self,
		name: &str,
		model: &str,
		range: Option<Range>,
	) -> miette::Result<Option<Hover>> {
		let model_key = interner().get_or_intern(model);
		let fields = some!(self.index.models.populate_field_names(model_key.into(), &[]));
		let field = some!(interner().get(name));
		let field = some!(fields.fields.as_ref()).get(&field.into());
		Ok(Some(Hover {
			range,
			contents: HoverContents::Markup(MarkupContent {
				kind: MarkupKind::Markdown,
				value: self.field_docstring(name, some!(field), true),
			}),
		}))
	}
	pub fn hover_record(&self, xml_id: &str, range: Option<Range>) -> miette::Result<Option<Hover>> {
		let key = some!(interner().get(xml_id));
		let record = some!(self.index.records.get(&key.into()));
		let model = match record.model.as_ref() {
			Some(model) => interner().resolve(model),
			None => "<unknown>",
		};
		let value = format!("<record id=\"{xml_id}\" model=\"{model}\" />");
		Ok(Some(Hover {
			contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
				language: "xml".to_owned(),
				value,
			})),
			range,
		}))
	}
	pub fn model_references(&self, model: &ModelName) -> miette::Result<Option<Vec<Location>>> {
		let record_locations = self
			.index
			.records
			.by_model(model)
			.map(|record| record.location.clone().into());
		let limit = self.references_limit.load(Relaxed);
		if let Some(entry) = self.index.models.get(model) {
			let inherit_locations = entry.descendants.iter().map(|loc| loc.0.clone().into());
			Ok(Some(inherit_locations.chain(record_locations).take(limit).collect()))
		} else {
			Ok(Some(record_locations.take(limit).collect()))
		}
	}
	pub fn record_references(
		&self,
		inherit_id: &str,
		current_module: Option<ModuleName>,
	) -> miette::Result<Option<Vec<Location>>> {
		let interner = &interner();
		let inherit_id = if inherit_id.contains('.') {
			Cow::from(inherit_id)
		} else if let Some(current_module) = current_module {
			Cow::from(format!("{}.{}", interner.resolve(&current_module), inherit_id))
		} else {
			debug!("No current module to resolve the XML ID {inherit_id}");
			return Ok(None);
		};
		let inherit_id = RecordId::from(some!(interner.get(inherit_id)));
		let limit = self.references_limit.load(Relaxed);
		let locations = self
			.index
			.records
			.by_inherit_id(&inherit_id)
			.map(|record| record.location.clone().into())
			.take(limit);
		Ok(Some(locations.collect()))
	}
	pub fn template_references(&self, name: &str, include_definition: bool) -> miette::Result<Option<Vec<Location>>> {
		let name = some!(interner().get(name));
		let template = some!(self.index.templates.get(&name.into()));
		let definition_location = if include_definition {
			template.value().location.clone().map(Location::from)
		} else {
			None
		};
		let descendant_locations = (template.value().descendants)
			.iter()
			.flat_map(|tpl| tpl.location.clone().map(Location::from));
		let mut locations = definition_location
			.into_iter()
			.chain(descendant_locations)
			.collect::<Vec<_>>();

		if let Some(component) = self.index.components.by_template.get(&name.into()) {
			let component = some!(self.index.components.get(&component));
			if let Some(location) = component.location.as_ref() {
				locations.push(location.clone().into());
				let last = locations.swap_remove(0);
				locations.push(last);
			}
		}
		Ok(Some(locations))
	}
	pub fn model_docstring(&self, model: &ModelEntry, model_name: Option<&str>, identifier: Option<&str>) -> String {
		let module = model
			.base
			.as_ref()
			.and_then(|base| self.index.module_of_path(&base.0.path.as_path()));
		let mut descendants = model
			.descendants
			.iter()
			.map(|loc| &loc.0)
			.scan(SymbolSet::default(), |mods, loc| {
				let Some(module) = self.index.module_of_path(&loc.path.as_path()) else {
					return Some(None);
				};
				if mods.insert(module) {
					Some(Some(module))
				} else {
					Some(None)
				}
			})
			.flatten();
		fomat! {
			if let Some(name) = model_name {
				"```python\n"
				if let Some(ident) = identifier { (ident) ": " } "Model[\"" (name) "\"]\n"
				"```  \n"
			}
			if let Some(module) = module {
				"*Defined in:* `" (interner().resolve(&module)) "`  \n"
			}
			for (idx, descendant) in descendants
				.by_ref()
				.take(Self::INHERITS_LIMIT)
				.enumerate()
			{
				if idx == 0 { "*Inherited in:* " } "`" (interner().resolve(&descendant)) "`"
			} sep { ", " }
			match descendants.count() {
				0 => {}
				remaining => { " (+" (remaining) " modules)" }
			}
			if let Some(help) = &model.docstring {
				match help.to_string() {
					empty if empty.is_empty() => {}
					other => {
						"  \n" (other)
					}
				}
			}
		}
	}
	pub fn field_docstring(&self, name: &str, field: &Field, signature: bool) -> String {
		fomat! {
			if signature {
				"```python\n"
				(name) " = fields." (interner().resolve(&field.type_))
				r#"("#
				match &field.kind {
					FieldKind::Value | FieldKind::Related(_) => {}
					FieldKind::Relational(relation) => { "\"" (interner().resolve(relation)) "\", " }
				}
				"…)\n```  \n"
			}
			if let Some(module) = self
				.index
				.module_of_path(&field.location.path.as_path())
			{
				"*Defined in:* `" (interner().resolve(&module)) "`  \n"
			}
			if let Some(help) = &field.help { (help.to_string()) }
		}
	}
	pub async fn on_change_config(&self, config: Config) {
		if let Some(SymbolsConfig { limit: Some(limit) }) = config.symbols {
			self.symbols_limit.store(limit as usize, Relaxed);
		}
		if let Some(ReferencesConfig { limit: Some(limit) }) = config.references {
			self.references_limit.store(limit as usize, Relaxed);
		}
		let Some(ModuleConfig { roots: Some(roots), .. }) = config.module else {
			return;
		};
		if !roots.is_empty() {
			self.roots.clear();
		}
		for root in roots {
			let Ok(root) = Path::new(&root).canonicalize() else {
				continue;
			};
			let root_display = root.to_string_lossy();
			if root_display.contains('*') {
				let Ok(glob) = globwalk::glob_builder(root.to_string_lossy())
					.file_type(FileType::DIR | FileType::SYMLINK)
					.build()
				else {
					continue;
				};
				for dir_entry in glob {
					let Ok(root) = dir_entry else { continue };
					self.roots.insert(root.path().to_string_lossy().to_string());
				}
			} else if tokio::fs::try_exists(&root).await.unwrap_or(false) {
				self.roots.insert(root_display.to_string());
			}
		}
		// self.added_roots_root.store(true, Relaxed);
	}
	pub async fn statistics(&self) -> tower_lsp::jsonrpc::Result<Value> {
		let Self {
			client: _,
			document_map,
			record_ranges,
			ast_map,
			index,
			roots: _,
			capabilities: _,
			root_setup: _,
			symbols_limit: _,
			references_limit: _,
		} = self;
		let interner = interner();
		let symbols_len = interner.len();
		let symbols_usage = interner.current_memory_usage();
		Ok(serde_json::json! {{
			"debug": cfg!(debug_assertions),
			"documents": document_map.usage(),
			"records": record_ranges.usage(),
			"ast": ast_map.usage(),
			"index": index.statistics(),
			"intern": {
				"len": symbols_len,
				"bytes": symbols_usage,
			}
		}})
	}
	pub fn ensure_nonoverlapping_roots(&self) {
		let mut redundant = vec![];
		let mut roots = self.roots.iter().map(|r| r.to_string()).collect::<Vec<_>>();
		roots.sort_unstable_by_key(|root| root.len());
		info!("{roots:?}");
		for lhs in 1..roots.len() {
			for rhs in 0..lhs {
				if roots[lhs].starts_with(&roots[rhs]) {
					redundant.push(&roots[lhs]);
					break;
				}
			}
		}
		if !redundant.is_empty() {
			warn!(
				concat!(
					"The following configured roots are redundant: {:?}\n",
					"Reconfigure your roots to dismiss this warning.\n",
					"Note that in VSCode, .odoo_lsp configs are applied before workspace folders."
				),
				redundant
			);
		}
		for root in redundant {
			self.roots.remove(root);
		}
	}
}

impl Text {
	/// Returns None if not a delta, the resulting range is empty, or if conversion of any range fails.
	fn damage_zone(&self, rope: &Rope, seed: Option<ByteRange>) -> Option<ByteRange> {
		let deltas = match self {
			Self::Full(_) => return None,
			Self::Delta(deltas) => deltas,
		};
		const NULL_RANGE: core::ops::Range<usize> = usize::MAX..usize::MIN;
		let mut out = seed.clone().unwrap_or_else(|| NULL_RANGE.map_unit(ByteOffset)).erase();
		for delta in deltas {
			let Some(range) = delta.range else {
				out = NULL_RANGE;
				continue;
			};
			let range = lsp_range_to_offset_range(range, rope)?;
			out = out.start.min(range.start.0)..out.end.max(range.end.0);
		}
		debug!("(damage_zone)\nseed={seed:?}\n out={out:?}");
		(!out.is_empty()).then(|| out.map_unit(ByteOffset))
	}
}

impl Usage for Document {
	fn usage(&self) -> UsageInfo<Self> {
		let Self {
			rope,
			diagnostics_cache,
			damage_zone: _,
		} = self;
		let mut usage = 0;
		usage += rope.usage().0;
		usage += diagnostics_cache.usage().0;
		UsageInfo::new(usage)
	}
}
