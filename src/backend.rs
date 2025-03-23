//! This module contains all leaf methods for [Backend] that are not suitable
//! for inclusion in [`server`][crate::server]
//!
//! This is the final destination in the flowchart.

use std::borrow::Cow;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering::Relaxed;
use std::sync::atomic::{AtomicBool, AtomicUsize};

use dashmap::DashMap;
use derive_more::{Deref, DerefMut};
use fomat_macros::fomat;
use globwalk::FileType;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use smart_default::SmartDefault;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::Client;
use tracing::{debug, error, info, instrument, warn};
use tree_sitter::{Parser, Tree};

use crate::component::{Prop, PropDescriptor};
use crate::config::{CompletionsConfig, Config, ModuleConfig, ReferencesConfig};
use crate::index::{Component, Index, ModuleName, RecordId, Symbol, SymbolSet, _G, _I, _P, _R};
use crate::model::{Field, FieldKind, Method, ModelEntry, ModelLocation, ModelName, PropertyKind};
use crate::record::Record;
use crate::{errloc, format_loc, some, utils::*};

#[derive(Deref)]
pub struct Backend {
	pub client: Client,
	#[deref]
	pub inner: BackendInner,
}

#[derive(SmartDefault)]
pub struct BackendInner {
	/// fs path -> rope, diagnostics etc.
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub document_map: DashMap<String, Document>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub record_ranges: DashMap<String, Box<[ByteRange]>>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub ast_map: DashMap<String, Tree>,
	pub index: Index,
	pub workspaces: Workspaces,
	pub root_setup: CondVar,
	pub capabilities: Capabilities,
	pub project_config: BackendConfig,
}

#[derive(Debug, Default, Clone)]
pub struct Workspace {
	// pub symbols: SymbolsConfig,
	pub completions: CompletionsConfig,
	pub references: ReferencesConfig,
}

#[derive(SmartDefault)]
pub struct BackendConfig {
	#[default(200.into())]
	pub symbols_limit: AtomicUsize,
	#[default(200.into())]
	pub completions_limit: AtomicUsize,
	#[default(200.into())]
	pub references_limit: AtomicUsize,
}

#[derive(Deref, DerefMut, SmartDefault, Debug)]
pub struct Workspaces {
	#[deref]
	#[default(_code = "DashMap::with_shard_amount(4)")]
	inner: DashMap<PathBuf, Workspace>,
}
impl Workspaces {
	#[inline]
	pub fn find_workspace_of<T>(&self, path: &Path, mut func: impl FnMut(&Path, &Workspace) -> Option<T>) -> Option<T> {
		for ws in &self.inner {
			if path.starts_with(ws.key()) {
				if let Some(res) = func(ws.key(), &ws) {
					return Some(res);
				}
			}
		}
		None
	}
}

#[derive(Debug, Default)]
pub struct Capabilities {
	pub can_notify_changed_config: AtomicBool,
	pub can_notify_changed_watched_files: AtomicBool,
	/// Whether the client is able and expected to explicitly request for diagnostics.
	pub pull_diagnostics: AtomicBool,
	/// Whether [`workspace/workspaceFolders`][request::WorkspaceFoldersRequest] can be called.
	pub workspace_folders: AtomicBool,
}

pub struct TextDocumentItem {
	pub uri: Uri,
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

/// Data provided by completion providers to be consumed by `completion_resolve`
#[derive(Serialize, Deserialize)]
pub struct CompletionData {
	pub model: String,
}

impl Backend {
	/// Maximum number of descendants to show in docstring.
	const INHERITS_LIMIT: usize = 3;

	/// Maximum file line count to process diagnostics each on_change
	pub const DIAGNOSTICS_LINE_LIMIT: usize = 1200;

	#[inline]
	pub fn new(client: Client) -> Self {
		Self {
			client,
			inner: Default::default(),
		}
	}

	pub fn init_workspaces(&self, params: &InitializeParams) {
		let mut workspaces = params
			.workspace_folders
			.as_ref()
			.map(|dirs| {
				dirs.iter()
					.filter_map(|wsdir| {
						let Some(path) = wsdir.uri.to_file_path() else {
							error!("{:?} does not resolve to a proper path, ignoring", wsdir.uri);
							return None;
						};
						Some(path.into_owned())
					})
					.collect::<Vec<_>>()
			})
			.unwrap_or_default();
		if workspaces.is_empty() {
			#[allow(deprecated)]
			if let Some(root) = params
				.root_uri
				.as_ref()
				.and_then(|uri| uri.to_file_path())
				.or_else(|| params.root_path.as_deref().map(Path::new).map(Cow::<Path>::from))
			{
				workspaces.push(root.into_owned());
			}
		}
		if workspaces.is_empty() {
			if let Ok(pwd) = std::env::current_dir() {
				workspaces.push(pwd);
			}
		}
		if workspaces.is_empty() {
			warn!("No workspace folders were detected");
		}

		'outer: for dir in workspaces {
			for choice in [".odoo_lsp", ".odoo_lsp.json"] {
				let workspace = Path::new(&dir);
				let path = workspace.join(choice);
				let Ok(file) = std::fs::File::open(&path) else { continue };
				match serde_json::from_reader(file) {
					Ok(config) => {
						self.on_change_config(config, Some(workspace));
						continue 'outer;
					}
					Err(err) => error!("could not parse config at {}:\n{err}", path.display()),
				}
				break;
			}
			self.workspaces.insert(dir, Workspace::default());
		}

		if let Some(Ok(config)) = params
			.initialization_options
			.as_ref()
			.cloned()
			.map(serde_json::from_value)
		{
			self.on_change_config(config, None);
		}
	}

	#[tracing::instrument(skip_all, ret)]
	pub fn find_root_of(&self, path: &Path) -> Option<PathBuf> {
		for root_ in self.index.roots.iter() {
			if path.starts_with(root_.key()) {
				return Some(root_.key().to_owned());
			}
		}
		None
	}
	#[instrument(skip_all)]
	pub async fn on_change(&self, params: TextDocumentItem) -> anyhow::Result<()> {
		let split_uri = params.uri.path().as_str().rsplit_once('.');
		let rope;
		let path;
		let root;
		let eager_diagnostics;
		{
			let mut document = self
				.document_map
				.get_mut(params.uri.path().as_str())
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
			rope = document.rope.clone();
			path = params.uri.to_file_path().unwrap();
			let root_path = self
				.find_root_of(&path)
				.ok_or_else(|| errloc!("file not under any root"))?;
			root = _P(root_path);
			eager_diagnostics = self.eager_diagnostics(params.open, &rope);
		};
		match (split_uri, params.language) {
			(Some((_, "py")), _) | (_, Some(Language::Python)) => {
				let mut document = self.document_map.get_mut(params.uri.path().as_str()).unwrap();
				self.on_change_python(&params.text, &params.uri, rope.clone(), params.old_rope)?;
				if eager_diagnostics {
					self.diagnose_python(
						params.uri.path().as_str(),
						&rope,
						params.text.damage_zone(&rope, None),
						&mut document.diagnostics_cache,
					);
				} else {
					document.damage_zone = params.text.damage_zone(&rope, document.damage_zone.take());
				}
			}
			(Some((_, "xml")), _) | (_, Some(Language::Xml)) => {
				self.update_xml(root, &params.text, &params.uri, rope, false).await?;
			}
			(Some((_, "js")), _) | (_, Some(Language::Javascript)) => {
				self.on_change_js(&params.text, &params.uri, rope, params.old_rope)?;
			}
			other => return Err(errloc!("Unhandled language: {:?}", other)),
		}

		if eager_diagnostics {
			let diagnostics = {
				self.document_map
					.get(params.uri.path().as_str())
					.unwrap()
					.diagnostics_cache
					.clone()
			};
			self.client
				.publish_diagnostics(params.uri, diagnostics, Some(params.version))
				.await;
		}

		Ok(())
	}
	pub async fn did_save_impl(&self, params: DidSaveTextDocumentParams) -> anyhow::Result<()> {
		let uri = params.text_document.uri;
		debug!("{}", uri.path().as_str());
		let (_, extension) = uri
			.path()
			.as_str()
			.rsplit_once('.')
			.ok_or_else(|| errloc!("no extension"))?;
		let path = uri.to_file_path().unwrap();
		let root = self.find_root_of(&path).ok_or_else(|| errloc!("out of root"))?;
		let root = _P(&root);

		match extension {
			"py" => self.did_save_python(uri, root).await?,
			"xml" => self.did_save_xml(uri, root).await?,
			_ => {}
		}
		Ok(())
	}
	/// Whether diagnostics should be processed/pushed with each `on_change`.
	pub fn eager_diagnostics(&self, open: bool, rope: &Rope) -> bool {
		!self.capabilities.pull_diagnostics.load(Relaxed) && (open || rope.len_lines() < Self::DIAGNOSTICS_LINE_LIMIT)
	}
	#[instrument(skip_all, fields(uri))]
	pub fn update_ast(
		&self,
		text: &Text,
		uri: &Uri,
		rope: Rope,
		old_rope: Option<Rope>,
		mut parser: Parser,
	) -> anyhow::Result<()> {
		let ast = self
			.ast_map
			.try_get_mut(uri.path().as_str())
			.expect(format_loc!("deadlock"));
		let ast = match (text, ast) {
			(Text::Full(full), _) => parser.parse(full, None),
			(Text::Delta(delta), Some(mut ast)) => {
				let old_rope = old_rope.ok_or_else(|| errloc!("delta requires old rope"))?;
				for change in delta {
					// TODO: Handle full text changes in delta
					let range = change.range.ok_or_else(|| errloc!("delta without range"))?;
					let start = position_to_offset(range.start, &rope).ok_or_else(|| errloc!("delta start"))?;
					// The old rope is used to calculate the *old* range-end, because
					// the diff may have caused it to fall out of the new rope's bounds.
					let end = position_to_offset(range.end, &old_rope).ok_or_else(|| errloc!("delta end"))?;
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
					let new_end_position =
						offset_to_position(new_end_offset, rope.clone()).ok_or_else(|| errloc!("new_end_position"))?;
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
			(Text::Delta(_), None) => Err(errloc!("(update_ast) got delta but no ast"))?,
		};
		let ast = ast.ok_or_else(|| errloc!("No AST was parsed"))?;
		self.ast_map.insert(uri.path().as_str().to_string(), ast);
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
	) -> anyhow::Result<()> {
		if !items.has_space() {
			return Ok(());
		}
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| errloc!("(complete_xml_id) range"))?;
		let by_prefix = self.index.records.by_prefix.read().await;
		let model_filter = model_filter.and_then(|model| _G(model).map(ModelName::from));
		fn to_completion_items(
			record: &Record,
			current_module: ModuleName,
			range: Range,
			scoped: bool,
		) -> CompletionItem {
			let label = if record.module == current_module && !scoped {
				record.id.to_string()
			} else {
				record.qualified_id()
			};
			let model = record.model.as_ref().map(|model| _R(*model).to_string());
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
		if let Some((module, needle)) = needle.split_once('.') {
			let Some(module) = _G(module).map(Into::into) else {
				return Ok(());
			};
			let completions = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, keys)| {
				keys.iter().flat_map(|key| {
					self.index.records.get(key).and_then(|record| {
						(record.module == module && (model_filter.is_none() || record.model == model_filter))
							.then(|| to_completion_items(&record, current_module, range, true))
					})
				})
			});
			items.extend(completions);
		} else {
			let completions = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, keys)| {
				keys.iter().flat_map(|key| {
					self.index.records.get(key).and_then(|record| {
						(model_filter.is_none() || record.model == model_filter)
							.then(|| to_completion_items(&record, current_module, range, false))
					})
				})
			});
			items.extend(completions);
		}
		Ok(())
	}
	/// `for_only_prop` should be specified if a particular kind of [property][PropertyKind] should be included,
	/// defaults to any if not specified.
	pub fn complete_property_name(
		&self,
		needle: &str,
		range: ByteRange,
		model: String,
		rope: Rope,
		for_only_prop: Option<PropertyKind>,
		items: &mut MaxVec<CompletionItem>,
	) -> anyhow::Result<()> {
		debug!("needle=`{needle}` model=`{model}`");
		if !items.has_space() {
			return Ok(());
		}
		let model_key = _I(&model);
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| errloc!("range"))?;
		let Some(model_entry) = self.index.models.populate_properties(model_key.into(), &[]) else {
			return Ok(());
		};
		let iter = if needle.is_empty() {
			model_entry.properties_by_prefix.iter()
		} else {
			model_entry.properties_by_prefix.iter_prefix(needle.as_bytes())
		};
		let completions = iter.filter_map(|(property_name, kind)| {
			if for_only_prop.as_ref().is_some_and(|target| target != kind) {
				return None;
			}
			// SAFETY: only utf-8 bytestrings from interner() are allowed
			let label = unsafe { core::str::from_utf8_unchecked(property_name).to_string() };
			let mut new_text = label.to_string();
			if matches!(kind, PropertyKind::Method) {
				new_text += "()";
			}
			let kind = Some(match kind {
				PropertyKind::Field => CompletionItemKind::FIELD,
				PropertyKind::Method => CompletionItemKind::METHOD,
			});
			Some(CompletionItem {
				label,
				kind,
				text_edit: Some(CompletionTextEdit::Edit(TextEdit { range, new_text })),
				data: serde_json::to_value(CompletionData { model: model.clone() }).ok(),
				..Default::default()
			})
		});
		items.extend(completions);
		Ok(())
	}
	pub async fn complete_model(
		&self,
		needle: &str,
		range: Range,
		items: &mut MaxVec<CompletionItem>,
	) -> anyhow::Result<()> {
		if !items.has_space() {
			return Ok(());
		}
		let by_prefix = self.index.models.by_prefix.read().await;
		let matches = by_prefix
			.iter_prefix(needle.as_bytes())
			.flat_map(|(_, key)| self.index.models.get(key))
			.map(|model| {
				let label = _R(*model.key()).to_string();
				let module = model.base.as_ref().and_then(|base| {
					let module = self.index.module_of_path(&base.0.path.to_path())?;
					Some(_R(module).to_string())
				});
				CompletionItem {
					text_edit: Some(CompletionTextEdit::Edit(TextEdit {
						new_text: label.clone(),
						range,
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
		range: Range,
		items: &mut MaxVec<CompletionItem>,
	) -> anyhow::Result<()> {
		if !items.has_space() {
			return Ok(());
		}
		let by_prefix = self.index.templates.by_prefix.read().await;
		let matches = by_prefix.iter_prefix(needle.as_bytes()).map(|(_, key)| {
			let label = _R(*key).to_string();
			CompletionItem {
				text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
					new_text: label.clone(),
					insert: range,
					replace: range,
				})),
				label,
				kind: Some(CompletionItemKind::REFERENCE),
				..Default::default()
			}
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
	) -> anyhow::Result<()> {
		let component = _G(component).ok_or_else(|| errloc!("(complete_component_prop) component"))?;
		let component = self
			.index
			.components
			.get(&component.into())
			.ok_or_else(|| errloc!("component"))?;
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| errloc!("(complete_prop_of) range"))?;
		let completions = component.props.iter().map(|(prop, desc)| {
			let prop = _R(prop);
			CompletionItem {
				text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
					new_text: prop.to_string(),
					insert: range,
					replace: range,
				})),
				label: prop.to_string(),
				kind: Some(CompletionItemKind::PROPERTY),
				detail: Some(format!("{:?}", desc.type_)),
				..Default::default()
			}
		});
		items.extend(completions);
		Ok(())
	}
	pub fn complete_widget(
		&self,
		range: ByteRange,
		rope: Rope,
		items: &mut MaxVec<CompletionItem>,
	) -> anyhow::Result<()> {
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| errloc!("(complete_widget) range"))?;
		let completions = self.index.widgets.iter().flat_map(|widget| {
			let widget = widget.key().to_string();
			Some(CompletionItem {
				text_edit: Some(CompletionTextEdit::Edit(TextEdit {
					range,
					new_text: widget.clone(),
				})),
				label: widget,
				kind: Some(CompletionItemKind::ENUM),
				..Default::default()
			})
		});
		items.extend(completions);
		Ok(())
	}
	pub fn complete_action_tag(
		&self,
		range: ByteRange,
		rope: Rope,
		items: &mut MaxVec<CompletionItem>,
	) -> anyhow::Result<()> {
		let range = offset_range_to_lsp_range(range, rope).ok_or_else(|| errloc!("(complete_action_tag) range"))?;
		let completions = self.index.actions.iter().flat_map(|tag| {
			let tag = tag.key().to_string();
			Some(CompletionItem {
				text_edit: Some(CompletionTextEdit::Edit(TextEdit {
					range,
					new_text: tag.clone(),
				})),
				label: tag,
				kind: Some(CompletionItemKind::ENUM),
				..Default::default()
			})
		});
		items.extend(completions);
		Ok(())
	}
	pub fn jump_def_xml_id(&self, cursor_value: &str, uri: &Uri) -> anyhow::Result<Option<Location>> {
		let mut value = Cow::from(cursor_value);
		let path = some!(uri.to_file_path());
		if !value.contains('.') {
			'unscoped: {
				if let Some(module) = self.index.module_of_path(&path) {
					value = format!("{}.{value}", _R(module)).into();
					break 'unscoped;
				}
				debug!(
					"Could not find a reference for {} in {}: could not infer module",
					cursor_value,
					uri.path().as_str()
				);
				return Ok(None);
			}
		}
		let record_id = some!(_G(value));
		Ok((self.index.records.get(&record_id.into())).map(|record| record.location.clone().into()))
	}
	pub fn jump_def_model(&self, model: &str) -> anyhow::Result<Option<Location>> {
		let model = some!(_G(model));
		match (self.index.models.get(&model.into())).and_then(|model| model.base.as_ref().cloned()) {
			Some(ModelLocation(base, _)) => Ok(Some(base.into())),
			None => Ok(None),
		}
	}
	#[instrument(level = "trace", skip(self), ret)]
	pub fn hover_model(
		&self,
		model_str_key: &str,
		range: Option<Range>,
		definition: bool,
		identifier: Option<&str>,
	) -> anyhow::Result<Option<Hover>> {
		let model_key = some!(_G(model_str_key));
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
		let key = _G(name)?;
		let component = self.index.components.get(&key.into())?;
		let module = component
			.location
			.as_ref()
			.and_then(|loc| self.index.module_of_path(&loc.path.to_path()));
		let value = fomat!(
			"```js\n"
			"(component) class " (name) "\n"
			"```"
			if let Some(module) = module {
				"\n*Defined in:* `" (_R(module)) "`"
			}
		);
		Some(Hover {
			contents: HoverContents::Scalar(MarkedString::String(value)),
			range,
		})
	}
	pub fn hover_template(&self, name: &str, range: Option<Range>) -> Option<Hover> {
		let key = _G(name)?;
		let template = self.index.templates.get(&key.into())?;
		let module = template
			.location
			.as_ref()
			.and_then(|loc| self.index.module_of_path(&loc.path.to_path()));
		let value = fomat!(
			"```xml\n"
			"<t t-name=\"" (name) "\"/>\n"
			"```"
			if let Some(module) = module {
				"\n*Defined in:* `" (_R(module)) "`"
			}
		);
		Some(Hover {
			contents: HoverContents::Scalar(MarkedString::String(value)),
			range,
		})
	}
	pub fn jump_def_property_name(&self, property: &str, model: &str) -> anyhow::Result<Option<Location>> {
		let model_key = _I(model);
		let entry = some!(self.index.models.populate_properties(model_key.into(), &[]));
		let prop = some!(_G(property));
		if let Some(field) = entry.fields.as_ref().and_then(|fields| fields.get(&prop.into())) {
			Ok(Some(field.location.deref().clone().into()))
		} else if let Some(method) = entry.methods.as_ref().and_then(|methods| methods.get(&prop.into())) {
			Ok(Some(some!(method.locations.first()).deref().clone().into()))
		} else {
			Ok(None)
		}
	}
	pub fn jump_def_template_name(&self, name: &str) -> anyhow::Result<Option<Location>> {
		let name = some!(_G(name));
		let entry = some!(self.index.templates.get(&name.into()));
		let location = some!(&entry.value().location);
		Ok(Some(location.clone().into()))
	}
	pub fn jump_def_component_prop(&self, component: &str, prop: &str) -> anyhow::Result<Option<Location>> {
		let component = some!(_G(component));
		let prop = some!(_G(prop));
		let prop = some!(self.find_prop_recursive(&component.into(), &prop.into()));
		Ok(Some(prop.location.into()))
	}
	pub fn jump_def_widget(&self, widget: &str) -> anyhow::Result<Option<Location>> {
		let field = some!(self.index.widgets.get(widget.as_bytes()));
		Ok(Some(field.value().clone().into()))
	}
	pub fn jump_def_action_tag(&self, tag: &str) -> anyhow::Result<Option<Location>> {
		let field = some!(self.index.actions.get(tag.as_bytes()));
		Ok(Some(field.value().clone().into()))
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
	pub fn hover_property_name(&self, name: &str, model: &str, range: Option<Range>) -> anyhow::Result<Option<Hover>> {
		let model_key = _I(model);
		let entry = some!(self.index.models.populate_properties(model_key.into(), &[]));
		let prop = some!(_G(name));
		if let Some(field) = entry.fields.as_ref().and_then(|fields| fields.get(&prop.into())) {
			Ok(Some(Hover {
				range,
				contents: HoverContents::Markup(MarkupContent {
					kind: MarkupKind::Markdown,
					value: self.field_docstring(field, true),
				}),
			}))
		} else if entry
			.methods
			.as_ref()
			.is_some_and(|methods| methods.contains_key(&prop.into()))
		{
			drop(entry);
			let rtype = self.index.resolve_method_returntype(prop.into(), model_key).map(_R);
			let model = self.index.models.get(&model_key.into()).unwrap();
			let method = model.methods.as_ref().unwrap().get(&prop.into()).unwrap();
			Ok(Some(Hover {
				range,
				contents: HoverContents::Markup(MarkupContent {
					kind: MarkupKind::Markdown,
					value: self.method_docstring(name, method, rtype),
				}),
			}))
		} else {
			Ok(None)
		}
	}
	pub fn hover_record(&self, xml_id: &str, range: Option<Range>) -> anyhow::Result<Option<Hover>> {
		let key = some!(_G(xml_id));
		let record = some!(self.index.records.get(&key.into()));
		let model = match record.model.as_ref() {
			Some(model) => _R(*model),
			None => "<unknown>",
		};
		let value = format!("<record id=\"{xml_id}\" model=\"{model}\"/>");
		Ok(Some(Hover {
			contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
				language: "xml".to_owned(),
				value,
			})),
			range,
		}))
	}
	pub fn model_references(&self, path: &Path, model: &ModelName) -> anyhow::Result<Option<Vec<Location>>> {
		let record_locations = self
			.index
			.records
			.by_model(model)
			.map(|record| record.location.clone().into());
		let limit = self
			.workspaces
			.find_workspace_of(path, |_, ws| ws.references.limit)
			.unwrap_or_else(|| self.project_config.references_limit.load(Relaxed));
		if let Some(entry) = self.index.models.get(model) {
			let inherit_locations = entry.descendants.iter().map(|loc| loc.0.clone().into());
			Ok(Some(inherit_locations.chain(record_locations).take(limit).collect()))
		} else {
			Ok(Some(record_locations.take(limit).collect()))
		}
	}
	pub fn method_references(&self, prop: &str, model: &str) -> anyhow::Result<Option<Vec<Location>>> {
		let model_key = _I(model);
		let entry = some!(self.index.models.populate_properties(model_key.into(), &[]));
		let prop = some!(_G(prop));
		let method = some!(some!(entry.methods.as_ref()).get(&prop.into()));
		Ok(Some(
			method.locations.iter().map(|loc| loc.deref().clone().into()).collect(),
		))
	}
	pub fn record_references(
		&self,
		path: &Path,
		inherit_id: &str,
		current_module: Option<ModuleName>,
	) -> anyhow::Result<Option<Vec<Location>>> {
		let inherit_id = if inherit_id.contains('.') {
			Cow::from(inherit_id)
		} else if let Some(current_module) = current_module {
			Cow::from(format!("{}.{}", _R(current_module), inherit_id))
		} else {
			debug!("No current module to resolve the XML ID {inherit_id}");
			return Ok(None);
		};
		let inherit_id = RecordId::from(some!(_G(inherit_id)));
		let limit = self
			.workspaces
			.find_workspace_of(path, |_, ws| ws.references.limit)
			.unwrap_or_else(|| self.project_config.references_limit.load(Relaxed));
		let locations = self
			.index
			.records
			.by_inherit_id(&inherit_id)
			.map(|record| record.location.clone().into())
			.take(limit);
		Ok(Some(locations.collect()))
	}
	pub fn template_references(&self, name: &str, include_definition: bool) -> anyhow::Result<Option<Vec<Location>>> {
		let name = some!(_G(name));
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
	/// Returns a Markdown-formatted docstring for a model.
	pub fn model_docstring(&self, model: &ModelEntry, model_name: Option<&str>, identifier: Option<&str>) -> String {
		let module = model
			.base
			.as_ref()
			.and_then(|base| self.index.module_of_path(&base.0.path.to_path()));
		let mut descendants = model
			.descendants
			.iter()
			.map(|loc| &loc.0)
			.scan(SymbolSet::default(), |mods, loc| {
				let Some(module) = self.index.module_of_path(&loc.path.to_path()) else {
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
				"*Defined in:* `" (_R(module)) "`  \n"
			}
			for (idx, descendant) in descendants
				.by_ref()
				.take(Self::INHERITS_LIMIT)
				.enumerate()
			{
				if idx == 0 { "*Inherited in:* " } "`" (_R(descendant)) "`"
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
	pub fn field_docstring(&self, field: &Field, signature: bool) -> String {
		fomat! {
			if signature {
				"```python\n"
				"(field) " (_R(field.type_)) r#"("#
				match &field.kind {
					FieldKind::Value | FieldKind::Related(_) => {}
					FieldKind::Relational(relation) => { "\"" (_R(*relation)) "\", " }
				}
				"…)\n```  \n"
			}
			if let Some(module) = self
				.index
				.module_of_path(&field.location.path.to_path())
			{
				"*Defined in:* `" (_R(module)) "`  \n"
			}
			if let Some(help) = &field.help { (help.to_string()) }
		}
	}
	pub fn method_docstring(&self, name: &str, method: &Method, rtype: Option<&str>) -> String {
		let origin_fragment = match method.locations.as_slice() {
			[] => String::new(),
			[first, rest @ ..] => {
				let rest = rest.iter().filter_map(|override_| {
					self.index
						.module_of_path(&override_.path.to_path())
						.map(|module| (_R(module), override_, override_.range))
				});
				fomat! {
					if let Some(module) = self
						.index
						.module_of_path(&first.path.to_path())
					{
						"*Defined in:* `" (_R(module)) "`  \n"
					}
					if let Some(help) = &method.docstring {
						match help.to_string() {
							empty if empty.is_empty() => {}
							other => {
								(other) "\n\n"
							}
						}
					}
					for (idx, (module, override_, range)) in rest.enumerate() {
						if idx == 0 { "*Overridden in:*\n" }
						"- [`" (module) "`](" (to_display_path(override_.path.to_path())) "#L" (range.start.line + 1) ") in " (override_.path.subpath()) ":" (range.start.line + 1)
					} sep { "\n" }
				}
			}
		};
		let rtype = match rtype {
			Some(type_) => format!("Model[\"{type_}\"]"),
			None => "…".to_string(),
		};
		fomat! {
			"```python\n"
			"(method) def " (name) "(…) → " (rtype)
			"\n"
			"```\n"
			(origin_fragment)
		}
	}
	/// The main entrypoint for configuration changes.
	///
	/// If `roots` is not given, only `project_config` will be updated.
	pub fn on_change_config(&self, config: Config, root: Option<&Path>) {
		let Config {
			symbols,
			references,
			module,
			completions,
		} = config;

		let Some(root) = root else {
			warn!("TODO: discarding project config for `module`: {:?}", module);
			if let Some(limit) = completions.and_then(|c| c.limit) {
				self.project_config.completions_limit.store(limit, Relaxed);
			}
			if let Some(limit) = references.and_then(|c| c.limit) {
				self.project_config.references_limit.store(limit, Relaxed);
			}
			if let Some(limit) = symbols.and_then(|c| c.limit) {
				self.project_config.symbols_limit.store(limit, Relaxed);
			}
			if let Some(ModuleConfig { roots: Some(roots) }) = module.as_ref() {
				for root in roots {
					self.workspaces.insert(PathBuf::from(root), Default::default());
				}
			}
			return;
		};

		let root_config = Workspace {
			// symbols: symbols.unwrap_or_default(),
			references: references.unwrap_or_default(),
			completions: completions.unwrap_or_default(),
		};

		let Some(ModuleConfig { roots: Some(subroots) }) = module else {
			self.workspaces.insert(root.to_path_buf(), root_config);
			return;
		};

		for subroot in subroots {
			let path = root.join(&subroot);
			let Ok(root) = strict_canonicalize(path) else {
				continue;
			};
			let root_display = root.to_string_lossy();
			if root_display.contains('*') {
				let Ok(glob) = globwalk::glob_builder(root_display)
					.file_type(FileType::DIR | FileType::SYMLINK)
					.build()
				else {
					continue;
				};
				for dir_entry in glob {
					let Ok(root) = dir_entry else { continue };
					self.workspaces.insert(root.path().to_owned(), root_config.clone());
				}
			} else if std::fs::exists(&root).unwrap_or(false) {
				self.workspaces.insert(root.clone(), root_config.clone());
			}
		}
	}
	pub fn ensure_nonoverlapping_roots(&self) {
		let mut redundant = vec![];
		let mut roots = self.workspaces.iter().map(|r| r.key().to_owned()).collect::<Vec<_>>();
		roots.sort_unstable_by_key(|root| root.as_os_str().len());
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
			self.workspaces.remove(Path::new(root));
		}
	}
	pub fn completion_resolve_field(&self, completion: &mut CompletionItem) -> Option<()> {
		let CompletionData { model } = completion
			.data
			.take()
			.and_then(|raw| serde_json::from_value(raw).ok())?;
		let field = _G(&completion.label)?;
		let model = _G(model)?;
		let mut entry = self
			.index
			.models
			.try_get_mut(&model.into())
			.expect(format_loc!("deadlock"))?;
		let fields = entry.fields.as_mut()?;
		let field_entry = fields.get(&field.into()).cloned()?;
		drop(entry);

		let type_ = _R(field_entry.type_);
		completion.detail = match self.index.models.resolve_related_field(field.into(), model) {
			None => Some(format!("{type_}(…)")),
			Some(relation) => {
				let relation = _R(relation);
				Some(format!("{type_}(\"{relation}\", …)"))
			}
		};
		completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
			kind: MarkupKind::Markdown,
			value: self.field_docstring(&field_entry, false),
		}));

		Some(())
	}
	pub fn completion_resolve_method(&self, completion: &mut CompletionItem) -> Option<()> {
		let CompletionData { model } = completion
			.data
			.take()
			.and_then(|raw| serde_json::from_value(raw).ok())?;
		let method = _G(&completion.label)?;
		let model_key = _G(&model)?;
		let method_name = _R(method);
		let rtype = self.index.resolve_method_returntype(method.into(), model_key).map(_R);
		let entry = self.index.models.get(&model_key.into())?;
		let methods = entry.methods.as_ref()?;
		let method_entry = methods.get(&method.into())?;

		completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
			kind: MarkupKind::Markdown,
			value: self.method_docstring(method_name, method_entry, rtype),
		}));

		Some(())
	}
}

impl Text {
	/// Returns None if not a delta, the resulting range is empty, or if conversion of any range fails.
	fn damage_zone(&self, rope: &Rope, seed: Option<ByteRange>) -> Option<ByteRange> {
		let deltas = match self {
			Self::Full(_) => return None,
			Self::Delta(deltas) => deltas,
		};
		#[allow(clippy::reversed_empty_ranges)]
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
