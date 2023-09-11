use std::borrow::Cow;
use std::path::Path;
use std::sync::atomic::Ordering::Relaxed;
use std::sync::atomic::{AtomicBool, AtomicUsize};

use dashmap::{DashMap, DashSet};
use futures::executor::block_on;
use globwalk::FileType;
use lasso::{Key, Spur};
use log::debug;
use miette::{diagnostic, IntoDiagnostic};
use ropey::Rope;
use serde_json::Value;
use tower_lsp::lsp_types::*;
use tower_lsp::Client;
use tree_sitter::{Parser, Tree};

use odoo_lsp::config::{Config, ModuleConfig, ReferencesConfig, SymbolsConfig};
use odoo_lsp::index::{interner, Index, Interner, ModuleName, RecordId, SymbolSet};
use odoo_lsp::model::{Field, FieldKind, ModelEntry, ModelLocation, ModelName};
use odoo_lsp::record::Record;
use odoo_lsp::utils::isolate::Isolate;
use odoo_lsp::{some, utils::*};

pub struct Backend {
	pub client: Client,
	pub document_map: DashMap<String, Rope>,
	pub record_ranges: DashMap<String, Box<[std::ops::Range<CharOffset>]>>,
	pub ast_map: DashMap<String, Tree>,
	pub index: Index,
	pub roots: DashSet<String>,
	pub capabilities: Capabilities,
	pub root_setup: AtomicBool,
	pub symbols_limit: AtomicUsize,
	pub references_limit: AtomicUsize,
	pub isolate: Isolate,
}

#[derive(Debug, Default)]
pub struct Capabilities {
	pub dynamic_config: AtomicBool,
}

pub struct TextDocumentItem {
	pub uri: Url,
	pub text: Text,
	pub version: i32,
	pub language: Option<Language>,
	pub rope: Option<Rope>,
	pub old_rope: Option<Rope>,
}

pub enum Text {
	Full(String),
	Delta(Vec<TextDocumentContentChangeEvent>),
}

pub enum Language {
	Python,
	Xml,
	Javascript,
}

impl Backend {
	pub const LIMIT: usize = 80;
	/// Maximum number of descendants to show in docstring.
	const INHERITS_LIMIT: usize = 3;

	pub async fn on_change(&self, params: TextDocumentItem) -> miette::Result<()> {
		let split_uri = params.uri.path().rsplit_once('.');
		let mut diagnostics = vec![];
		let rope = match (params.rope, &params.text) {
			(Some(rope), _) => rope,
			(None, Text::Full(full)) => ropey::Rope::from_str(full),
			(None, Text::Delta(_)) => Err(diagnostic!("No rope and got delta"))?,
		};
		if matches!(split_uri, Some((_, "xml"))) || matches!(params.language, Some(Language::Xml)) {
			self.on_change_xml(&params.text, &params.uri, rope, &mut diagnostics)
				.await;
		} else if matches!(split_uri, Some((_, "py"))) || matches!(params.language, Some(Language::Python)) {
			self.on_change_python(&params.text, &params.uri, rope, params.old_rope)
				.await?;
		}
		self.client
			.publish_diagnostics(params.uri, diagnostics, Some(params.version))
			.await;

		Ok(())
	}
	pub fn update_ast(
		&self,
		text: &Text,
		uri: &Url,
		rope: Rope,
		old_rope: Option<Rope>,
		mut parser: Parser,
	) -> miette::Result<()> {
		let ast = self.ast_map.get_mut(uri.path());
		let ast = match (text, ast) {
			(Text::Full(full), _) => parser.parse(full, None),
			(Text::Delta(delta), Some(mut ast)) => {
				for change in delta {
					let range = change.range.ok_or_else(|| diagnostic!("delta without range"))?;
					let start =
						position_to_offset(range.start, rope.clone()).ok_or_else(|| diagnostic!("delta start"))?;
					// The old rope is used to calculate the *old* range-end, because
					// the diff may have caused it to fall out of the new rope's bounds.
					let old_rope = old_rope
						.as_ref()
						.cloned()
						.ok_or_else(|| diagnostic!("python diff requires pre-diff rope"))?;
					let end = position_to_offset(range.end, old_rope).ok_or_else(|| diagnostic!("delta end"))?;
					let len_new = change.text.len();
					let len_new_bytes = change.text.as_bytes().len();
					let start_position = tree_sitter::Point {
						row: range.start.line as usize,
						column: range.start.character as usize,
					};
					let old_end_position = tree_sitter::Point {
						row: range.end.line as usize,
						column: range.end.character as usize,
					};
					// calculate new_end_position using rope
					let start_char = rope.try_byte_to_char(start.0).into_diagnostic()?;
					let new_end_offset = CharOffset(start_char + len_new);
					let new_end_position = char_to_position(new_end_offset, rope.clone())
						.ok_or_else(|| diagnostic!("new_end_position"))?;
					let new_end_position = tree_sitter::Point {
						row: new_end_position.line as usize,
						column: new_end_position.character as usize,
					};
					// let it rip 🚀
					ast.edit(&tree_sitter::InputEdit {
						start_byte: start.0,
						old_end_byte: end.0,
						new_end_byte: start.0 + len_new_bytes,
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
		range: std::ops::Range<CharOffset>,
		rope: Rope,
		model_filter: Option<&str>,
		current_module: ModuleName,
		items: &mut Vec<CompletionItem>,
	) -> miette::Result<()> {
		let range = char_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("(complete_xml_id) range"))?;
		let by_prefix = self.index.records.by_prefix.read().await;
		let model_filter = model_filter.and_then(|model| interner().get(model).map(ModelName::from));
		fn to_completion_items(
			entry: &Record,
			current_module: ModuleName,
			range: Range,
			scoped: bool,
			interner: &Interner,
		) -> CompletionItem {
			let label = if entry.module == current_module && !scoped {
				entry.id.to_string()
			} else {
				entry.qualified_id(interner)
			};
			let model = entry.model.as_ref().map(|model| interner.resolve(model).to_string());
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
		let interner = &interner();
		if let Some((module, needle)) = needle.split_once('.') {
			let Some(module) = interner.get(module).map(Into::into) else {
				return Ok(());
			};
			let completions = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, keys)| {
				keys.keys().flat_map(|key| {
					self.index.records.get(&key).and_then(|entry| {
						(entry.module == module && (model_filter.is_none() || entry.model == model_filter))
							.then(|| to_completion_items(&entry, current_module, range, true, interner))
					})
				})
			});
			items.extend(completions.take(Self::LIMIT));
		} else {
			let completions = by_prefix.iter_prefix(needle.as_bytes()).flat_map(|(_, keys)| {
				keys.keys().flat_map(|key| {
					self.index.records.get(&key).and_then(|entry| {
						(model_filter.is_none() || entry.model == model_filter)
							.then(|| to_completion_items(&entry, current_module, range, false, interner))
					})
				})
			});
			items.extend(completions.take(Self::LIMIT));
		}
		Ok(())
	}
	pub async fn complete_field_name(
		&self,
		needle: &str,
		range: std::ops::Range<CharOffset>,
		model: String,
		rope: Rope,
		items: &mut Vec<CompletionItem>,
	) -> miette::Result<()> {
		let model_key = interner().get_or_intern(&model);
		let Some(mut entry) = self.index.models.get_mut(&model_key.into()) else {
			return Ok(());
		};
		let range = char_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("range"))?;
		let fields = self.populate_field_names(&mut entry, &[]).await?;
		let completions = fields.iter().flat_map(|(key, _)| {
			let field_name = interner().resolve(&Spur::try_from_usize(*key as usize).unwrap());
			field_name.contains(needle).then(|| CompletionItem {
				text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
					new_text: field_name.to_string(),
					insert: range,
					replace: range,
				})),
				label: field_name.to_string(),
				kind: Some(CompletionItemKind::FIELD),
				// TODO: Make this type-safe
				data: Some(Value::String(model.clone())),
				..Default::default()
			})
		});
		items.extend(completions);
		Ok(())
	}
	pub async fn complete_model(
		&self,
		needle: &str,
		range: std::ops::Range<CharOffset>,
		rope: Rope,
		items: &mut Vec<CompletionItem>,
	) -> miette::Result<()> {
		let range = char_range_to_lsp_range(range, rope).ok_or_else(|| diagnostic!("(complete_model) range"))?;
		let by_prefix = self.index.models.by_prefix.read().await;
		let matches = by_prefix
			.iter_prefix(needle.as_bytes())
			.flat_map(|(_, key)| self.index.models.get(key))
			.map(|entry| {
				let label = interner().resolve(entry.key()).to_string();
				let module = entry.base.as_ref().and_then(|base| {
					let loc = interner().resolve(&base.0.path);
					let module = self.index.module_of_path(Path::new(loc))?;
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
	pub fn jump_def_inherit_id(&self, cursor_value: &str, uri: &Url) -> miette::Result<Option<Location>> {
		let mut value = Cow::from(cursor_value);
		if !value.contains('.') {
			'unscoped: {
				if let Some(module) = self.index.module_of_path(Path::new(uri.path())) {
					value = format!("{}.{value}", interner().resolve(module.key())).into();
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
		return Ok((self.index.records.get(&record_id.into())).map(|entry| entry.location.clone().into()));
	}
	pub fn jump_def_model(&self, model: &str) -> miette::Result<Option<Location>> {
		let model = some!(interner().get(model));
		match self
			.index
			.models
			.get(&model.into())
			.and_then(|entry| entry.base.as_ref().cloned())
		{
			Some(ModelLocation(base, _)) => Ok(Some(base.into())),
			None => Ok(None),
		}
	}
	pub fn hover_model(
		&self,
		model_str_key: &str,
		range: Option<Range>,
		definition: bool,
	) -> miette::Result<Option<Hover>> {
		let model_key = some!(interner().get(model_str_key));
		let model = some!(self.index.models.get(&model_key.into()));
		Ok(Some(Hover {
			range,
			contents: HoverContents::Markup(MarkupContent {
				kind: MarkupKind::Markdown,
				value: self.model_docstring(&model, definition.then_some(model_str_key)),
			}),
		}))
	}
	pub fn jump_def_field_name(&self, field: &str, model: &str) -> miette::Result<Option<Location>> {
		let model = interner().get_or_intern(model);
		let mut entry = some!(self.index.models.get_mut(&model.into()));
		let field = some!(interner().get(field));
		let fields = block_on(self.populate_field_names(&mut entry, &[]))?;
		let field = some!(fields.get(&field.into()));
		Ok(Some(field.location.clone().into()))
	}
	pub fn hover_field_name(&self, name: &str, model: &str, range: Option<Range>) -> miette::Result<Option<Hover>> {
		let model = interner().get_or_intern(model);
		let mut entry = some!(self.index.models.get_mut(&model.into()));
		let field = some!(interner().get(name));
		let fields = block_on(self.populate_field_names(&mut entry, &[]))?;
		let field = some!(fields.get(&field.into()));
		Ok(Some(Hover {
			range,
			contents: HoverContents::Markup(MarkupContent {
				kind: MarkupKind::Markdown,
				value: self.field_docstring(name, field, true),
			}),
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
		current_module: Option<&ModuleName>,
	) -> miette::Result<Option<Vec<Location>>> {
		let interner = &interner();
		let inherit_id = if inherit_id.contains('.') {
			Cow::from(inherit_id)
		} else if let Some(current_module) = current_module {
			Cow::from(format!("{}.{}", interner.resolve(current_module), inherit_id))
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
	pub fn model_docstring(&self, model: &ModelEntry, model_name: Option<&str>) -> String {
		use std::fmt::Write;
		let mut out = String::new();
		if let Some(name) = model_name {
			_ = writeln!(
				&mut out,
				concat!("```python\n", "models.Model[\"{}\"]\n", "```  "),
				name
			)
		}

		if let Some(base) = &model.base {
			if let Some(module) = self.index.module_of_path(Path::new(interner().resolve(&base.0.path))) {
				let module = interner().resolve(&module);
				_ = writeln!(&mut out, "*Defined in:* `{module}`  ")
			}
		}
		let mut descendants = model
			.descendants
			.iter()
			.map(|loc| &loc.0)
			.scan(SymbolSet::default(), |mods, loc| {
				let Some(module) = self.index.module_of_path(Path::new(interner().resolve(&loc.path))) else {
					return Some(None);
				};
				if mods.insert(*module) {
					Some(Some(loc))
				} else {
					Some(None)
				}
			})
			.flatten();
		let mut descendants_count = 0;
		while descendants_count < Self::INHERITS_LIMIT {
			let Some(loc) = descendants.next() else {
				break;
			};
			let Some(module) = self.index.module_of_path(Path::new(interner().resolve(&loc.path))) else {
				continue;
			};
			let module = interner().resolve(&module);
			if descendants_count == 0 {
				_ = write!(&mut out, "*Inherited in:* `{module}`");
			} else {
				_ = write!(&mut out, ", `{module}`");
			}
			descendants_count += 1
		}
		let remaining = descendants.count();
		if remaining > 0 {
			_ = writeln!(&mut out, " (+{remaining} modules)  \n");
		} else {
			_ = out.write_str("  \n\n");
		}
		if let Some(help) = &model.docstring {
			_ = out.write_str(help.to_string().as_ref());
		}
		out
	}
	pub fn field_docstring(&self, name: &str, field: &Field, signature: bool) -> String {
		use std::fmt::Write;
		let mut out = String::new();
		let type_ = interner().resolve(&field.type_);
		if signature {
			match &field.kind {
				FieldKind::Value => {
					out = format!(concat!("```python\n", "{} = fields.{}(…)\n", "```\n\n"), name, type_)
				}
				FieldKind::Relational(relation) => {
					let relation = interner().resolve(relation);
					out = format!(
						concat!("```python\n", "{} = fields.{}(\"{}\", …)\n", "```\n\n"),
						name, type_, relation
					);
				}
			}
		}
		if let Some(module) = self
			.index
			.module_of_path(Path::new(interner().resolve(&field.location.path)))
		{
			let module = interner().resolve(&module);
			_ = writeln!(&mut out, "*Defined in:* `{module}`  ");
		}
		if let Some(help) = &field.help {
			_ = out.write_str(help.to_string().as_ref());
		}
		out
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
		self.root_setup.store(true, Relaxed);
	}
}
