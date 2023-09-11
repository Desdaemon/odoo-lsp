use std::borrow::Cow;

use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering::Relaxed;
use std::sync::atomic::{AtomicBool, AtomicUsize};

use catch_panic::CatchPanic;
use dashmap::{DashMap, DashSet};
use futures::executor::block_on;
use globwalk::FileType;
use lasso::{Key, Spur};
use log::{debug, error, info};
use miette::{diagnostic, IntoDiagnostic};
use odoo_lsp::record::Record;
use ropey::Rope;
use serde_json::Value;
use tower::ServiceBuilder;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::{DidChangeConfiguration, Notification, Progress};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{Parser, Tree};

use odoo_lsp::config::{Config, ModuleConfig, ReferencesConfig, SymbolsConfig};
use odoo_lsp::index::{interner, Index, Interner, ModuleName, RecordId, SymbolSet};
use odoo_lsp::model::{Field, FieldKind, ModelEntry, ModelLocation, ModelName};
use odoo_lsp::utils::isolate::Isolate;
use odoo_lsp::{format_loc, some, utils::*};

mod analyze;
mod catch_panic;
mod python;
mod xml;

/// Maximum number of descendants to show in docstring.
const INHERITS_LIMIT: usize = 3;

pub struct Backend {
	client: Client,
	document_map: DashMap<String, Rope>,
	record_ranges: DashMap<String, Box<[std::ops::Range<CharOffset>]>>,
	ast_map: DashMap<String, Tree>,
	index: Index,
	roots: DashSet<String>,
	capabilities: Capabilities,
	root_setup: AtomicBool,
	symbols_limit: AtomicUsize,
	references_limit: AtomicUsize,
	isolate: Isolate,
}

#[derive(Debug, Default)]
struct Capabilities {
	dynamic_config: AtomicBool,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
		let root = params.root_uri.and_then(|uri| uri.to_file_path().ok()).or_else(
			#[allow(deprecated)]
			|| params.root_path.map(PathBuf::from),
		);
		'root: {
			match params.initialization_options.map(serde_json::from_value::<Config>) {
				Some(Ok(config)) => {
					self.on_change_config(config).await;
					break 'root;
				}
				Some(Err(err)) => {
					error!("could not parse provided config:\n{err}");
				}
				None => {}
			}
			if let Some(root) = &root {
				let config_file = 'find_root: {
					for choice in [".odoo_lsp", ".odoo_lsp.json"] {
						let path = root.join(choice);
						if let Ok(file) = tokio::fs::read(&path).await {
							break 'find_root Some((path, file));
						}
					}
					None
				};
				let Some((path, config)) = &config_file else {
					self.roots.insert(root.to_string_lossy().to_string());
					break 'root;
				};
				match serde_json::from_slice::<Config>(config) {
					Ok(config) => self.on_change_config(config).await,
					Err(err) => error!("could not parse {:?}:\n{err}", path),
				}
			}
		}

		if let Some(WorkspaceClientCapabilities {
			did_change_configuration:
				Some(DynamicRegistrationClientCapabilities {
					dynamic_registration: Some(true),
				}),
			..
		}) = params.capabilities.workspace
		{
			self.capabilities.dynamic_config.store(true, Relaxed);
		}

		Ok(InitializeResult {
			server_info: None,
			offset_encoding: None,
			capabilities: ServerCapabilities {
				definition_provider: Some(OneOf::Left(true)),
				hover_provider: Some(HoverProviderCapability::Simple(true)),
				references_provider: Some(OneOf::Left(true)),
				workspace_symbol_provider: Some(OneOf::Left(true)),
				text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
				completion_provider: Some(CompletionOptions {
					resolve_provider: Some(true),
					trigger_characters: Some(vec!["\"".to_string(), "'".to_string(), ".".to_string()]),
					all_commit_characters: None,
					completion_item: None,
					work_done_progress_options: Default::default(),
				}),
				workspace: Some(WorkspaceServerCapabilities {
					workspace_folders: Some(WorkspaceFoldersServerCapabilities {
						supported: Some(true),
						change_notifications: Some(OneOf::Left(true)),
					}),
					file_operations: None,
				}),
				..ServerCapabilities::default()
			},
		})
	}
	async fn shutdown(&self) -> Result<()> {
		Ok(())
	}
	async fn did_close(&self, params: DidCloseTextDocumentParams) {
		let path = params.text_document.uri.path();
		let Self {
			document_map,
			record_ranges,
			ast_map,
			client: _,
			index: _,
			roots: _,
			capabilities: _,
			root_setup: _,
			symbols_limit: _,
			references_limit: _,
			isolate: _,
		} = self;
		document_map.remove(path);
		record_ranges.remove(path);
		ast_map.remove(path);
	}
	async fn initialized(&self, _: InitializedParams) {
		debug!("initialized");
		let token = NumberOrString::String("_odoo_lsp_initialized".to_string());
		self.client
			.send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams { token: token.clone() })
			.await
			.expect("Could not create WDP");
		_ = self
			.client
			.send_notification::<Progress>(ProgressParams {
				token: token.clone(),
				value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(WorkDoneProgressBegin {
					title: "Indexing".to_string(),
					..Default::default()
				})),
			})
			.await;
		let progress = Some((&self.client, token.clone()));

		if !self.root_setup.load(Relaxed) {
			for workspace in self.client.workspace_folders().await.unwrap().unwrap_or_default() {
				let Ok(file_path) = workspace.uri.to_file_path() else {
					continue;
				};
				self.roots.insert(file_path.to_string_lossy().to_string());
			}
		}

		for root in self.roots.iter() {
			match self.index.add_root(root.as_str(), progress.clone()).await {
				Ok(Some(results)) => {
					info!(
						"{} | {} modules | {} records | {} models | {:.2}s",
						root.as_str(),
						results.module_count,
						results.record_count,
						results.model_count,
						results.elapsed.as_secs_f64()
					);
				}
				Err(err) => {
					error!("(initialized) could not add root {}:\n{err}", root.as_str());
				}
				_ => {}
			}
		}

		_ = self
			.client
			.send_notification::<Progress>(ProgressParams {
				token,
				value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(Default::default())),
			})
			.await;

		if self.capabilities.dynamic_config.load(Relaxed) {
			_ = self
				.client
				.register_capability(vec![Registration {
					id: "_dynamic_config".to_string(),
					method: DidChangeConfiguration::METHOD.to_string(),
					register_options: None,
				}])
				.await;
		}
	}
	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		let language;
		let language_id = params.text_document.language_id.as_str();
		let split_uri = params.text_document.uri.path().rsplit_once('.');
		if language_id == "python" || matches!(split_uri, Some((_, "py"))) {
			language = Language::Python
		} else if language_id == "javascript" || matches!(split_uri, Some((_, "js"))) {
			language = Language::Javascript
		} else if language_id == "xml" || matches!(split_uri, Some((_, "xml"))) {
			language = Language::Xml
		} else {
			debug!("Could not determine language, or language not supported:\nlanguage_id={language_id} split_uri={split_uri:?}");
			return;
		}

		let rope = Rope::from_str(&params.text_document.text);
		self.document_map
			.insert(params.text_document.uri.path().to_string(), rope.clone());

		if self
			.index
			.module_of_path(Path::new(params.text_document.uri.path()))
			.is_none()
		{
			// outside of root?
			debug!("oob: {}", params.text_document.uri.path());
			'oob: {
				let Ok(path) = params.text_document.uri.to_file_path() else {
					break 'oob;
				};
				let mut path = Some(path.as_path());
				while let Some(path_) = path {
					if tokio::fs::try_exists(path_.with_file_name("__manifest__.py"))
						.await
						.unwrap_or(false)
					{
						let file_path = path_.parent().expect("has parent").to_string_lossy();
						self.index
							.add_root(&file_path, None)
							.await
							.report(|| format_loc!("failed to add root {}", file_path));
						break;
					}
					path = path_.parent();
				}
			}
		}

		self.on_change(TextDocumentItem {
			uri: params.text_document.uri,
			text: Text::Full(params.text_document.text),
			version: params.text_document.version,
			language: Some(language),
			rope: Some(rope),
			old_rope: None,
		})
		.await
		.report(|| format_loc!("did_open failed"))
	}
	async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
		if let [TextDocumentContentChangeEvent {
			range: None,
			range_length: None,
			text,
		}] = params.content_changes.as_mut_slice()
		{
			self.on_change(TextDocumentItem {
				uri: params.text_document.uri,
				text: Text::Full(std::mem::take(text)),
				version: params.text_document.version,
				language: None,
				rope: None,
				old_rope: None,
			})
			.await
			.report(|| format_loc!("did_change failed"));
			return;
		}
		let mut rope = self
			.document_map
			.get_mut(params.text_document.uri.path())
			.expect("Did not build a rope");
		let old_rope = rope.clone();
		// Update the rope
		// TODO: Refactor into method
		for change in &params.content_changes {
			if change.range.is_none() && change.range_length.is_none() {
				*rope.value_mut() = ropey::Rope::from_str(&change.text);
			} else {
				let range = change.range.expect("LSP change event must have a range");
				let Some(range) = lsp_range_to_char_range(range, rope.value().clone()) else {
					continue;
				};
				let rope = rope.value_mut();
				let start = range.start.0;
				rope.remove(range.map_unit(|unit| unit.0));
				rope.insert(start, &change.text);
			}
		}
		self.on_change(TextDocumentItem {
			uri: params.text_document.uri,
			text: Text::Delta(params.content_changes),
			version: params.text_document.version,
			language: None,
			rope: Some(rope.value().clone()),
			old_rope: Some(old_rope),
		})
		.await
		.report(|| format_loc!("did_change failed"));
	}
	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		if let Some(text) = params.text {
			debug!("did_save ignored:\n{text}");
		}
	}
	async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
		let uri = &params.text_document_position_params.text_document.uri;
		debug!("goto_definition {}", uri.path());
		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			debug!("Unsupported file {}", uri.path());
			return Ok(None);
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			panic!("Bug: did not build a rope for {}", uri.path());
		};
		let location;
		if ext == "xml" {
			location = self
				.xml_jump_def(params, document.value().clone())
				.await
				.map_err(|err| error!("Error retrieving references:\n{err}"))
				.ok()
				.flatten();
		} else if ext == "py" {
			location = self
				.python_jump_def(params, document.value().clone())
				.await
				.map_err(|err| error!("Error retrieving references:\n{err}"))
				.ok()
				.flatten();
		} else {
			debug!("Unsupported file {}", uri.path());
			return Ok(None);
		}

		Ok(location.map(GotoDefinitionResponse::Scalar))
	}
	async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
		let uri = &params.text_document_position.text_document.uri;
		debug!("references {}", uri.path());

		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			return Ok(None); // hit a directory, super unlikely
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			debug!("Bug: did not build a rope for {}", uri.path());
			return Ok(None);
		};
		if ext == "py" {
			let Some(ast) = self.ast_map.get(uri.path()) else {
				debug!("Bug: did not build AST for {}", uri.path());
				return Ok(None);
			};
			match self.python_references(params, document.value().clone(), ast.value().clone()) {
				Ok(ret) => Ok(ret),
				Err(report) => {
					debug!("{report}");
					Ok(None)
				}
			}
		} else if ext == "xml" {
			match self.xml_references(params, document.value().clone()) {
				Ok(ret) => Ok(ret),
				Err(report) => {
					debug!("{report}");
					Ok(None)
				}
			}
		} else {
			Ok(None)
		}
	}
	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		let uri = &params.text_document_position.text_document.uri;
		debug!("completion {}", uri.path());

		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			return Ok(None); // hit a directory, super unlikely
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			debug!("Bug: did not build a rope for {}", uri.path());
			return Ok(None);
		};
		if ext == "xml" {
			let completions = self.isolate.send_task(|send| {
				Box::pin(async { _ = send.send(self.xml_completions(params, document.value().clone()).await) })
			});
			match completions.await.expect("isolate error") {
				Ok(ret) => Ok(ret),
				Err(report) => {
					self.client
						.show_message(MessageType::ERROR, format!("error during xml completion:\n{report}"))
						.await;
					Ok(None)
				}
			}
		} else if ext == "py" {
			let Some(ast) = self.ast_map.get(uri.path()) else {
				debug!("Bug: did not build AST for {}", uri.path());
				return Ok(None);
			};
			let completions = self.isolate.send_task(|send| {
				Box::pin(async {
					_ = send.send(
						self.python_completions(params, ast.value().clone(), document.value().clone())
							.await,
					);
				})
			});
			match completions.await.expect("isolate error") {
				Ok(ret) => Ok(ret),
				Err(err) => {
					self.client
						.show_message(MessageType::ERROR, format!("error during python completion:\n{err}"))
						.await;
					Ok(None)
				}
			}
		} else {
			debug!("Unsupported file {}", uri.path());
			Ok(None)
		}
	}
	async fn completion_resolve(&self, mut completion: CompletionItem) -> Result<CompletionItem> {
		'resolve: {
			match &completion.kind {
				Some(CompletionItemKind::CLASS) => {
					let Some(model) = interner().get(&completion.label) else {
						break 'resolve;
					};
					let Some(mut entry) = self.index.models.get_mut(&model.into()) else {
						break 'resolve;
					};
					if let Err(err) = entry.resolve_details().await {
						dbg!(err);
					}
					completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
						kind: MarkupKind::Markdown,
						value: self.model_docstring(&entry, None),
					}))
				}
				Some(CompletionItemKind::FIELD) => {
					// NOTE: This was injected by complete().
					let Some(Value::String(value)) = &completion.data else {
						break 'resolve;
					};
					let Some(field) = interner().get(&completion.label) else {
						break 'resolve;
					};
					let field_name = interner().resolve(&field);
					let Some(model) = interner().get(value) else {
						break 'resolve;
					};
					let Some(entry) = self.index.models.get(&model.into()) else {
						break 'resolve;
					};
					let Some(fields) = &entry.fields else { break 'resolve };
					if let Some(field) = fields.get(&field.into()) {
						let type_ = interner().resolve(&field.type_);
						completion.detail = match field.kind {
							FieldKind::Value => Some(format!("{type_}(…)")),
							FieldKind::Relational(relation) => {
								let relation = interner().resolve(&relation);
								Some(format!("{type_}(\"{relation}\", …)"))
							}
						};
						completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
							kind: MarkupKind::Markdown,
							value: self.field_docstring(field_name, field, false),
						}))
					}
				}
				_ => {}
			}
		}
		Ok(completion)
	}
	async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let document = some!(self.document_map.get(uri.path()));
		let (_, ext) = some!(uri.path().split_once('.'));
		if ext == "py" {
			match self.python_hover(params, document.value().clone()).await {
				Ok(ret) => Ok(ret),
				Err(err) => {
					debug!("{err}");
					Ok(None)
				}
			}
		} else {
			debug!("Unsupported file {}", uri.path());
			Ok(None)
		}
	}
	async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
		let items = self
			.roots
			.iter()
			.map(|entry| {
				let scope_uri = Some(format!("file://{}", entry.key()).parse().unwrap());
				ConfigurationItem {
					section: Some("odoo-lsp".into()),
					scope_uri,
				}
			})
			.collect();
		let configs = self.client.configuration(items).await.unwrap_or_default();
		for (root, config) in self.roots.iter().zip(configs) {
			let config = serde_json::from_value::<Config>(config);
			// TODO: Do something with the config
			debug!("config: {} => {:?}", root.key(), config);
		}
	}
	async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
		for added in params.event.added {
			let file_path = added.uri.to_file_path().expect("not a file path");
			let file_path = file_path.as_os_str().to_string_lossy();
			self.index
				.add_root(&file_path, None)
				.await
				.report(|| format_loc!("(did_change_workspace_folders) failed to add root {}", file_path));
		}
		for removed in params.event.removed {
			self.index.remove_root(removed.uri.path());
		}
		self.index.mark_n_sweep();
	}
	async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
		Ok(None)
	}
	#[allow(deprecated)]
	async fn symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
		let query = &params.query;

		let models_by_prefix = self.index.models.by_prefix.read().await;
		let records_by_prefix = self.index.records.by_prefix.read().await;
		let models = models_by_prefix.iter_prefix(query.as_bytes()).flat_map(|(_, key)| {
			self.index.models.get(key).into_iter().flat_map(|entry| {
				entry.base.as_ref().map(|loc| SymbolInformation {
					name: interner().resolve(entry.key()).to_string(),
					kind: SymbolKind::CONSTANT,
					tags: None,
					deprecated: None,
					location: loc.0.clone().into(),
					container_name: None,
				})
			})
		});
		let limit = self.symbols_limit.load(Relaxed);
		fn to_symbol_information(record: &odoo_lsp::record::Record, interner: &Interner) -> SymbolInformation {
			SymbolInformation {
				name: record.qualified_id(interner),
				kind: SymbolKind::VARIABLE,
				tags: None,
				deprecated: None,
				location: record.location.clone().into(),
				container_name: None,
			}
		}
		let interner = &interner();
		if let Some((module, xml_id_query)) = query.split_once('.') {
			let module = some!(interner.get(module)).into();
			let records = records_by_prefix
				.iter_prefix(xml_id_query.as_bytes())
				.flat_map(|(_, keys)| {
					keys.keys().flat_map(|key| {
						self.index.records.get(&key).and_then(|record| {
							(record.module == module).then(|| to_symbol_information(&record, interner))
						})
					})
				});
			Ok(Some(models.chain(records).take(limit).collect()))
		} else {
			let records = records_by_prefix.iter_prefix(query.as_bytes()).flat_map(|(_, keys)| {
				keys.keys().flat_map(|key| {
					self.index
						.records
						.get(&key)
						.map(|record| to_symbol_information(&record, interner))
				})
			});
			Ok(Some(models.chain(records).take(limit).collect()))
		}
	}
}

struct TextDocumentItem {
	uri: Url,
	text: Text,
	version: i32,
	language: Option<Language>,
	rope: Option<Rope>,
	old_rope: Option<Rope>,
}

pub enum Text {
	Full(String),
	Delta(Vec<TextDocumentContentChangeEvent>),
}

enum Language {
	Python,
	Xml,
	Javascript,
}

impl Backend {
	const LIMIT: usize = 80;
	async fn on_change(&self, params: TextDocumentItem) -> miette::Result<()> {
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
	fn update_ast(
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
	async fn complete_xml_id(
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
	async fn complete_field_name(
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
		let fields = self.populate_field_names(&mut entry).await?;
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
	async fn complete_model(
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
	fn jump_def_inherit_id(&self, cursor_value: &str, uri: &Url) -> miette::Result<Option<Location>> {
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
	fn jump_def_model(&self, model: &str) -> miette::Result<Option<Location>> {
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
	fn hover_model(
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
	fn jump_def_field_name(&self, field: &str, model: &str) -> miette::Result<Option<Location>> {
		let model = interner().get_or_intern(model);
		let mut entry = some!(self.index.models.get_mut(&model.into()));
		let field = some!(interner().get(field));
		let fields = block_on(self.populate_field_names(&mut entry))?;
		let field = some!(fields.get(&field.into()));
		Ok(Some(field.location.clone().into()))
	}
	fn hover_field_name(&self, name: &str, model: &str, range: Option<Range>) -> miette::Result<Option<Hover>> {
		let model = interner().get_or_intern(model);
		let mut entry = some!(self.index.models.get_mut(&model.into()));
		let field = some!(interner().get(name));
		let fields = block_on(self.populate_field_names(&mut entry))?;
		let field = some!(fields.get(&field.into()));
		Ok(Some(Hover {
			range,
			contents: HoverContents::Markup(MarkupContent {
				kind: MarkupKind::Markdown,
				value: self.field_docstring(name, field, true),
			}),
		}))
	}
	fn model_references(&self, model: &ModelName) -> miette::Result<Option<Vec<Location>>> {
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
	fn record_references(
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
	fn model_docstring(&self, model: &ModelEntry, model_name: Option<&str>) -> String {
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
		while descendants_count < INHERITS_LIMIT {
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
	fn field_docstring(&self, name: &str, field: &Field, signature: bool) -> String {
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
	async fn on_change_config(&self, config: Config) {
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

#[tokio::main]
async fn main() {
	env_logger::init();

	let stdin = tokio::io::stdin();
	let stdout = tokio::io::stdout();

	let (service, socket) = LspService::build(|client| Backend {
		client,
		index: Default::default(),
		document_map: DashMap::new(),
		record_ranges: DashMap::new(),
		roots: DashSet::new(),
		capabilities: Default::default(),
		root_setup: Default::default(),
		ast_map: DashMap::new(),
		symbols_limit: AtomicUsize::new(100),
		references_limit: AtomicUsize::new(100),
		isolate: Isolate::new(),
	})
	.finish();

	let service = ServiceBuilder::new().layer_fn(CatchPanic).service(service);
	Server::new(stdin, stdout, socket).serve(service).await;
}
