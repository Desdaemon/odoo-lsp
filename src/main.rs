//! ## Application flow
//!
//! All LSP requests are implemented here on [`Backend`] via the [`LanguageServer`] trait.
//!
//! As the server handles these requests, it may opt to delegate language-specific tasks
//! to methods in the appropriate modules, for example Python to [`python`], XML to [`xml`] etc.
//!
//! Finally, these modules may also opt to delegate formulation of responses to [`backend`],
//! where most leaf methods live.
//!
//! Here's a rough flowchart of the server:
//! ```txt
//!                      ┌───────────────┐                        
//!        ┌─────────────► src/python.rs ├───────────────┐        
//!        │             └───────────────┘               │        
//!        │             ┌────────────┐                  │        
//!        ├─────────────► src/xml.rs ├──────────────────┤        
//!        │             └────────────┘                  │        
//! ┌──────┴──────┐                             ┌────────▼───────┐
//! │ src/main.rs │                             │ src/backend.rs │
//! └──────┬──────┘                             └────────▲───────┘
//!        │             ┌───────────┐                   │        
//!        ├─────────────► src/js.rs ├───────────────────┤        
//!        │             └───────────┘                   │        
//!        │             ┌──────────────┐                │        
//!        └─────────────► src/index.rs ├────────────────┘        
//!                      └──────────────┘                         
//! ```
//!
//! ## String handling
//!
//! Apart from normal Rust strings, the server uses a few more string-like types
//! for optimizing memory usage and performance:
//!
//! - [`ropey::Rope`] represents full documents, and is also a [data structure] of the same name.
//! - [`Spur`] and [`Symbol`] are [`u32`]-sized tokens representing
//!   [interned strings]. While they are not themselves strings, the
//!   [`interner`] can be used to resolve them into strings as well as intern new strings.
//!   Furthermore, [`Symbol`] is a type-safe wrapper around [`Spur`] to prevent mixing
//!   symbols representing different types.
//! - [`ImStr`](odoo_lsp::str::ImStr) and [`Text`](odoo_lsp::str::Text) are used strategically
//!   to avoid excessive memory usage and copying.
//!
//! ## tree-sitter
//!
//! [tree-sitter] is used to parse and analyze Python and JS code.
//! The entire library is too large to cover here, but the important functions/types used are:
//!
//! - [`tree_sitter::Parser`] creates a generic parser for any language.
//! - [`tree_sitter::Language`] defines the [AST] for each language, in this case
//!   they are provided by [`tree_sitter_python`] and [`tree_sitter_javascript`].
//! - [`tree_sitter::QueryCursor`] is used to extract desired patterns from a [`tree_sitter::Tree`],
//!   which is produced by parsing raw text.
//! - [`ts_macros::query!`] provides a shorthand to manually defining queries
//!   and correctly extracting [captures].
//!
//! XML doesn't use tree-sitter, but instead a low-level [lexer](xmlparser::Tokenizer) which
//! yields a sequence of [tokens](xmlparser::Token).
//!
//! [tree-sitter]: https://tree-sitter.github.io/tree-sitter/
//! [AST]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
//! [data structure]: https://en.wikipedia.org/wiki/Rope_(data_structure)
//! [interned strings]: https://en.wikipedia.org/wiki/String_interning
//! [captures]: https://tree-sitter.github.io/tree-sitter/using-parsers#capturing-nodes
//! [lexer]: quickxml::Reader
//! [`Spur`]: lasso::Spur
//! [`Symbol`]: odoo_lsp::index::Symbol

#![warn(clippy::cognitive_complexity)]
#![deny(clippy::unused_async)]
#![deny(clippy::await_holding_invalid_type)]

use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::Relaxed;
use std::time::Duration;

use catch_panic::CatchPanic;
use dashmap::{DashMap, DashSet};
use ropey::Rope;
use serde_json::Value;
use tower::ServiceBuilder;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::{DidChangeConfiguration, Notification, Progress};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};
use tracing::{debug, error, info, instrument, warn};

use odoo_lsp::config::Config;
use odoo_lsp::index::{interner, Interner};
use odoo_lsp::{loc, some, utils::*};

mod backend;
mod catch_panic;
mod cli;
mod js;
mod python;
mod xml;

#[cfg(doc)]
pub use odoo_lsp::*;

use backend::{Backend, Document, Language, Text};
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	#[instrument(skip_all)]
	async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
		let root = params.root_uri.and_then(|uri| uri.to_file_path().ok()).or_else(
			#[allow(deprecated)]
			|| params.root_path.map(PathBuf::from),
		);
		'root: {
			match params.initialization_options.map(serde_json::from_value::<Config>) {
				Some(Ok(config)) => {
					self.on_change_config(config).await;
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
						match tokio::fs::read(&path).await {
							Ok(file) => break 'find_root Some((path, file)),
							Err(err) => error!("(initialize) could not read {path:?}: {err}"),
						}
					}
					None
				};
				let Some((path, config)) = &config_file else {
					self.roots.insert(root.clone());
					break 'root;
				};
				match serde_json::from_slice::<Config>(config) {
					Ok(config) => self.on_change_config(config).await,
					Err(err) => error!("could not parse {:?}:\n{err}", path),
				}
			}
			for ws_dir in params.workspace_folders.unwrap_or_default() {
				match ws_dir.uri.to_file_path() {
					Ok(path) => drop(self.roots.insert(path)),
					Err(()) => {
						error!("not a file path: {}", ws_dir.uri);
					}
				}
			}
		}

		if let Some(WorkspaceClientCapabilities {
			did_change_configuration:
				Some(DynamicRegistrationClientCapabilities {
					dynamic_registration: Some(true),
				}),
			..
		}) = params.capabilities.workspace.as_ref()
		{
			self.capabilities.dynamic_config.store(true, Relaxed);
		}
		if let Some(WorkspaceClientCapabilities {
			workspace_folders: Some(true),
			..
		}) = params.capabilities.workspace.as_ref()
		{
			self.capabilities.workspace_folders.store(true, Relaxed);
		}

		if let Some(TextDocumentClientCapabilities {
			diagnostic: Some(..), ..
		}) = params.capabilities.text_document
		{
			debug!("Client supports pull diagnostics");
			self.capabilities.pull_diagnostics.store(true, Relaxed);
		}

		Ok(InitializeResult {
			server_info: None,
			offset_encoding: None,
			capabilities: ServerCapabilities {
				definition_provider: Some(OneOf::Left(true)),
				hover_provider: Some(HoverProviderCapability::Simple(true)),
				references_provider: Some(OneOf::Left(true)),
				workspace_symbol_provider: Some(OneOf::Left(true)),
				diagnostic_provider: Some(DiagnosticServerCapabilities::Options(Default::default())),
				// XML code actions are done in 1 pass only
				code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
				execute_command_provider: Some(ExecuteCommandOptions {
					commands: vec!["goto_owl".to_string()],
					..Default::default()
				}),
				text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
					change: Some(TextDocumentSyncKind::INCREMENTAL),
					save: Some(TextDocumentSyncSaveOptions::Supported(true)),
					open_close: Some(true),
					..Default::default()
				})),
				completion_provider: Some(CompletionOptions {
					resolve_provider: Some(true),
					trigger_characters: Some(['"', '\'', '.', '_', ',', ' '].iter().map(char::to_string).collect()),
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
	#[instrument(skip_all)]
	async fn shutdown(&self) -> Result<()> {
		Ok(())
	}
	#[instrument(skip(self))]
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
			completions_limit: _,
		} = self;
		document_map.remove(path);
		record_ranges.remove(path);
		ast_map.remove(path);

		self.client
			.publish_diagnostics(params.text_document.uri, vec![], None)
			.await;
	}
	#[instrument(skip_all)]
	async fn initialized(&self, _: InitializedParams) {
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

		let token = NumberOrString::String("odoo-lsp/postinit".to_string());
		let mut progress = None;
		if self
			.client
			.send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams { token: token.clone() })
			.await
			.is_ok()
		{
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
			progress = Some((&self.client, token.clone()));
		}

		let _blocker = self.root_setup.block();
		self.ensure_nonoverlapping_roots();

		for root in self.roots.iter() {
			match self.index.add_root(&root, progress.clone(), false).await {
				Ok(Some(results)) => {
					info!(
						target: "initialized",
						"{} | {} modules | {} records | {} templates | {} models | {} components | {:.2}s",
						root.display(),
						results.module_count,
						results.record_count,
						results.template_count,
						results.model_count,
						results.component_count,
						results.elapsed.as_secs_f64()
					);
				}
				Err(err) => {
					error!("could not add root {}:\n{err}", root.display());
				}
				_ => {}
			}
		}
		drop(_blocker);

		if progress.is_some() {
			_ = self
				.client
				.send_notification::<Progress>(ProgressParams {
					token,
					value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(Default::default())),
				})
				.await;
		}
	}
	#[instrument(skip_all, ret, fields(uri=params.text_document.uri.path()))]
	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		info!("{}", params.text_document.uri.path());
		let language_id = params.text_document.language_id.as_str();
		let split_uri = params.text_document.uri.path().rsplit_once('.');
		let language = match (language_id, split_uri) {
			("python", _) | (_, Some((_, "py"))) => Language::Python,
			("javascript", _) | (_, Some((_, "js"))) => Language::Javascript,
			("xml", _) | (_, Some((_, "xml"))) => Language::Xml,
			_ => {
				debug!("Could not determine language, or language not supported:\nlanguage_id={language_id} split_uri={split_uri:?}");
				return;
			}
		};

		let rope = Rope::from_str(&params.text_document.text);
		self.document_map
			.insert(params.text_document.uri.path().to_string(), Document::new(rope.clone()));

		self.root_setup.wait().await;
		let path = params.text_document.uri.to_file_path().unwrap();
		if self.index.module_of_path(&path).is_none() {
			// outside of root?
			debug!("oob: {}", params.text_document.uri.path());
			let path = params.text_document.uri.to_file_path().ok();
			let mut path = path.as_deref();
			while let Some(path_) = path {
				if tokio::fs::try_exists(path_.with_file_name("__manifest__.py"))
					.await
					.unwrap_or(false)
				{
					if let Some(file_path) = path_.parent().and_then(|p| p.parent()) {
						_ = self
							.index
							.add_root(file_path, None, false)
							.await
							.inspect_err(|err| warn!("failed to add root {}:\n{err}", file_path.display()));
						break;
					}
				}
				path = path_.parent();
			}
		}

		_ = self
			.on_change(backend::TextDocumentItem {
				uri: params.text_document.uri,
				text: Text::Full(params.text_document.text),
				version: params.text_document.version,
				language: Some(language),
				old_rope: None,
				open: true,
			})
			.await
			.inspect_err(|err| warn!("{err}"));
	}
	#[instrument(skip_all)]
	async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
		self.root_setup.wait().await;
		if let [TextDocumentContentChangeEvent {
			range: None,
			range_length: None,
			text,
		}] = params.content_changes.as_mut_slice()
		{
			_ = self
				.on_change(backend::TextDocumentItem {
					uri: params.text_document.uri,
					text: Text::Full(core::mem::take(text)),
					version: params.text_document.version,
					language: None,
					old_rope: None,
					open: false,
				})
				.await
				.inspect_err(|err| warn!("{err}"));
			return;
		}
		let old_rope;
		{
			let mut document = self
				.document_map
				.get_mut(params.text_document.uri.path())
				.expect("Did not build a document");
			old_rope = document.rope.clone();
			// Update the rope
			// TODO: Refactor into method
			// Per the spec (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange),
			// deltas are applied SEQUENTIALLY so we don't have to do any extra processing.
			for change in &params.content_changes {
				if change.range.is_none() && change.range_length.is_none() {
					document.rope = ropey::Rope::from_str(&change.text);
				} else {
					let range = change.range.expect("LSP change event must have a range");
					let range =
						lsp_range_to_char_range(range, &document.rope).expect("did_change applying delta: no range");
					let rope = &mut document.rope;
					rope.remove(range.erase());
					if !change.text.is_empty() {
						rope.insert(range.start.0, &change.text);
					}
				}
			}
		}
		_ = self
			.on_change(backend::TextDocumentItem {
				uri: params.text_document.uri,
				text: Text::Delta(params.content_changes),
				version: params.text_document.version,
				language: None,
				old_rope: Some(old_rope),
				open: false,
			})
			.await
			.inspect_err(|err| warn!("{err}"));
	}
	#[instrument(skip_all)]
	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		if self.root_setup.should_wait() {
			return;
		}
		_ = self.did_save_impl(params).await.inspect_err(|err| warn!("{err}"));
	}
	#[instrument(skip_all)]
	async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		let uri = &params.text_document_position_params.text_document.uri;
		debug!("goto_definition {}", uri.path());
		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			debug!("(goto_definition) unsupported: {}", uri.path());
			return Ok(None);
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			panic!("Bug: did not build a document for {}", uri.path());
		};
		let location = match ext {
			"xml" => self.xml_jump_def(params, document.rope.clone()),
			"py" => self.python_jump_def(params, document.rope.clone()),
			"js" => self.js_jump_def(params, &document.rope),
			_ => {
				debug!("(goto_definition) unsupported: {}", uri.path());
				return Ok(None);
			}
		};

		let location = location
			.map_err(|err| error!("Error retrieving references:\n{err}"))
			.ok()
			.flatten();
		Ok(location.map(GotoDefinitionResponse::Scalar))
	}
	#[instrument(skip_all)]
	async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		let uri = &params.text_document_position.text_document.uri;
		debug!("references {}", uri.path());

		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			return Ok(None); // hit a directory, super unlikely
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			debug!("Bug: did not build a document for {}", uri.path());
			return Ok(None);
		};
		let refs = match ext {
			"py" => self.python_references(params, document.rope.clone()),
			"xml" => self.xml_references(params, document.rope.clone()),
			"js" => self.js_references(params, &document.rope),
			_ => return Ok(None),
		};

		Ok(refs.inspect_err(|err| warn!("{err}")).ok().flatten())
	}
	#[instrument(skip_all)]
	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		let uri = &params.text_document_position.text_document.uri;
		debug!("(completion) {}", uri.path());

		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			return Ok(None); // hit a directory, super unlikely
		};

		let rope = {
			let Some(document) = self.document_map.get(uri.path()) else {
				debug!("Bug: did not build a document for {}", uri.path());
				return Ok(None);
			};
			document.rope.clone()
		};
		if ext == "xml" {
			let completions = self.xml_completions(params, rope).await;
			match completions {
				Ok(ret) => Ok(ret),
				Err(report) => {
					self.client
						.show_message(MessageType::ERROR, format!("error during xml completion:\n{report}"))
						.await;
					Ok(None)
				}
			}
		} else if ext == "py" {
			let ast = {
				let Some(ast) = self.ast_map.get(uri.path()) else {
					debug!("Bug: did not build AST for {}", uri.path());
					return Ok(None);
				};
				ast.value().clone()
			};
			let completions = self.python_completions(params, ast, rope).await;
			match completions {
				Ok(ret) => Ok(ret),
				Err(err) => {
					self.client
						.show_message(MessageType::ERROR, format!("error during python completion:\n{err}"))
						.await;
					Ok(None)
				}
			}
		} else {
			debug!("(completion) unsupported {}", uri.path());
			Ok(None)
		}
	}
	#[instrument(skip_all)]
	async fn completion_resolve(&self, mut completion: CompletionItem) -> Result<CompletionItem> {
		if self.root_setup.should_wait() {
			return Ok(completion);
		}
		'resolve: {
			match &completion.kind {
				Some(CompletionItemKind::CLASS) => {
					let Some(model) = interner().get(&completion.label) else {
						break 'resolve;
					};
					let Some(mut entry) = self.index.models.get_mut(&model.into()) else {
						break 'resolve;
					};
					if let Err(err) = entry.resolve_details() {
						error!("resolving details: {err}");
					}
					completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
						kind: MarkupKind::Markdown,
						value: self.model_docstring(&entry, None, None),
					}))
				}
				Some(CompletionItemKind::FIELD) => {
					_ = self.completion_resolve_field(&mut completion);
				}
				Some(CompletionItemKind::METHOD) => {
					_ = self.completion_resolve_method(&mut completion);
				}
				_ => {}
			}
		}
		Ok(completion)
	}
	#[instrument(skip_all)]
	async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		let uri = &params.text_document_position_params.text_document.uri;
		let document = some!(self.document_map.get(uri.path()));
		let (_, ext) = some!(uri.path().rsplit_once('.'));
		let hover = match ext {
			"py" => self.python_hover(params, document.rope.clone()),
			"xml" => self.xml_hover(params, document.rope.clone()),
			"js" => self.js_hover(params, document.rope.clone()),
			_ => {
				debug!("(hover) unsupported {}", uri.path());
				Ok(None)
			}
		};
		match hover {
			Ok(ret) => Ok(ret),
			Err(err) => {
				error!("{err}");
				Ok(None)
			}
		}
	}
	#[instrument(skip_all)]
	async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
		self.root_setup.wait().await;
		let items = self
			.roots
			.iter()
			.map(|entry| {
				let scope_uri = Url::from_file_path(entry.key()).ok();
				ConfigurationItem {
					section: Some("odoo-lsp".into()),
					scope_uri,
				}
			})
			.collect();
		let mut configs = self.client.configuration(items).await.unwrap_or_default();
		// TODO: Per-folder configuration
		let Some(mut config) = configs
			.pop()
			.and_then(|config| serde_json::from_value::<Config>(config).ok())
		else {
			return;
		};
		// Don't configure modules yet.
		info!("Configuration changed, but roots not implemented yet");
		config.module.take();
		self.on_change_config(config).await;
	}
	#[instrument(skip_all)]
	async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
		self.root_setup.wait().await;
		for added in params.event.added {
			// let file_path = added.uri.path();
			let Ok(file_path) = added.uri.to_file_path() else {
				error!("not a file path: {}", added.uri);
				continue;
			};
			_ = self
				.index
				.add_root(&file_path, None, false)
				.await
				.inspect_err(|err| warn!("failed to add root {}:\n{err}", file_path.display()));
		}
		for removed in params.event.removed {
			let Ok(file_path) = removed.uri.to_file_path() else {
				error!("not a file path: {}", removed.uri);
				continue;
			};
			self.index.remove_root(&file_path);
		}
		self.index.mark_n_sweep();
	}
	#[instrument(skip_all)]
	async fn symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		let query = &params.query;

		let models_by_prefix = self.index.models.by_prefix.read().await;
		let records_by_prefix = self.index.records.by_prefix.read().await;
		let models = models_by_prefix.iter_prefix(query.as_bytes()).flat_map(|(_, key)| {
			self.index.models.get(key).into_iter().flat_map(|entry| {
				#[allow(deprecated)]
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
			#[allow(deprecated)]
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
					keys.iter().flat_map(|key| {
						self.index.records.get(key).and_then(|record| {
							(record.module == module).then(|| to_symbol_information(&record, interner))
						})
					})
				});
			Ok(Some(models.chain(records).take(limit).collect()))
		} else {
			let records = records_by_prefix.iter_prefix(query.as_bytes()).flat_map(|(_, keys)| {
				keys.iter().flat_map(|key| {
					self.index
						.records
						.get(key)
						.map(|record| to_symbol_information(&record, interner))
				})
			});
			Ok(Some(models.chain(records).take(limit).collect()))
		}
	}
	#[instrument(skip_all)]
	async fn diagnostic(&self, params: DocumentDiagnosticParams) -> Result<DocumentDiagnosticReportResult> {
		self.root_setup.wait().await;
		let path = params.text_document.uri.path();
		debug!("{path}");
		let mut diagnostics = vec![];
		if let Some((_, "py")) = path.rsplit_once('.') {
			if let Some(mut document) = self.document_map.get_mut(path) {
				let damage_zone = document.damage_zone.take();
				let rope = &document.rope.clone();
				self.diagnose_python(
					params.text_document.uri.path(),
					rope,
					damage_zone,
					&mut document.diagnostics_cache,
				);
				diagnostics.clone_from(&document.diagnostics_cache);
			}
		}
		Ok(DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
			RelatedFullDocumentDiagnosticReport {
				related_documents: None,
				full_document_diagnostic_report: FullDocumentDiagnosticReport {
					result_id: None,
					items: diagnostics,
				},
			},
		)))
	}
	#[instrument(skip_all)]
	async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		let Some((_, "xml")) = params.text_document.uri.path().rsplit_once('.') else {
			return Ok(None);
		};

		let document = some!(self.document_map.get(params.text_document.uri.path()));

		Ok(self
			.xml_code_actions(params, document.rope.clone())
			.inspect_err(|err| {
				error!("(code_lens) {err}");
			})
			.unwrap_or(None))
	}
	#[instrument(skip_all)]
	async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
		if self.root_setup.should_wait() {
			return Ok(None);
		}
		if let ("goto_owl", [Value::String(_), Value::String(subcomponent)]) =
			(params.command.as_str(), params.arguments.as_slice())
		{
			// FIXME: Subcomponents should not just depend on the component's name,
			// since users can readjust subcomponents' names at will.
			let component = some!(interner().get(subcomponent));
			let location = {
				let component = some!(self.index.components.get(&component.into()));
				some!(component.location.clone())
			};
			_ = self
				.client
				.show_document(ShowDocumentParams {
					uri: Url::from_file_path(location.path.as_string()).unwrap(),
					external: Some(false),
					take_focus: Some(true),
					selection: Some(location.range),
				})
				.await;
		}

		Ok(None)
	}
}

#[tokio::main(worker_threads = 2)]
async fn main() {
	let outlog = std::env::var("ODOO_LSP_LOG").ok().and_then(|var| {
		let path = match var.as_str() {
			#[cfg(unix)]
			"1" => Path::new("/tmp/odoo_lsp.log"),
			_ => Path::new(&var),
		};
		std::fs::OpenOptions::new().create(true).append(true).open(path).ok()
	});
	let registry = tracing_subscriber::registry().with(EnvFilter::from_default_env());
	let layer = tracing_subscriber::fmt::layer()
		.compact()
		.without_time()
		.with_writer(std::io::stderr)
		.with_file(true)
		.with_line_number(true)
		.with_target(cfg!(debug_assertions));
	if let Some(outlog) = outlog {
		registry.with(layer.map_writer(|stderr| stderr.and(outlog))).init();
	} else {
		registry.with(layer).init();
	}

	let args = std::env::args().collect::<Vec<_>>();
	let args = args.iter().skip(1).map(String::as_str).collect::<Vec<_>>();
	let args = cli::parse_args(&args[..]);
	match args.command {
		cli::Command::Run => {}
		cli::Command::TsConfig => {
			_ = cli::tsconfig(&args.addons_path, args.output)
				.await
				.inspect_err(|err| eprintln!("{} tsconfig failed: {err}", loc!()));
			return;
		}
		cli::Command::Init { tsconfig } => {
			_ = cli::init(&args.addons_path, args.output).inspect_err(|err| eprintln!("{} init failed: {err}", loc!()));
			if tsconfig {
				_ = cli::tsconfig(&args.addons_path, None)
					.await
					.inspect_err(|err| eprintln!("{} tsconfig failed: {err}", loc!()));
			}
			return;
		}
		cli::Command::SelfUpdate { nightly } => {
			_ = tokio::task::spawn_blocking(move || cli::self_update(nightly))
				.await
				.unwrap()
				.inspect_err(|err| eprintln!("{} self-update failed: {err}", loc!()));
			return;
		}
	}

	let stdin = tokio::io::stdin();
	let stdout = tokio::io::stdout();

	let (service, socket) = LspService::build(|client| Backend {
		client,
		index: Default::default(),
		document_map: DashMap::with_shard_amount(4),
		record_ranges: DashMap::with_shard_amount(4),
		roots: DashSet::default(),
		capabilities: Default::default(),
		root_setup: Default::default(),
		ast_map: DashMap::with_shard_amount(4),
		symbols_limit: AtomicUsize::new(80),
		references_limit: AtomicUsize::new(80),
		completions_limit: AtomicUsize::new(200),
	})
	.custom_method("odoo-lsp/statistics", Backend::statistics)
	.custom_method("odoo-lsp/debug/usage", |_: &Backend| async move {
		Ok(interner().report_usage())
	})
	.custom_method("odoo-lsp/inspect-type", Backend::debug_inspect_type)
	.finish();

	let service = ServiceBuilder::new()
		.layer(tower::timeout::TimeoutLayer::new(Duration::from_secs(30)))
		.layer_fn(CatchPanic)
		.service(service);
	Server::new(stdin, stdout, socket).serve(service).await;
}
