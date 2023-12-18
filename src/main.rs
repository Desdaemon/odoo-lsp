use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::Relaxed;

use catch_panic::CatchPanic;
use dashmap::{DashMap, DashSet};
use log::{debug, error, info};
use ropey::Rope;
use serde_json::Value;
use tower::ServiceBuilder;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::{DidChangeConfiguration, Notification, Progress};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, LspService, Server};

use odoo_lsp::config::Config;
use odoo_lsp::index::{interner, Interner};
use odoo_lsp::{format_loc, some, utils::*};

mod analyze;
mod backend;
mod catch_panic;
mod cli;
mod js;
mod python;
mod xml;

use backend::{Backend, Document, Language, Text};

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

		if let Some(TextDocumentClientCapabilities {
			diagnostic: Some(diagnostics),
			..
		}) = params.capabilities.text_document
		{
			dbg!(diagnostics);
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
				text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
					change: Some(TextDocumentSyncKind::INCREMENTAL),
					save: Some(TextDocumentSyncSaveOptions::Supported(true)),
					open_close: Some(true),
					..Default::default()
				})),
				completion_provider: Some(CompletionOptions {
					resolve_provider: Some(true),
					trigger_characters: Some(['"', '\'', '.', '_'].iter().map(char::to_string).collect()),
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

		for workspace in self.client.workspace_folders().await.unwrap().unwrap_or_default() {
			let Ok(file_path) = workspace.uri.to_file_path() else {
				continue;
			};
			self.roots.insert(file_path.to_string_lossy().to_string());
		}

		self.root_setup.block_waiters();
		for root in self.roots.iter() {
			match self.index.add_root(root.as_str(), progress.clone(), false).await {
				Ok(Some(results)) => {
					info!(
						"{} | {} modules | {} records | {} templates | {} models | {} components | {:.2}s",
						root.as_str(),
						results.module_count,
						results.record_count,
						results.template_count,
						results.model_count,
						results.component_count,
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

		self.root_setup.release_waiters();

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
		self.root_setup.wait().await;
		// let language;
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
						if let Some(file_path) = path_.parent() {
							let file_path = file_path.to_string_lossy();
							self.index
								.add_root(&file_path, None, false)
								.await
								.report(|| format_loc!("failed to add root {}", file_path));
							break;
						}
					}
					path = path_.parent();
				}
			}
		}

		self.on_change(backend::TextDocumentItem {
			uri: params.text_document.uri,
			text: Text::Full(params.text_document.text),
			version: params.text_document.version,
			language: Some(language),
			old_rope: None,
			open: true,
		})
		.await
		.report(|| format_loc!("did_open failed"))
	}
	async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
		self.root_setup.wait().await;
		if let [TextDocumentContentChangeEvent {
			range: None,
			range_length: None,
			text,
		}] = params.content_changes.as_mut_slice()
		{
			self.on_change(backend::TextDocumentItem {
				uri: params.text_document.uri,
				text: Text::Full(core::mem::take(text)),
				version: params.text_document.version,
				language: None,
				old_rope: None,
				open: false,
			})
			.await
			.report(|| format_loc!("did_change failed"));
			return;
		}
		let mut document = self
			.document_map
			.get_mut(params.text_document.uri.path())
			.expect("Did not build a document");
		let old_rope = document.rope.clone();
		// Update the rope
		// TODO: Refactor into method
		// Per the spec (https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange),
		// deltas are applied SEQUENTIALLY so we don't have to do any extra processing.
		for change in &params.content_changes {
			if change.range.is_none() && change.range_length.is_none() {
				document.value_mut().rope = ropey::Rope::from_str(&change.text);
			} else {
				let range = change.range.expect("LSP change event must have a range");
				let range =
					lsp_range_to_char_range(range, &document.rope).expect("did_change applying delta: no range");
				let rope = &mut document.value_mut().rope;
				rope.remove(range.erase());
				if !change.text.is_empty() {
					rope.insert(range.start.0, &change.text);
				}
			}
		}
		drop(document);
		self.on_change(backend::TextDocumentItem {
			uri: params.text_document.uri,
			text: Text::Delta(params.content_changes),
			version: params.text_document.version,
			language: None,
			old_rope: Some(old_rope),
			open: false,
		})
		.await
		.report(|| format_loc!("did_change failed"));
	}
	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		self.root_setup.wait().await;
		let uri = params.text_document.uri;
		debug!("did_save {}", uri.path());
		if uri.path().ends_with(".py") {
			let rope = &self.document_map.get(uri.path()).unwrap().rope;
			let text = Cow::from(rope.slice(..));
			self.update_models(Text::Full(text.into_owned()), &uri, rope.clone())
				.await
				.report(|| format_loc!("update_models"));
			if !self.capabilities.pull_diagnostics.load(Relaxed) {
				let mut document = self
					.document_map
					.try_get_mut(uri.path())
					.expect(format_loc!("deadlock"))
					.unwrap();
				self.diagnose_python(
					&uri,
					&rope,
					document.damage_zone.take(),
					&mut document.diagnostics_cache,
				);
				self.client
					.publish_diagnostics(uri, document.diagnostics_cache.clone(), None)
					.await;
			}
		}
	}
	async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
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
			"xml" => self.xml_jump_def(params, document.rope.clone()).await,
			"py" => self.python_jump_def(params, document.rope.clone()).await,
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
	async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
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

		Ok(refs.map_err(|err| error!("{err}")).ok().flatten())
	}
	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		let uri = &params.text_document_position.text_document.uri;
		debug!("completion {}", uri.path());

		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			return Ok(None); // hit a directory, super unlikely
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			debug!("Bug: did not build a document for {}", uri.path());
			return Ok(None);
		};
		if ext == "xml" {
			let completions = self.xml_completions(params, document.rope.clone()).await;
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
			let Some(ast) = self.ast_map.get(uri.path()) else {
				debug!("Bug: did not build AST for {}", uri.path());
				return Ok(None);
			};
			let completions = self
				.python_completions(params, ast.value().clone(), document.rope.clone())
				.await;
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
	async fn completion_resolve(&self, mut completion: CompletionItem) -> Result<CompletionItem> {
		'resolve: {
			match &completion.kind {
				Some(CompletionItemKind::CLASS) => {
					let Some(model) = interner().get(&completion.label) else {
						break 'resolve;
					};
					let Some(mut entry) = self
						.index
						.models
						.try_get_mut(&model.into())
						.expect(format_loc!("deadlock"))
					else {
						break 'resolve;
					};
					if let Err(err) = entry.resolve_details().await {
						error!("resolving details: {err}");
					}
					completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
						kind: MarkupKind::Markdown,
						value: self.model_docstring(&entry, None, None),
					}))
				}
				Some(CompletionItemKind::FIELD) => {
					// NOTE: This was injected by complete_field_name().
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
					let Some(mut entry) = self
						.index
						.models
						.try_get_mut(&model.into())
						.expect(format_loc!("deadlock"))
					else {
						break 'resolve;
					};
					let Some(fields) = &mut entry.fields else {
						break 'resolve;
					};
					let field_entry = fields.get(&field.into()).cloned();
					drop(entry);
					if let Some(field_entry) = field_entry {
						let type_ = interner().resolve(&field_entry.type_);
						completion.detail = match self.index.models.normalize_field_relation(field.into(), model) {
							None => Some(format!("{type_}(…)")),
							Some(relation) => {
								let relation = interner().resolve(&relation);
								Some(format!("{type_}(\"{relation}\", …)"))
							}
						};
						completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
							kind: MarkupKind::Markdown,
							value: self.field_docstring(field_name, &field_entry, false),
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
		let (_, ext) = some!(uri.path().rsplit_once('.'));
		if ext == "py" {
			match self.python_hover(params, document.rope.clone()).await {
				Ok(ret) => Ok(ret),
				Err(err) => {
					error!("{err}");
					Ok(None)
				}
			}
		} else {
			debug!("(hover) unsupported: {}", uri.path());
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
				.add_root(&file_path, None, false)
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
	async fn symbol(&self, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
		let query = &params.query;

		let models_by_prefix = self.index.models.by_prefix.read().await;
		let records_by_prefix = self.index.records.by_prefix.read().await;
		let models = models_by_prefix.iter_prefix_str(query).flat_map(|(_, key)| {
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
			let records = records_by_prefix.iter_prefix_str(xml_id_query).flat_map(|(_, keys)| {
				keys.keys().flat_map(|key| {
					self.index
						.records
						.get(&key)
						.and_then(|record| (record.module == module).then(|| to_symbol_information(&record, interner)))
				})
			});
			Ok(Some(models.chain(records).take(limit).collect()))
		} else {
			let records = records_by_prefix.iter_prefix_str(query).flat_map(|(_, keys)| {
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
	async fn diagnostic(&self, params: DocumentDiagnosticParams) -> Result<DocumentDiagnosticReportResult> {
		let path = params.text_document.uri.path();
		debug!("(diagnostics) {path}");
		let mut diagnostics = vec![];
		if let Some((_, "py")) = path.rsplit_once('.') {
			if let Some(mut document) = self.document_map.try_get_mut(path).expect(format_loc!("deadlock")) {
				let damage_zone = document.damage_zone.take();
				let rope = &document.rope.clone();
				self.diagnose_python(
					&params.text_document.uri,
					rope,
					damage_zone,
					&mut document.diagnostics_cache,
				);
				diagnostics = document.diagnostics_cache.clone();
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
}

#[tokio::main]
async fn main() {
	env_logger::init();

	let args = std::env::args().collect::<Vec<_>>();
	let args = args.iter().skip(1).map(String::as_str).collect::<Vec<_>>();
	let args = cli::parse_args(&args[..]);
	match args.command {
		cli::Command::Run => {}
		cli::Command::TsConfig => {
			return cli::tsconfig(&args.addons_path, args.output)
				.await
				.report(|| format_loc!("tsconfig failed"))
		}
		cli::Command::Init { tsconfig } => {
			cli::init(&args.addons_path, args.output).report(|| format_loc!("init failed"));
			if tsconfig {
				cli::tsconfig(&args.addons_path, None)
					.await
					.report(|| format_loc!("tsconfig failed"));
			}
			return;
		}
	}

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
	})
	.custom_method("odoo-lsp/statistics", Backend::statistics)
	.finish();

	let service = ServiceBuilder::new().layer_fn(CatchPanic).service(service);
	Server::new(stdin, stdout, socket).serve(service).await;
}
