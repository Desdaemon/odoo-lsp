use std::any::Any;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Relaxed;
use std::sync::OnceLock;
use std::task::Poll;

use dashmap::{DashMap, DashSet};
use faststr::FastStr;
use futures::future::CatchUnwind;
use futures::{Future, FutureExt};
use globwalk::FileType;
use miette::{diagnostic, Context, IntoDiagnostic};
use odoo_lsp::config::{Config, ModuleConfig};
use odoo_lsp::record::Record;
use ropey::Rope;
use serde_json::Value;
use tower::Service;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::{DidChangeConfiguration, Notification, Progress};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{Parser, Query, Tree};
use xmlparser::{ElementEnd, StrSpan, Token, Tokenizer};

use odoo_lsp::index::ModuleIndex;
use odoo_lsp::{format_loc, utils::*};

#[derive(Debug)]
struct Backend {
	client: Client,
	document_map: DashMap<String, Rope>,
	record_ranges: DashMap<String, Box<[std::ops::Range<CharOffset>]>>,
	ast_map: DashMap<String, Tree>,
	module_index: ModuleIndex,
	roots: DashSet<String>,
	capabilities: Capabilities,
	root_setup: AtomicBool,
}

#[derive(Debug, Default)]
struct Capabilities {
	dynamic_config: OnceLock<()>,
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
					eprintln!("could not parse provided config:\n{err}");
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
					Err(err) => eprintln!("could not parse {:?}:\n{err}", path),
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
			_ = self.capabilities.dynamic_config.set(());
		}

		Ok(InitializeResult {
			server_info: None,
			offset_encoding: None,
			capabilities: ServerCapabilities {
				definition_provider: Some(OneOf::Left(true)),
				references_provider: Some(OneOf::Left(true)),
				text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
				completion_provider: Some(CompletionOptions {
					resolve_provider: None,
					trigger_characters: Some(vec!["\"".to_string(), "'".to_string(), ".".to_string()]),
					work_done_progress_options: Default::default(),
					all_commit_characters: None,
					completion_item: None,
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
	async fn did_close(&self, _: DidCloseTextDocumentParams) {}
	async fn initialized(&self, _: InitializedParams) {
		eprintln!("initialized");
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
			match self.module_index.add_root(dbg!(&root.as_str()), progress.clone()).await {
				Ok(Some((modules, records, elapsed))) => {
					eprintln!(
						"Processed {} with {} modules and {} records in {:.2}s",
						root.as_str(),
						modules,
						records,
						elapsed.as_secs_f64()
					);
				}
				Err(err) => {
					eprintln!("(initialized) could not add root {}:\n{err}", root.as_str());
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

		if self.capabilities.dynamic_config.get().is_some() {
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
			eprintln!("Could not determine language, or language not supported:\nlanguage_id={language_id} split_uri={split_uri:?}");
			return;
		}

		let rope = Rope::from_str(&params.text_document.text);
		self.document_map
			.insert(params.text_document.uri.path().to_string(), rope.clone());

		if self
			.module_index
			.module_of_path(Path::new(params.text_document.uri.path()))
			.is_none()
		{
			// outside of root?
			eprintln!("oob: {}", params.text_document.uri.path());
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
						self.module_index
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
			})
			.await
			.report(|| format_loc!("did_change failed"));
			return;
		}
		let mut rope = self
			.document_map
			.get_mut(params.text_document.uri.path())
			.expect("Did not build a rope");
		// TODO: Refactor into method
		for change in &params.content_changes {
			if change.range.is_none() && change.range_length.is_none() {
				*rope.value_mut() = ropey::Rope::from_str(&change.text);
			} else {
				let range = change.range.expect("LSP change event must have a range");
				let Some(range) = lsp_range_to_char_range(range, rope.value().clone()) else {
					continue;
				};
				// if change.text.is_empty() {
				// 	// Prevents an off-by-one error when deleting text
				// 	range = range.start..CharOffset(range.end.0.saturating_sub(1));
				// }
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
		})
		.await
		.report(|| format_loc!("did_change failed"));
	}
	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		if let Some(text) = params.text {
			eprintln!("did_save ignored:\n{text}");
		}
	}
	async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
		let uri = &params.text_document_position_params.text_document.uri;
		eprintln!("goto_definition {}", uri.path());
		let Some((_, "xml")) = uri.path().rsplit_once('.') else {
			eprintln!("Unsupported file {}", uri.path());
			return Ok(None);
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			panic!("Bug: did not build a rope for {}", uri.path());
		};
		let location = self
			.xml_jump_def(params, document.value().clone())
			.await
			.expect("Error retrieving references");

		Ok(location.map(GotoDefinitionResponse::Scalar))
	}
	async fn references(&self, _: ReferenceParams) -> Result<Option<Vec<Location>>> {
		Ok(None)
	}
	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		let uri = &params.text_document_position.text_document.uri;
		eprintln!("completion {}", uri.path());

		let Some((_, ext)) = uri.path().rsplit_once('.') else {
			return Ok(None); // hit a directory, super unlikely
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			eprintln!("Bug: did not build a rope for {}", uri.path());
			return Ok(None);
		};
		if ext == "xml" {
			match self.xml_completions(params, document.value().clone()).await {
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
				eprintln!("Bug: did not build AST for {}", uri.path());
				return Ok(None);
			};
			match self
				.python_completions(params, ast.value().clone(), document.value().clone())
				.await
			{
				Ok(ret) => Ok(ret),
				Err(err) => {
					self.client
						.show_message(MessageType::ERROR, format!("error during python completion:\n{err}"))
						.await;
					Ok(None)
				}
			}
		} else {
			eprintln!("Unsupported file {}", uri.path());
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
			eprintln!("config: {} => {:?}", root.key(), config);
		}
	}
	async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
		self.module_index.mark_n_sweep();
		for added in params.event.added {
			let file_path = added.uri.to_file_path().expect("not a file path");
			let file_path = file_path.as_os_str().to_string_lossy();
			self.module_index
				.add_root(&file_path, None)
				.await
				.report(|| format_loc!("(did_change_workspace_folders) failed to add root {}", file_path));
		}
		for removed in params.event.removed {
			self.module_index.remove_root(removed.uri.path());
		}
	}
	async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
		Ok(None)
	}
}

struct TextDocumentItem {
	uri: Url,
	text: Text,
	version: i32,
	language: Option<Language>,
	rope: Option<Rope>,
}

enum Text {
	Full(String),
	Delta(Vec<TextDocumentContentChangeEvent>),
}

enum Language {
	Python,
	Xml,
	Javascript,
}

impl Backend {
	async fn on_change(&self, params: TextDocumentItem) -> miette::Result<()> {
		let split_uri = params.uri.path().rsplit_once('.');
		let mut diagnostics = vec![];
		let rope = match (params.rope, &params.text) {
			(Some(rope), _) => rope,
			(None, Text::Full(full)) => ropey::Rope::from_str(full),
			(None, Text::Delta(_)) => todo!("No rope and got delta"),
		};
		if matches!(split_uri, Some((_, "xml"))) || matches!(params.language, Some(Language::Xml)) {
			self.on_change_xml(&params.text, &params.uri, rope, &mut diagnostics)
				.await;
		} else if matches!(split_uri, Some((_, "py"))) || matches!(params.language, Some(Language::Python)) {
			self.on_change_python(&params.text, &params.uri, rope).await?;
		}
		self.client
			.publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
			.await;

		Ok(())
	}
	async fn on_change_xml(&self, text: &Text, uri: &Url, rope: Rope, diagnostics: &mut Vec<Diagnostic>) {
		let text = match text {
			Text::Full(full) => Cow::Borrowed(full.as_str()),
			// Assume rope is up to date
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let mut reader = Tokenizer::from(text.as_ref());
		let mut record_ranges = vec![];
		let Some(current_module) = self.module_index.module_of_path(Path::new(uri.path())) else {
			return;
		};
		let current_module = FastStr::from(current_module.to_string());
		loop {
			match reader.next() {
				Some(Ok(Token::ElementStart { local, span, .. })) => {
					let offset = CharOffset(span.start());
					match local.as_str() {
						"record" => {
							let Ok(Some(record)) = Record::from_reader(
								offset,
								current_module.clone(),
								uri.path(),
								&mut reader,
								rope.clone(),
							) else {
								continue;
							};
							let Some(range) = lsp_range_to_char_range(record.location.range, rope.clone()) else {
								continue;
							};
							record_ranges.push(range);
							self.module_index.records.insert(record.qualified_id(), record);
						}
						"template" => {
							let Ok(Some(template)) =
								Record::template(offset, current_module.clone(), uri.path(), &mut reader, rope.clone())
							else {
								continue;
							};
							let Some(range) = lsp_range_to_char_range(template.location.range, rope.clone()) else {
								continue;
							};
							record_ranges.push(range);
							self.module_index.records.insert(template.qualified_id(), template);
						}
						_ => {}
					}
				}
				None => break,
				Some(Err(err)) => {
					let pos = err.pos();
					let pos = Position::new(pos.row - 1, pos.col - 1);
					diagnostics.push(Diagnostic {
						severity: Some(DiagnosticSeverity::WARNING),
						range: Range::new(pos, pos),
						message: format!("could not parse XML:\n{err}"),
						..Default::default()
					});
					break;
				}
				_ => {}
			}
		}
		self.record_ranges
			.insert(uri.path().to_string(), record_ranges.into_boxed_slice());
	}
	async fn on_change_python(&self, text: &Text, uri: &Url, rope: Rope) -> miette::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(tree_sitter_python::language())
			.into_diagnostic()
			.with_context(|| "failed to init python parser")?;
		self.update_ast(text, uri, rope, parser)?;
		Ok(())
	}
	fn update_ast(&self, text: &Text, uri: &Url, rope: Rope, mut parser: Parser) -> miette::Result<()> {
		let ast = self.ast_map.get_mut(uri.path());
		let ast = match (text, ast) {
			(Text::Full(full), _) => parser.parse(full, None),
			(Text::Delta(delta), Some(mut ast)) => {
				// assume rope is up to date
				for change in delta {
					let range = change.range.ok_or_else(|| diagnostic!("delta without range"))?;
					let start =
						position_to_offset(range.start, rope.clone()).ok_or_else(|| diagnostic!("delta start"))?;
					let end = position_to_offset(range.end, rope.clone())
						.ok_or_else(|| diagnostic!("delta end ({change:?})"))?;
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
					let new_end_offset = start_char + len_new;
					let new_end_position = char_offset_to_position(new_end_offset, rope.clone())
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
			(Text::Delta(_), None) => return Err(diagnostic!("(update_ast) got delta but no ast").into()),
		};
		let Some(ast) = ast else { return Ok(()) };
		self.ast_map.insert(uri.path().to_string(), ast);
		Ok(())
	}
	fn record_slice<'rope>(
		&self,
		rope: &'rope Rope,
		uri: &Url,
		position: Position,
	) -> miette::Result<Option<(Cow<'rope, str>, usize, usize)>> {
		let ranges = self
			.record_ranges
			.get(uri.path())
			.ok_or_else(|| diagnostic!("Did not build record ranges"))?;
		let cursor = position_to_char_offset(position, rope.clone()).ok_or_else(|| diagnostic!("cursor"))?;
		let mut cursor_by_char = rope.try_byte_to_char(cursor.0).into_diagnostic()?;
		let Ok(record) = ranges.value().binary_search_by(|range| {
			if cursor < range.start {
				Ordering::Greater
			} else if cursor > range.end {
				Ordering::Less
			} else {
				Ordering::Equal
			}
		}) else {
			eprintln!(
				"Could not find record for cursor={cursor:?} ranges={:?}",
				ranges.value()
			);
			return Ok(None);
		};
		let record_range = &ranges.value()[record];
		let relative_offset = rope.try_byte_to_char(record_range.start.0).into_diagnostic()?;
		cursor_by_char = cursor_by_char.saturating_sub(relative_offset);

		let slice = rope.byte_slice(record_range.clone().map_unit(|unit| unit.0));
		let slice = Cow::from(slice);

		Ok(Some((slice, cursor_by_char, relative_offset)))
	}

	const LIMIT: usize = 20;
	async fn xml_completions(
		&self,
		params: CompletionParams,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let Some((slice, cursor_by_char, relative_offset)) = self.record_slice(&rope, uri, position)? else {
			return Ok(None);
		};
		let reader = Tokenizer::from(&slice[..]);

		// let current_module = self
		// 	.module_index
		// 	.modules
		// 	.iter()
		// 	.find(|module| uri.path().contains(module.key()))
		// 	.expect("must be in a module");
		let current_module = self
			.module_index
			.module_of_path(Path::new(uri.path()))
			.expect("must be in a module");

		enum Tag {
			Record,
			Template,
			Field,
		}

		enum RecordField {
			InheritId,
		}

		let mut items = vec![];
		let mut model_filter = None;
		let mut tag = None::<Tag>;
		let mut record_field = None::<RecordField>;
		let mut cursor_value = None::<StrSpan>;

		for token in reader {
			match token {
				Ok(Token::ElementStart { local, .. }) => match local.as_str() {
					"record" => tag = Some(Tag::Record),
					"template" => tag = Some(Tag::Template),
					"field" => tag = Some(Tag::Field),
					_ => {}
				},
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Record)) && local.as_str() == "model" =>
				{
					model_filter = Some(value.as_str().to_string());
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Field)) && local.as_str() == "name" =>
				{
					match value.as_str() {
						"inherit_id" => record_field = Some(RecordField::InheritId),
						_ => {}
					}
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Field))
						&& local.as_str() == "ref"
						&& (value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char) =>
				{
					cursor_value = Some(value);
				}
				Ok(Token::ElementEnd {
					end: ElementEnd::Empty, ..
				}) if matches!(tag, Some(Tag::Field)) => {
					let (Some(value), Some(record_field)) = (cursor_value, record_field.take()) else {
						continue;
					};
					let needle = &value.as_str()[..cursor_by_char - value.range().start];
					let replace_range = value.range().map_unit(|unit| CharOffset(unit + relative_offset));
					match record_field {
						RecordField::InheritId => self.complete_inherit_id(
							needle,
							replace_range,
							rope.clone(),
							model_filter.as_deref(),
							&current_module,
							&mut items,
						)?,
					}
					break;
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Template))
						&& local.as_str() == "inherit_id"
						&& (value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char) =>
				{
					cursor_value = Some(value);
				}
				Ok(Token::ElementEnd { .. }) if matches!(tag, Some(Tag::Template)) => {
					let Some(value) = cursor_value else { continue };
					let needle = &value.as_str()[..cursor_by_char - value.range().start];
					let replace_range = value.range().map_unit(|unit| CharOffset(unit + relative_offset));
					self.complete_inherit_id(
						needle,
						replace_range,
						rope.clone(),
						model_filter.as_deref(),
						&current_module,
						&mut items,
					)?;
					break;
				}
				Err(err) => {
					eprintln!("bug:\n{err}\nin file:\n{}", slice);
					break;
				}
				_ => {}
			}
		}

		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: items.len() >= Self::LIMIT,
			items,
		})))
	}
	async fn python_completions(
		&self,
		params: CompletionParams,
		ast: Tree,
		rope: Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let Some(ByteOffset(offset)) = position_to_offset(params.text_document_position.position, rope.clone()) else {
			eprintln!(format_loc!("python_completions: invalid offset"));
			return Ok(None);
		};
		let Some(current_module) = self
			.module_index
			.module_of_path(Path::new(params.text_document_position.text_document.uri.path()))
		else {
			eprintln!(format_loc!("python_completions: no current_module"));
			return Ok(None);
		};
		let mut cursor = tree_sitter::QueryCursor::new();
		cursor.set_match_limit(256);
		let bytes = rope.bytes().collect::<Vec<_>>();
		// TODO: Very inexact, is there a better way?
		let range = offset.saturating_sub(50)..bytes.len().min(offset + 200);
		let query = env_ref_query();
		cursor.set_byte_range(range.clone());
		'match_: for match_ in cursor.matches(query, ast.root_node(), &bytes[..]) {
			for xml_id in match_.nodes_for_capture_index(2) {
				if xml_id.byte_range().contains(&offset) {
					let Some(slice) = rope.get_byte_slice(xml_id.byte_range()) else {
						dbg!((xml_id.byte_range(), &range));
						break 'match_;
					};
					let relative_offset = xml_id.byte_range().start;
					// remove the quotes
					let needle = Cow::from(slice.byte_slice(1..offset - relative_offset));
					let range = xml_id.range().start_byte + 1..xml_id.range().end_byte - 1;
					let range = range.map_unit(|unit| CharOffset(rope.byte_to_char(unit)));
					let mut items = vec![];
					self.complete_inherit_id(
						dbg!(&needle),
						dbg!(range),
						rope.clone(),
						None,
						&current_module,
						&mut items,
					)?;
					return Ok(Some(CompletionResponse::List(CompletionList {
						is_incomplete: items.len() >= Self::LIMIT,
						items,
					})));
				}
			}
		}
		Ok(None)
	}
	fn complete_inherit_id(
		&self,
		needle: &str,
		range: std::ops::Range<CharOffset>,
		rope: Rope,
		model_filter: Option<&str>,
		current_module: &str,
		items: &mut Vec<CompletionItem>,
	) -> miette::Result<()> {
		let replace_start =
			char_offset_to_position(range.start.0, rope.clone()).ok_or_else(|| diagnostic!("replace_start"))?;
		let replace_end =
			char_offset_to_position(range.end.0, rope.clone()).ok_or_else(|| diagnostic!("replace_end"))?;
		let range = Range::new(replace_start, replace_end);
		let matches = self
			.module_index
			.records
			.iter()
			.filter(|entry| {
				let id_filter = if let Some((module, xml_id)) = needle.split_once('.') {
					entry.module == module && entry.id.contains(xml_id)
				} else {
					entry.id.contains(needle)
				};
				if let (Some(filter), Some(model)) = (model_filter, &entry.model) {
					id_filter && filter == model
				} else {
					id_filter && model_filter.is_none()
				}
			})
			.take(Self::LIMIT)
			.map(|entry| {
				let label = if entry.module == current_module {
					entry.id.to_string()
				} else {
					entry.qualified_id()
				};
				CompletionItem {
					text_edit: Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
						new_text: label.clone(),
						insert: range,
						replace: range,
					})),
					label,
					..Default::default()
				}
			});
		items.extend(matches);
		Ok(())
	}

	async fn xml_jump_def(&self, params: GotoDefinitionParams, rope: Rope) -> miette::Result<Option<Location>> {
		let position = params.text_document_position_params.position;
		let uri = &params.text_document_position_params.text_document.uri;
		let Some((slice, cursor_by_char, _)) = self.record_slice(&rope, uri, position)? else {
			return Ok(None);
		};
		let reader = Tokenizer::from(&slice[..]);

		enum RecordField {
			InheritId,
		}

		enum Tag {
			Field,
			Template,
		}

		let mut record_field = None::<RecordField>;
		let mut cursor_value = None::<StrSpan>;
		let mut tag = None::<Tag>;

		for token in reader {
			match token {
				Ok(Token::ElementStart { local, .. }) => match local.as_str() {
					"field" => tag = Some(Tag::Field),
					"template" => tag = Some(Tag::Template),
					_ => {}
				},
				Ok(Token::Attribute { local, value, .. }) if matches!(tag, Some(Tag::Field)) => {
					if local.as_str() == "ref" && value.range().contains(&cursor_by_char) {
						cursor_value = Some(value);
					} else if local.as_str() == "name" && value.as_str() == "inherit_id" {
						record_field = Some(RecordField::InheritId);
					}
				}
				Ok(Token::ElementEnd { .. }) if matches!(tag, Some(Tag::Field)) => {
					let Some(cursor_value) = cursor_value else { continue };
					match record_field {
						Some(RecordField::InheritId) => {
							return self.jump_def_inherit_id(cursor_value, uri);
						}
						None => {}
					}
				}
				Ok(Token::Attribute { local, value, .. })
					if matches!(tag, Some(Tag::Template))
						&& local.as_str() == "inherit_id"
						&& value.range().contains(&cursor_by_char) =>
				{
					cursor_value = Some(value);
				}
				Ok(Token::ElementEnd { .. }) if matches!(tag, Some(Tag::Template)) => {
					let Some(cursor_value) = cursor_value else { continue };
					return self.jump_def_inherit_id(cursor_value, uri);
				}
				Err(err) => {
					eprintln!("bug:\n{err}\nin file:\n{}", slice);
					break;
				}
				_ => {}
			}
		}

		Ok(None)
	}

	fn jump_def_inherit_id(&self, cursor_value: StrSpan, uri: &Url) -> miette::Result<Option<Location>> {
		let mut value = Cow::from(cursor_value.as_str());
		if !value.contains('.') {
			'unqualified: {
				if let Some(module) = self.module_index.module_of_path(Path::new(uri.path())) {
					value = format!("{}.{value}", module.key()).into();
					break 'unqualified;
				}
				eprintln!(
					"Could not find a reference for {} in {}: could not infer module",
					cursor_value.as_str(),
					uri.path()
				);
				return Ok(None);
			}
		}
		return Ok(self
			.module_index
			.records
			.get(value.as_ref())
			.map(|entry| entry.location.clone()));
	}

	async fn on_change_config(&self, config: Config) {
		let Some(ModuleConfig { roots: Some(roots), .. }) = config.module else {
			return;
		};
		for root in roots {
			let Ok(root) = dbg!(Path::new(&root).canonicalize()) else {
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
					let Ok(root) = dbg!(dir_entry) else { continue };
					self.roots.insert(root.path().to_string_lossy().to_string());
				}
			} else if tokio::fs::try_exists(&root).await.unwrap_or(false) {
				self.roots.insert(root_display.to_string());
			}
		}
		dbg!(&self.roots);
		self.root_setup.store(true, Relaxed);
	}
}

fn env_ref_query() -> &'static Query {
	static ENV_REF: OnceLock<Query> = OnceLock::new();
	ENV_REF.get_or_init(|| {
		tree_sitter::Query::new(
			tree_sitter_python::language(),
			// matches *.env.ref("_xml_id")
			r#"
			((call
				[(attribute (attribute (_) (identifier) @_env) (identifier) @_ref)
				 (attribute (identifier) @_env (identifier) @_ref)]
				(argument_list . (string) @xml_id))
			 (#eq? @_env "env")
			 (#eq? @_ref "ref")) "#,
		)
		.unwrap()
	})
}

#[tokio::main]
async fn main() {
	env_logger::init();

	let stdin = tokio::io::stdin();
	let stdout = tokio::io::stdout();

	let (service, socket) = LspService::build(|client| Backend {
		client,
		module_index: Default::default(),
		document_map: DashMap::new(),
		record_ranges: DashMap::new(),
		roots: DashSet::new(),
		capabilities: Default::default(),
		root_setup: Default::default(),
		ast_map: DashMap::new(),
	})
	.finish();

	// let layer = tower::layer::layer_fn(|service| {
	// 	struct CatchPanic<S>(S);
	// 	CatchPanic(service)
	// });

	// let service = ServiceBuilder::new().layer_fn(CatchPanic).service(service);

	Server::new(stdin, stdout, socket).serve(service).await;
}

// copied from tower_http::catch_panic
struct CatchPanic<S>(S);
impl<S> Service<tower_lsp::jsonrpc::Request> for CatchPanic<S>
where
	S: Service<tower_lsp::jsonrpc::Request, Response = Option<tower_lsp::jsonrpc::Response>>,
{
	type Response = S::Response;
	type Error = S::Error;
	type Future = CatchPanicFuture<S::Future>;

	#[inline]
	fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<std::result::Result<(), Self::Error>> {
		self.0.poll_ready(cx)
	}

	fn call(&mut self, req: tower_lsp::jsonrpc::Request) -> Self::Future {
		match std::panic::catch_unwind(AssertUnwindSafe(|| self.0.call(req))) {
			Ok(fut) => CatchPanicFuture {
				kind: Kind::Future {
					future: AssertUnwindSafe(fut).catch_unwind(),
				},
			},
			Err(panic_err) => CatchPanicFuture {
				kind: Kind::Panicked { panic_err },
			},
		}
	}
}

pin_project_lite::pin_project! {
	struct CatchPanicFuture<S> {
		#[pin]
		kind: Kind<S>
	}
}
pin_project_lite::pin_project! {
	#[project = KindProj]
	enum Kind<S> {
		Panicked {
			panic_err: Box<dyn Any + Send + 'static>
		},
		Future {
			#[pin]
			future: CatchUnwind<AssertUnwindSafe<S>>
		}
	}
}

impl<S, R, E> Future for CatchPanicFuture<S>
where
	S: Future<Output = core::result::Result<R, E>>,
{
	type Output = core::result::Result<Option<tower_lsp::jsonrpc::Response>, E>;

	fn poll(self: Pin<&mut Self>, _: &mut std::task::Context<'_>) -> Poll<Self::Output> {
		todo!()
		// match self.project().kind.project() {
		// 	KindProj::Panicked { panic_err } => Poll::Ready(Ok(Some({

		// 	}))),
		// }
	}
}
