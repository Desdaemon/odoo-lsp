use std::borrow::Cow;
use std::cmp::Ordering;
use std::sync::atomic::Ordering::Relaxed;

use dashmap::DashMap;
use miette::IntoDiagnostic;
use ropey::Rope;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use xmlparser::{ElementEnd, Token, Tokenizer};

use odoo_lsp::index::ModuleIndex;
use odoo_lsp::utils::*;

#[derive(Debug)]
struct Backend {
	client: Client,
	document_map: DashMap<String, Rope>,
	record_ranges: DashMap<String, Box<[std::ops::Range<ByteOffset>]>>,
	module_index: ModuleIndex,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
		Ok(InitializeResult {
			server_info: None,
			offset_encoding: None,
			capabilities: ServerCapabilities {
				definition_provider: Some(OneOf::Left(true)),
				references_provider: Some(OneOf::Left(true)),
				text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
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
	async fn initialized(&self, _: InitializedParams) {
		self.module_index.in_progress.store(true, Relaxed);
		for workspace in self.client.workspace_folders().await.unwrap().unwrap_or_default() {
			let file_path = workspace.uri.to_file_path().unwrap();
			self.module_index
				.add_root(dbg!(&file_path.to_string_lossy()))
				.await
				.unwrap();
		}
		self.module_index.in_progress.store(false, Relaxed);
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
			eprintln!("Could not determine language, or language not supported: language_id={language_id} split_uri={split_uri:?}");
			return;
		}
		let rope = ropey::Rope::from_str(&params.text_document.text);
		self.document_map
			.insert(params.text_document.uri.path().to_string(), rope.clone());

		let path = params.text_document.uri.to_file_path().unwrap();
		let mut path = Some(path.as_path());
		while let Some(path_) = path {
			if tokio::fs::try_exists(path_.with_file_name("__manifest__.py"))
				.await
				.unwrap_or(false)
			{
				let file_path = path_.parent().expect("has parent").to_string_lossy();
				self.module_index.add_root(&file_path).await.unwrap();
				break;
			}
			path = path_.parent();
		}

		self.on_change(TextDocumentItem {
			uri: params.text_document.uri,
			text: params.text_document.text,
			version: params.text_document.version,
			language: Some(language),
			rope: Some(rope),
		})
		.await
	}
	async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
		self.on_change(TextDocumentItem {
			uri: params.text_document.uri,
			text: std::mem::take(&mut params.content_changes[0].text),
			version: params.text_document.version,
			language: None,
			rope: None,
		})
		.await
	}
	async fn goto_definition(&self, _: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
		Ok(None)
	}
	async fn references(&self, _: ReferenceParams) -> Result<Option<Vec<Location>>> {
		Ok(None)
	}
	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		let uri = &params.text_document_position.text_document.uri;
		eprintln!("completion {}", uri.path());
		let Some((_, "xml")) = uri.path().rsplit_once('.') else {
			panic!("Unsupported file {}", uri.path());
		};
		let Some(document) = self.document_map.get(uri.path()) else {
			panic!("Bug: did not build a rope for {}", uri.path());
		};
		match self.xml_completions(params, document.value()).await {
			Ok(ret) => Ok(ret),
			Err(report) => {
				self.client
					.show_message(MessageType::ERROR, format!("error during completion:\n{report}"))
					.await;
				Ok(None)
			}
		}
	}
	async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}
	async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
		self.module_index.mark_n_sweep();
		for added in params.event.added {
			let file_path = dbg!(added).uri.to_file_path().expect("not a file path");
			let file_path = file_path.as_os_str().to_string_lossy();
			self.module_index.add_root(&file_path).await.unwrap();
		}
		for removed in params.event.removed {
			self.module_index.remove_root(dbg!(removed).uri.path());
		}
	}
	async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
		Ok(None)
	}
}

struct TextDocumentItem {
	uri: Url,
	text: String,
	version: i32,
	language: Option<Language>,
	rope: Option<Rope>,
}

enum Language {
	Python,
	Xml,
	Javascript,
}

fn read_to_end(lex: &mut Tokenizer, local_name: &str) -> miette::Result<Option<std::ops::Range<CharOffset>>> {
	let mut stack = 0;
	loop {
		match lex.next() {
			Some(Ok(Token::ElementStart { local, .. })) if local.as_str() == local_name => stack += 1,
			Some(Ok(Token::ElementEnd {
				end: ElementEnd::Empty,
				span,
			})) if stack == 0 => return Ok(Some(dbg!(span).range().map_unit(CharOffset))),
			Some(Ok(Token::ElementEnd {
				end: ElementEnd::Close(_, local),
				span,
			})) if local.as_str() == local_name => {
				stack -= 1;
				if stack <= 0 {
					return Ok(Some(dbg!(span).range().map_unit(CharOffset)));
				}
			}
			Some(Err(err)) => return Err(err).into_diagnostic(),
			None => return Ok(None),
			_ => {}
		}
	}
}

impl Backend {
	async fn on_change(&self, params: TextDocumentItem) {
		eprintln!("on_change {}", params.uri.path());
		let split_uri = params.uri.path().rsplit_once('.');
		let mut diagnostics = vec![];
		let rope = params.rope.unwrap_or_else(|| ropey::Rope::from_str(&params.text));
		if matches!(split_uri, Some((_, "xml"))) || matches!(params.language, Some(Language::Xml)) {
			// let mut reader = Reader::from_str(&params.text);
			let mut reader = Tokenizer::from(params.text.as_str());
			let mut record_ranges = vec![];
			// for token in reader {
			loop {
				match reader.next() {
					Some(Ok(Token::ElementStart { local, span, .. })) if local.as_str() == "record" => {
						let Ok(Some(end_offset)) = read_to_end(&mut reader, local.as_str()) else {
							eprintln!("end_offset not found");
							continue;
						};
						let CharOffset(end_offset) = end_offset.end;
						let Ok(end_offset) = rope.try_char_to_byte(end_offset) else {
							eprintln!("could not convert end_offset");
							continue;
						};
						let Ok(start_offset) = rope.try_char_to_byte(span.start()) else {
							eprintln!("could not convert start_offset");
							continue;
						};
						let record_range = start_offset..end_offset;
						#[cfg(debug_assertions)]
						{
							let slice = rope.byte_slice(record_range.clone());
							dbg!(Cow::from(slice));
						}
						record_ranges.push(record_range.map_unit(ByteOffset));
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
				.insert(params.uri.path().to_string(), record_ranges.into_boxed_slice());
		}
		self.client
			.publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
			.await;
	}
	async fn xml_completions(
		&self,
		params: CompletionParams,
		rope: &Rope,
	) -> miette::Result<Option<CompletionResponse>> {
		let position = params.text_document_position.position;
		let uri = &params.text_document_position.text_document.uri;
		let Some(ranges) = self.record_ranges.get(uri.path()) else {
			panic!("Could not get record ranges")
		};
		let Some(cursor) = position_to_offset(position, rope) else {
			panic!("cursor")
		};
		let Ok(mut cursor_by_char) = rope.try_byte_to_char(cursor.0) else {
			panic!("cursor_by_char")
		};
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
		let Ok(relative_offset) = rope.try_byte_to_char(record_range.start.0) else {
			panic!("relative_offset")
		};
		cursor_by_char -= relative_offset;

		// let Some(slice) = rope.get_byte_slice(record_range.clone().map_unit(|unit| unit.0)) else {panic!("slice")};
		let slice = rope.byte_slice(record_range.clone().map_unit(|unit| unit.0));
		let slice = Cow::from(slice);
		let reader = Tokenizer::from(&slice[..]);

		let current_module = self
			.module_index
			.modules
			.iter()
			.find(|module| uri.path().contains(module.key()))
			.expect("must be in a module");

		let mut items = vec![];
		for token in reader {
			match token {
				Ok(Token::Attribute { local, value, .. })
					if local.as_str() == "ref"
						&& (value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char) =>
				{
					let needle = &value.as_str()[..cursor_by_char - value.range().start];
					let replace_range = value.range().map_unit(|unit| unit + relative_offset);
					let Some(replace_start) = char_offset_to_position(replace_range.start, rope)
					else {
						panic!("replace_start")
					};
					let Some(replace_end) = char_offset_to_position(replace_range.end, rope) else {
						panic!("replace_end")
					};
					let range = Range::new(replace_start, replace_end);
					let matches = self
						.module_index
						.records
						.iter()
						.filter(|entry| entry.id.starts_with(needle))
						.take(10)
						.map(|entry| {
							let label = if entry.module == current_module.as_str() {
								entry.id.to_string()
							} else {
								format!("{}.{}", entry.module, entry.id)
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
				}
				Err(err) => {
					eprintln!("bug:\n{err}\nin file:\n{}", slice);
					break;
				}
				_ => {}
			}
		}

		Ok(Some(CompletionResponse::List(CompletionList {
			is_incomplete: items.len() >= 10,
			items,
		})))
	}
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
	})
	.finish();

	Server::new(stdin, stdout, socket).serve(service).await;
}
