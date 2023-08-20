use std::borrow::Cow;
use std::cmp::Ordering;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Relaxed;

use dashmap::{DashMap, DashSet};
use faststr::FastStr;
use globwalk::FileType;
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use xmlparser::{ElementEnd, Token, Tokenizer};

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
					trigger_characters: Some(vec!["\"".to_string(), "'".to_string()]),
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
				// execute_command_provider: Some(ExecuteCommandOptions {
				//     commands: vec!["dummy.do_something".to_string()],
				//     work_done_progress_options: Default::default(),
				// }),
				// definition: Some(GotoCapability::default()),
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

	async fn did_close(&self, _: DidCloseTextDocumentParams) {}

	async fn goto_definition(&self, _: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
		// let definition = async {
		//     let uri = params.text_document_position_params.text_document.uri;
		//     // let ast = self.ast_map.get(uri.as_str())?;
		//     let rope = self.document_map.get(uri.as_str())?;

		//     let position = params.text_document_position_params.position;
		//     let char = rope.try_line_to_char(position.line as usize).ok()?;
		//     let offset = char + position.character as usize;
		//     // self.client.log_message(MessageType::INFO, &format!("{:#?}, {}", ast.value(), offset)).await;
		//     let span = get_definition(&ast, offset);
		//     self.client
		//         .log_message(MessageType::INFO, &format!("{:?}, ", span))
		//         .await;
		//     span.and_then(|(_, range)| {
		//         let start_position = offset_to_position(range.start, &rope)?;
		//         let end_position = offset_to_position(range.end, &rope)?;

		//         let range = Range::new(start_position, end_position);

		//         Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
		//     })
		// }
		// .await;
		Ok(None)
	}
	async fn references(&self, _: ReferenceParams) -> Result<Option<Vec<Location>>> {
		// let uri = params.text_document_position.text_document.uri;;
		// let Some((_, "xml")) = uri.path().rsplit_once('.') else {
		//     panic!("Unsupported file {}", uri.path());
		// };
		// let Some(document) = self.document_map.get(uri.path()) else {
		//     panic!("Bug: did not build a rope for {}", uri.path());
		// };
		// let reference_list = || -> Option<Vec<Location>> {
		//     let uri = params.text_document_position.text_document.uri;
		//     // let ast = self.ast_map.get(&uri.to_string())?;
		//     let rope = self.document_map.get(&uri.to_string())?;

		//     let position = params.text_document_position.position;
		//     let char = rope.try_line_to_char(position.line as usize).ok()?;
		//     let offset = char + position.character as usize;
		//     let reference_list = get_reference(&ast, offset, false);
		//     let ret = reference_list
		//         .into_iter()
		//         .filter_map(|(_, range)| {
		//             let start_position = offset_to_position(range.start, &rope)?;
		//             let end_position = offset_to_position(range.end, &rope)?;

		//             let range = Range::new(start_position, end_position);

		//             Some(Location::new(uri.clone(), range))
		//         })
		//         .collect::<Vec<_>>();
		//     Some(ret)
		// }();
		Ok(None)
	}

	async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
		let uri = &params.text_document_position.text_document.uri;
		eprintln!("completion {}", uri.path());
		// let position = params.text_document_position.position;
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

	async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
		self.client
			.log_message(MessageType::INFO, "configuration changed!")
			.await;
	}

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

	async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
		self.client
			.log_message(MessageType::INFO, "watched files have changed!")
			.await;
	}

	async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
		// self.client
		//     .log_message(MessageType::INFO, "command executed!")
		//     .await;

		// match self.client.apply_edit(WorkspaceEdit::default()).await {
		//     Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
		//     Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
		//     Err(err) => self.client.log_message(MessageType::ERROR, err).await,
		// }

		Ok(None)
	}
}

// #[derive(Debug, Deserialize, Serialize)]
// struct InlayHintParams {
//     path: String,
// }

// enum CustomNotification {}
// impl Notification for CustomNotification {
//     type Params = InlayHintParams;
//     const METHOD: &'static str = "custom/notification";
// }

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
		let Some(ranges) = self
			.record_ranges
			.get(params.text_document_position.text_document.uri.path())
		else {
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

		let mut items = vec![];
		for token in reader {
			match token {
				Ok(Token::Attribute { local, value, .. })
					if local.as_str() == "ref"
						&& (value.range().contains(&cursor_by_char) || value.range().end == cursor_by_char) =>
				{
					let needle = &value.as_str()[..cursor_by_char - value.range().start];
					let replace_range = value.range();
					let Some(replace_start) = char_offset_to_position(replace_range.start + relative_offset, rope)
					else {
						panic!("replace_start")
					};
					let Some(replace_end) = char_offset_to_position(replace_range.end + relative_offset, rope) else {
						panic!("replace_end")
					};
					let range = Range::new(replace_start, replace_end);
					let matches = self
						.module_index
						.records
						.iter()
						.filter(|entry| entry.id.starts_with(needle))
						.take(10)
						.map(|entry| CompletionItem {
							label: dbg!(&entry.id).to_string(),
							text_edit: Some(CompletionTextEdit::Edit(TextEdit {
								range,
								new_text: entry.id.to_string(),
							})),
							..Default::default()
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

#[derive(Debug, Default)]
pub struct ModuleIndex {
	in_progress: AtomicBool,
	roots: DashSet<String>,
	records: DashMap<String, Record>,
}

#[derive(Debug)]
pub struct Record {
	deleted: bool,
	id: FastStr,
	module: FastStr,
	model: Option<FastStr>,
	/// (inherit_module?, xml_id)
	inherit_id: Option<(Option<FastStr>, FastStr)>,
	location: Location,
}

impl Record {
	pub fn from_reader<S>(
		offset: CharOffset,
		module: S,
		uri: &str,
		reader: &mut Tokenizer,
		rope: &Rope,
	) -> miette::Result<Self>
	where
		S: Into<FastStr>,
	{
		let mut id = None;
		let mut model = None;
		let start =
			char_offset_to_position(offset.0, rope).ok_or_else(|| diagnostic!("Failed to parse start location"))?;
		let mut inherit_id = None;
		let mut end = None;

		// for attr in attrs {
		//     let attr = attr.into_diagnostic()?;
		//     match attr.key.local_name().as_ref() {
		//         b"id" => id = Some(String::from_utf8_lossy(&attr.value).to_string().into()),
		//         b"model" => model = Some(String::from_utf8_lossy(&attr.value).to_string().into()),
		//         _ => {}
		//     }
		// }

		loop {
			match reader.next() {
				Some(Ok(Token::ElementEnd {
					end: ElementEnd::Close(_, close),
					span,
					..
				})) if close.as_str() == "record" => {
					end = Some(span.end());
					break;
				}
				Some(Ok(Token::Attribute { local, value, .. })) => match local.as_str() {
					"id" => id = Some(value.as_str().to_string().into()),
					"model" => model = Some(value.as_str().to_string().into()),
					_ => {}
				},
				Some(Ok(Token::ElementStart { local, .. })) if local.as_str() == "field" => {
					let mut is_inherit_id = false;
					let mut maybe_inherit_id = None;
					loop {
						match reader.next() {
							Some(Ok(Token::Attribute { local, value, .. }))
								if local.as_str() == "name" && value.as_str() == "inherit_id" =>
							{
								is_inherit_id = true
							}
							Some(Ok(Token::Attribute { local, value, .. })) if local.as_str() == "ref" => {
								maybe_inherit_id = Some(value.as_str().to_string());
							}
							Some(Ok(Token::ElementEnd {
								end: ElementEnd::Empty, ..
							})) => break,
							Some(Ok(Token::ElementEnd {
								end: ElementEnd::Close(_, local),
								..
							})) if local.as_str() == "field" => break,
							None | Some(Err(_)) => break,
							_ => {}
						}
					}
					if !is_inherit_id {
						continue;
					}
					let Some(maybe_inherit_id) = maybe_inherit_id else {
						continue;
					};
					if let Some((module, xml_id)) = maybe_inherit_id.split_once('.') {
						inherit_id = Some((Some(module.to_string().into()), xml_id.to_string().into()));
					} else {
						inherit_id = Some((None, maybe_inherit_id.into()));
					}
				}
				None | Some(Err(_)) => break,
				_ => {}
			}
		}
		let end = end.ok_or_else(|| diagnostic!("Unbound range for record"))?;
		let end = char_offset_to_position(end, rope).ok_or_else(|| diagnostic!("Failed to parse end location"))?;

		Ok(Self {
			deleted: false,
			id: id.ok_or_else(|| diagnostic!("record without `id`"))?,
			module: module.into(),
			model,
			inherit_id,
			location: Location {
				uri: format!("file://{uri}").parse().into_diagnostic()?,
				range: Range { start, end },
			},
		})
	}
}

impl ModuleIndex {
	fn mark_n_sweep(&self) {
		self.records.retain(|_, record| !record.deleted)
	}
	async fn add_root(&self, root: &str) -> miette::Result<()> {
		if !self.roots.insert(dbg!(root).to_string()) {
			return Ok(());
		}
		let modules = globwalk::glob_builder(format!("{root}/**/__manifest__.py"))
			.file_type(FileType::FILE | FileType::SYMLINK)
			.build()
			.into_diagnostic()
			.with_context(|| format!("Could not glob into {root}"))?;

		let mut handles: Vec<tokio::task::JoinHandle<miette::Result<_>>> = vec![];
		for module in modules {
			let module = module.into_diagnostic()?;
			let module_dir = dbg!(&module)
				.path()
				.parent()
				.map(Path::to_path_buf)
				.ok_or_else(|| miette::diagnostic!("Unexpected empty path"))?;
			let module_name = module_dir.file_name().unwrap().to_string_lossy().to_string();
			let module_name = FastStr::from(module_name);
			let xmls = globwalk::glob_builder(dbg!(format!("{}/**/*.xml", module_dir.display())))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format!("Could not glob into {:?}", module.path()))?;
			for xml in xmls {
				let xml = xml.into_diagnostic()?;
				let path = dbg!(&xml).path().to_path_buf();
				let path_uri = path.to_string_lossy().to_string();
				let module_name = module_name.clone();
				handles.push(tokio::spawn(async move {
					let file = tokio::fs::read(&path)
						.await
						.into_diagnostic()
						.with_context(|| format!("Could not read {}", path.display()))?;
					let file = std::str::from_utf8(&file)
						.into_diagnostic()
						.with_context(|| "non-utf8 file")?;
					let mut reader = Tokenizer::from(file);
					let mut out = vec![];
					loop {
						match reader.next() {
							Some(Ok(Token::ElementStart { local, span, .. })) if local.as_str() == "record" => {
								let record = Record::from_reader(
									CharOffset(span.start()),
									module_name.clone(),
									&path_uri,
									&mut reader,
									&Rope::from_str(file),
								)?;
								out.push(record);
							}
							None | Some(Err(_)) => break,
							_ => {}
						}
					}

					miette::Result::Ok(out)
				}));
			}
		}
		for result in futures::future::join_all(handles).await {
			let records = result.into_diagnostic()??;
			for record in records {
				self.records.insert(record.id.to_string(), record);
			}
		}

		Ok(())
	}
	fn remove_root(&self, root: &str) {
		self.roots.remove(root);
		for mut entry in self.records.iter_mut() {
			if root.contains(entry.module.as_str()) {
				entry.deleted = true;
			}
		}
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
		// ast_map: Default::default(),
		// ast_map: DashMap::new(),
		// semantic_token_map: DashMap::new(),
	})
	.finish();

	Server::new(stdin, stdout, socket).serve(service).await;
}

// fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
//     let line = rope.try_char_to_line(offset).ok()?;
//     let first_char_of_line = rope.try_line_to_char(line).ok()?;
//     let column = offset - first_char_of_line;
//     Some(Position::new(line as u32, column as u32))
// }

fn position_to_offset(position: Position, rope: &Rope) -> Option<ByteOffset> {
	// let line_char_start = rope.try_line_to_char(position.line as usize).ok()?;
	// let line_char_end = line_char_start + position.character as usize;
	let CharOffset(offset) = position_to_char_offset(position, rope)?;
	let byte_offset = rope.try_char_to_byte(offset).ok()?;
	Some(ByteOffset(byte_offset))
}

fn position_to_char_offset(position: Position, rope: &Rope) -> Option<CharOffset> {
	let line_offset_in_char = rope.try_line_to_char(position.line as usize).ok()?;
	let line_end = line_offset_in_char + position.character as usize;
	Some(CharOffset(line_end))
}

fn char_offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
	let line = rope.try_char_to_line(offset).ok()?;
	let line_offset = rope.try_line_to_char(line).ok()?;
	Some(Position::new(line as u32, (offset - line_offset) as u32))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ByteOffset(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CharOffset(usize);

trait RangeExt {
	type Unit;
	fn map_unit<F, V>(self, op: F) -> std::ops::Range<V>
	where
		F: FnMut(Self::Unit) -> V;
}

impl<T> RangeExt for std::ops::Range<T> {
	type Unit = T;

	#[inline]
	fn map_unit<F, V>(self, mut op: F) -> std::ops::Range<V>
	where
		F: FnMut(Self::Unit) -> V,
	{
		op(self.start)..op(self.end)
	}
}
