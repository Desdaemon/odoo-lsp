use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::sync::OnceLock;
use std::time::Duration;

use async_lsp::lsp_types::*;
use async_lsp::LanguageServer;
use futures::{stream::FuturesUnordered, StreamExt};
use pretty_assertions::{Comparison, StrComparison};
use rstest::*;
use tree_sitter::Parser;
use tree_sitter::Query;
use tree_sitter::QueryCursor;
use ts_macros::query;

use crate::server;

use std::sync::Once;

static TRACING_INIT: Once = Once::new();

fn init_tracing() {
	TRACING_INIT.call_once(|| {
		tracing_subscriber::fmt()
			.with_env_filter(tracing_subscriber::EnvFilter::builder().parse_lossy("warn,odoo_lsp=trace"))
			.init();
	});
}

#[rstest]
#[timeout(Duration::from_secs(1))]
#[tokio::test(flavor = "current_thread")]
async fn fixture_test(#[files("fixtures/*")] root: PathBuf) {
	std::env::set_current_dir(&root).unwrap();
	let mut server = server::setup_lsp_server(None);
	init_tracing();

	_ = server
		.initialize(InitializeParams {
			workspace_folders: Some(vec![WorkspaceFolder {
				uri: Url::from_file_path(&root).unwrap(),
				name: root
					.file_name()
					.map(|ostr| ostr.to_string_lossy().into_owned())
					.unwrap_or("odoo-lsp".to_string()),
			}]),
			..Default::default()
		})
		.await
		.expect("initialization failed");

	_ = server.notify::<notification::Initialized>(InitializedParams {});

	// <!> collect expected samples
	let mut expected = gather_expected(&root, TestLanguages::Python);
	expected.extend(gather_expected(&root, TestLanguages::Xml));
	expected.retain(|_, expected| {
		!expected.complete.is_empty() || !expected.diag.is_empty() || !expected.r#type.is_empty()
	});

	// <!> compare and run
	let mut expected: FuturesUnordered<_> = expected
		.into_iter()
		.map(|(path, expected)| {
			let mut server = server.clone();
			let text = match std::fs::read_to_string(&path) {
				Ok(t) => t,
				Err(e) => panic!("Failed to read {}: {e}", path.display()),
			};
			async move {
				let mut diffs = vec![];

				let language_id = match path.extension().unwrap().to_string_lossy().as_ref() {
					"py" => "python",
					"xml" => "xml",
					unk => panic!("unknown file extension {unk}"),
				}
				.to_string();

				let uri = Url::from_file_path(&path).unwrap();

				_ = server.did_open(DidOpenTextDocumentParams {
					text_document: TextDocumentItem {
						uri: uri.clone(),
						language_id,
						version: 1,
						text: text.clone(),
					},
				});

				let diags = server
					.document_diagnostic(DocumentDiagnosticParams {
						text_document: TextDocumentIdentifier { uri: uri.clone() },
						identifier: None,
						previous_result_id: None,
						work_done_progress_params: Default::default(),
						partial_result_params: Default::default(),
					})
					.await;
				if let Ok(DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(report))) = diags {
					let actual = report
						.full_document_diagnostic_report
						.items
						.iter()
						.map(|diag| (diag.range.start, diag.message.clone()))
						.collect::<Vec<_>>();
					if expected.diag[..] != actual[..] {
						diffs.push(format!(
							"[diag] {}\n{}",
							path.display(),
							Comparison::new(&expected.diag[..], &actual[..]),
						));
					}
				} else {
					diffs.push(format!("[diag] failed to get diag: {diags:?}\n\tat {}", path.display()))
				}

				let completions: FuturesUnordered<_> = expected
					.complete
					.iter()
					.map(|(position, expected)| {
						let mut server = server.clone();
						let uri = uri.clone();
						let path = path.display();
						async move {
							let completions = server
								.completion(CompletionParams {
									text_document_position: TextDocumentPositionParams {
										text_document: TextDocumentIdentifier { uri },
										position: *position,
									},
									work_done_progress_params: Default::default(),
									partial_result_params: Default::default(),
									context: None,
								})
								.await;
							if let Ok(Some(CompletionResponse::List(list))) = completions {
								let actual = list.items.iter().map(|comp| comp.label.to_string()).collect::<Vec<_>>();
								if expected[..] != actual[..] {
									format!(
										"[complete] in {path}:{}:{}\n{}",
										position.line + 1,
										position.character + 1,
										Comparison::new(&expected[..], &actual[..]),
									)
								} else {
									String::new()
								}
							} else {
								format!(
									"[complete] failed to get completions: {completions:?}\n\tat {path}:{}:{}",
									position.line + 1,
									position.character + 1
								)
							}
						}
					})
					.collect();

				let types: FuturesUnordered<_> = expected
					.r#type
					.iter()
					.map(|(position, expected)| {
						let server = server.clone();
						let uri = uri.clone();
						let path = path.display();
						async move {
							let r#type = server
								.request::<InspectType>(TextDocumentPositionParams {
									text_document: TextDocumentIdentifier { uri },
									position: *position,
								})
								.await;
							if let Ok(actual) = r#type {
								let actual = actual.as_deref().unwrap_or("None");
								if expected != actual {
									format!(
										"[type] in {path}:{}:{}\n{}",
										position.line + 1,
										position.character + 1,
										StrComparison::new(expected, actual),
									)
								} else {
									String::new()
								}
							} else {
								format!(
									"[type] failed to get type: {type:?}\n\tat {path}:{}:{}",
									position.line + 1,
									position.character + 1
								)
							}
						}
					})
					.collect();

				let mut items = completions.chain(types);
				while let Some(diff) = items.next().await {
					diffs.push(diff);
				}

				diffs
			}
		})
		.collect();

	let mut messages = vec![];
	while let Some(diffs) = expected.next().await {
		messages.extend(diffs);
	}

	_ = server.shutdown(()).await;
	_ = server.exit(());

	let message = messages.join("\n");
	let message = message.trim_ascii();
	assert!(message.is_empty(), "{message}");
}

query! {
	PyExpected();

	((comment) @diag
	(#match? @diag "\\^diag "))

	((comment) @complete
	(#match? @complete "\\^complete "))

	((comment) @type
	(#match? @type "\\^type "))
}

fn xml_query() -> &'static Query {
	static QUERY: OnceLock<Query> = OnceLock::new();
	const XML_QUERY: &str = r#"
		((Comment) @diag
		(#match? @diag "\\^diag "))

		((Comment) @complete
		(#match? @complete "\\^complete "))
	"#;
	QUERY.get_or_init(|| Query::new(&tree_sitter_xml::LANGUAGE_XML.into(), XML_QUERY).unwrap())
}

#[derive(Default)]
struct Expected {
	diag: Vec<(Position, String)>,
	complete: Vec<(Position, Vec<String>)>,
	r#type: Vec<(Position, String)>,
}

enum TestLanguages {
	Python,
	Xml,
}

enum InspectType {}
impl request::Request for InspectType {
	type Params = TextDocumentPositionParams;
	type Result = Option<String>;
	const METHOD: &str = "odoo-lsp/inspect-type";
}

fn gather_expected(root: &Path, lang: TestLanguages) -> HashMap<PathBuf, Expected> {
	let (glob, query, language) = match lang {
		TestLanguages::Python => ("**/*.py", PyExpected::query as fn() -> _, tree_sitter_python::LANGUAGE),
		TestLanguages::Xml => ("**/*.xml", xml_query as _, tree_sitter_xml::LANGUAGE_XML),
	};

	let path = root.join(glob).to_string_lossy().into_owned();
	let mut expected = HashMap::<_, Expected>::new();

	for file in globwalk::glob(&path).unwrap() {
		let Ok(file) = file else { continue };
		let contents = std::fs::read_to_string(file.path()).unwrap().into_bytes();
		let expected = expected.entry(file.into_path()).or_default();
		let mut parser = Parser::new();
		parser.set_language(&language.into()).unwrap();
		let ast = parser.parse(&contents, None).unwrap();
		let mut cursor = QueryCursor::new();

		for (match_, _) in cursor.captures(query(), ast.root_node(), &contents[..]) {
			for capture in match_.captures {
				let text = &contents[capture.node.byte_range()][1..];
				let Some(idx) = text.iter().position(|ch| *ch == b'^') else {
					continue;
				};
				let mut text = text.trim_ascii();
				if matches!(lang, TestLanguages::Xml) {
					text = text.strip_suffix(b"-->").unwrap_or(text).trim_ascii_end();
				}
				let range = capture.node.range();
				let position = Position {
					line: range.start_point.row as u32 - 1,
					character: (range.start_point.column + idx + 1) as u32,
				};
				if let Some(complete) = text.strip_prefix(b"^complete ") {
					let completions = String::from_utf8_lossy(complete);
					let completions = completions.split(' ').map(String::from).collect();
					(expected.complete).push((position, completions))
				} else if let Some(diag) = text.strip_prefix(b"^diag ") {
					(expected.diag).push((position, String::from_utf8_lossy(diag).to_string()));
				} else if let Some(r#type) = text.strip_prefix(b"^type ") {
					(expected.r#type).push((position, String::from_utf8_lossy(r#type).to_string()));
				}
			}
		}
	}

	expected
}
