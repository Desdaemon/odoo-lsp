use std::time::Duration;

use async_lsp::{LanguageServer, lsp_types::*};
use iai_callgrind::{
	Callgrind, EntryPoint, FlamegraphConfig, LibraryBenchmarkConfig, OutputFormat, library_benchmark,
	library_benchmark_group, main,
};

fn init_and_shutdown(max_concurrency: usize) {
	let mut builder = match max_concurrency {
		1 => tokio::runtime::Builder::new_current_thread(),
		_ => tokio::runtime::Builder::new_multi_thread(),
	};
	let rt = builder.worker_threads(max_concurrency).enable_all().build().unwrap();
	rt.block_on(async move {
		#[cfg(target_os = "linux")]
		iai_callgrind::client_requests::callgrind::start_instrumentation();

		let mut server = odoo_lsp_tests::server::setup_lsp_server(Some(max_concurrency));

		let root = std::env::current_dir().unwrap();
		let root = root.join("benches/odoo");

		server
			.initialize(InitializeParams {
				workspace_folders: Some(vec![WorkspaceFolder {
					uri: Url::from_file_path(&root).expect("failed to parse uri from root"),
					name: root
						.file_name()
						.map(|ostr| ostr.to_string_lossy().into_owned())
						.unwrap_or("odoo-lsp".to_string()),
				}]),
				..Default::default()
			})
			.await
			.expect("initialization failed");

		server.initialized(InitializedParams {}).expect("[initialized] failed");

		if false {
			// not activating this yet; the timeout at the end is still being hit.
			// tokio::time::sleep(Duration::from_secs(30)).await;

			let path = root.join("addons/account/models/partner.py");
			let text = std::fs::read_to_string(&path)
				.inspect_err(|err| eprintln!("failed to read {}: {err}", path.display()))
				.unwrap();
			let uri = Url::from_file_path(&path).expect("failed to parser uri for path");
			server
				.did_open(DidOpenTextDocumentParams {
					text_document: TextDocumentItem {
						uri: uri.clone(),
						language_id: "python".to_string(),
						version: 0,
						text,
					},
				})
				.expect("[did_open] failed");

			// record.fiscal_country_codes
			let completions = server.completion(CompletionParams {
				text_document_position: TextDocumentPositionParams {
					text_document: TextDocumentIdentifier { uri },
					position: Position {
						line: 334,
						character: 19,
					},
				},
				work_done_progress_params: Default::default(),
				partial_result_params: Default::default(),
				context: None,
			});
			tokio::time::timeout(Duration::from_secs(10), completions)
				.await
				.expect("timed out after 60 seconds")
				.expect("[completions] failed");
		}

		server.shutdown(()).await.expect("[shutdown] failed");
		server.exit(()).expect("[exit] failed");

		#[cfg(target_os = "linux")]
		iai_callgrind::client_requests::callgrind::stop_instrumentation();
	});
}

#[library_benchmark]
#[bench::parallel(4)]
#[bench::serial(1)]
fn bench_standard(concurrency: usize) {
	#[cfg(target_os = "linux")]
	iai_callgrind::client_requests::callgrind::stop_instrumentation();
	init_and_shutdown(concurrency)
}

library_benchmark_group!(
	name = bench_standard_group;
	benchmarks = bench_standard
);

// set to collect at start so that children threads start instrumenting
// immediately, while we manually control when the main thread starts
main!(
	config = LibraryBenchmarkConfig::default()
		.output_format(OutputFormat::default().show_intermediate(true))
		.tool(Callgrind::default()
			.entry_point(EntryPoint::Custom("tokio::runtime::task::waker::wake_by_val".to_string()))
			.flamegraph(FlamegraphConfig::default())
		);
	library_benchmark_groups = bench_standard_group
);
