use std::time::Duration;

use async_lsp::MainLoop;
use tower::ServiceBuilder;
use tower_lsp_server::{LspService, Server};

use odoo_lsp::backend::Backend;
use odoo_lsp::utils::CatchPanic;

pub fn setup_lsp_server(concurrency_level: Option<usize>) -> async_lsp::ServerSocket {
	let (service, socket) = LspService::build(Backend::new)
		.custom_method("odoo-lsp/inspect-type", Backend::debug_inspect_type)
		.finish();

	let service = ServiceBuilder::new()
		.layer(tower::timeout::TimeoutLayer::new(Duration::from_secs(30)))
		.layer_fn(CatchPanic)
		.service(service);

	let (server_read, server_write) = tokio::io::simplex(64);
	let (client_read, client_write) = tokio::io::simplex(64);

	let (client_read, server_write) = (
		tokio_util::compat::TokioAsyncReadCompatExt::compat(client_read),
		tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(server_write),
	);

	let (ml, client) = MainLoop::new_client(|socket| socket);

	tokio::spawn(
		Server::new(server_read, client_write, socket)
			.concurrency_level(concurrency_level.unwrap_or(4))
			.serve(service),
	);
	tokio::spawn(ml.run_buffered(client_read, server_write));

	client
}
