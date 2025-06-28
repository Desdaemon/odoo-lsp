//! ## Application flow
//!
//! All LSP requests are implemented in [`server`] on [`Backend`] via the [`LanguageServer`] trait.
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
//!   [interned strings]. While they are not themselves strings, there are [helpers][odoo_lsp::index::_I]
//!   which intern new strings or resolve [`Spur`]s into strings.
//!   Furthermore, [`Symbol`] is a type-safe wrapper around [`Spur`] to prevent mixing
//!   symbols representing different types.
//! - [`ImStr`] is used to avoid excessive memory usage and copying.
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
//! [`LanguageServer`]: tower_lsp_server::LanguageServer
//! [`python`]: odoo_lsp::python
//! [`xml`]: odoo_lsp::xml
//! [`server`]: odoo_lsp::server

// #![warn(clippy::cognitive_complexity)]
#![deny(clippy::unused_async)]
#![deny(clippy::await_holding_invalid_type)]

use std::path::Path;
use std::time::Duration;

use tower_lsp_server::{LspService, Server};
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::{EnvFilter, fmt as tracing_fmt};

use odoo_lsp::backend::Backend;
use odoo_lsp::index::Interner;
use odoo_lsp::utils::CatchPanic;

mod cli;

#[cfg(doc)]
pub use odoo_lsp::*;

#[cfg(unix)]
mod stdio;

fn main() {
	let args = std::env::args().collect::<Vec<_>>();
	let args = parse_args_and_init_logger(&args);

	let mut threads = args.threads.unwrap_or(4);
	match std::thread::available_parallelism() {
		Ok(value) if value.get() <= 4 => {
			threads = 1usize.max(threads / 2);
		}
		_ => {}
	}

	let rt = if threads <= 1 {
		tokio::runtime::Builder::new_current_thread().enable_all().build()
	} else {
		tokio::runtime::Builder::new_multi_thread()
			.worker_threads(threads)
			.enable_all()
			.build()
	}
	.expect("failed to build runtime");
	rt.block_on(async move {
		if cli::run(args).await {
			return;
		}

		#[cfg(unix)]
		let (stdin, stdout) = (
			stdio::PipeStdin::lock_tokio().unwrap(),
			stdio::PipeStdout::lock_tokio().unwrap(),
		);

		#[cfg(not(unix))]
		let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

		let (service, socket) = LspService::build(Backend::new)
			.custom_method("odoo-lsp/debug/usage", |_: &Backend| async move {
				Ok(Interner::report_usage())
			})
			.custom_method("odoo-lsp/inspect-type", Backend::debug_inspect_type)
			.finish();

		let service = tower::ServiceBuilder::new()
			.layer(tower::timeout::TimeoutLayer::new(Duration::from_secs(30)))
			.layer_fn(CatchPanic)
			.service(service);
		Server::new(stdin, stdout, socket).serve(service).await;
	})
}

fn parse_args_and_init_logger(args: &[String]) -> cli::Args<'_> {
	let args = args.iter().skip(1).map(String::as_str).collect::<Vec<_>>();
	let args = cli::parse_args(&args[..]);

	let outlog = std::env::var("ODOO_LSP_LOG").ok().and_then(|var| {
		let path = match var.as_str() {
			#[cfg(unix)]
			"1" => Path::new("/tmp/odoo_lsp.log"),
			_ => Path::new(&var),
		};
		std::fs::OpenOptions::new().create(true).append(true).open(path).ok()
	});
	let registry = tracing_subscriber::registry().with(EnvFilter::from_default_env());
	let layer = tracing_fmt::layer()
		.without_time()
		.with_writer(std::io::stderr)
		.with_file(true)
		.with_line_number(true)
		.with_target(true);

	match args.log_format {
		cli::LogFormat::Compact => {
			let layer = layer.compact();
			match outlog {
				Some(outlog) => registry.with(layer.map_writer(|stderr| stderr.and(outlog))).init(),
				None => registry.with(layer).init(),
			}
		}
		cli::LogFormat::Json => {
			// both must be specified, ref https://github.com/tokio-rs/tracing/issues/1365#issuecomment-828845393
			let layer = layer
				.event_format(tracing_fmt::format().json())
				.fmt_fields(tracing_fmt::format::JsonFields::new());
			match outlog {
				Some(outlog) => registry.with(layer.map_writer(|stderr| stderr.and(outlog))).init(),
				None => registry.with(layer).init(),
			}
		}
	}

	args
}
