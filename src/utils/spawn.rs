//! Tower middleware that runs each request handler on its own tokio task.
//!
//! `tower-lsp-server`'s `Server::serve` drives every handler future on a *single*
//! task (`buffer_unordered(concurrency_level)` inside a `join!`), so it never uses
//! more than one worker thread for request handling. A handler that does a long
//! synchronous (non-yielding) stretch — e.g. the globwalk directory traversals
//! during a module load triggered by `did_open` — therefore stalls *every* other
//! pending handler, across all workspaces, until it yields.
//!
//! Wrapping the inner service with [`Spawn`] hands each handler to `tokio::spawn`,
//! so the multi-threaded runtime actually runs handlers in parallel and a heavy
//! `did_open` no longer blocks completion/hover/goto for other files.

use std::task::{Context, Poll};

use futures::FutureExt;
use futures::future::Map;
use tokio::task::{JoinError, JoinHandle};
use tower::Service;
use tower_lsp_server::jsonrpc::{Request, Response};
use tracing::error;

type HandlerResult<E> = Result<Option<Response>, E>;

/// Service wrapper that spawns each `call` onto the tokio runtime.
///
/// The inner service's future must be `Send + 'static` (it is — the `tower-lsp`
/// router hands out `BoxFuture<'static, _>` built from an `Arc<Backend>`), which
/// is exactly what `tokio::spawn` requires.
pub struct Spawn<S>(pub S);

impl<S> Service<Request> for Spawn<S>
where
	S: Service<Request, Response = Option<Response>>,
	S::Future: Send + 'static,
	S::Error: Send + 'static,
{
	type Response = S::Response;
	type Error = S::Error;
	type Future = Map<
		JoinHandle<HandlerResult<S::Error>>,
		fn(Result<HandlerResult<S::Error>, JoinError>) -> HandlerResult<S::Error>,
	>;

	#[inline]
	fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
		self.0.poll_ready(cx)
	}

	fn call(&mut self, req: Request) -> Self::Future {
		tokio::spawn(self.0.call(req)).map(|res| {
			res.unwrap_or_else(|join_err| {
				error!("handler task failed to join: {join_err}");
				Ok(None)
			})
		})
	}
}
