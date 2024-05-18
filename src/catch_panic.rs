//! copied from [`tower_http::catch_panic`](https://docs.rs/tower-http/latest/tower_http/catch_panic/index.html)

use std::any::Any;
use std::panic::AssertUnwindSafe;
use std::pin::Pin;
use std::task::{ready, Context, Poll};

use futures::FutureExt;
use futures::{future::CatchUnwind, Future};
use tracing::{error, warn};
use tower::Service;
use tower_lsp::jsonrpc::{Error, ErrorCode, Id, Response};

pub struct CatchPanic<S>(pub S);
impl<S> Service<tower_lsp::jsonrpc::Request> for CatchPanic<S>
where
	S: Service<tower_lsp::jsonrpc::Request, Response = Option<tower_lsp::jsonrpc::Response>>,
{
	type Response = S::Response;
	type Error = S::Error;
	type Future = CatchPanicFuture<S::Future>;

	#[inline]
	fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<std::result::Result<(), Self::Error>> {
		self.0.poll_ready(cx)
	}

	fn call(&mut self, req: tower_lsp::jsonrpc::Request) -> Self::Future {
		let id = req.id().cloned();
		match std::panic::catch_unwind(AssertUnwindSafe(|| self.0.call(req))) {
			Ok(fut) => CatchPanicFuture {
				id,
				kind: Kind::Future {
					future: AssertUnwindSafe(fut).catch_unwind(),
				},
			},
			Err(panic_err) => CatchPanicFuture {
				id,
				kind: Kind::Panicked { panic_err },
			},
		}
	}
}

// Read more about pinning, projection and why it is needed here:
// https://doc.rust-lang.org/std/pin/index.html#projections-and-structural-pinning
pin_project_lite::pin_project! {
	pub struct CatchPanicFuture<S> {
		#[pin]
		kind: Kind<S>,
		id: Option<Id>,
	}
}
pin_project_lite::pin_project! {
	#[project = KindProj]
	enum Kind<S> {
		Panicked {
			panic_err: Box<dyn Any + Send + 'static>,
		},
		Future {
			#[pin]
			future: CatchUnwind<AssertUnwindSafe<S>>,
		}
	}
}

fn handle_panic_err(err: &dyn Any) {
	if let Some(msg) = err.downcast_ref::<String>() {
		error!("{msg}");
	} else if let Some(msg) = err.downcast_ref::<&str>() {
		error!("{msg}");
	} else {
		warn!("panic: {err:?} type_id={:?}", err.type_id());
	}
}

enum ServerErrors {
	PreawaitPanic,
	PostawaitPanic,
}

impl<S, E> Future for CatchPanicFuture<S>
where
	// TODO: Hardcoded for LspService, is there a more general bound?
	S: Future<Output = core::result::Result<Option<Response>, E>>,
{
	type Output = core::result::Result<Option<Response>, E>;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		let self_ = self.project();
		match self_.kind.project() {
			KindProj::Panicked { panic_err } => {
				handle_panic_err(panic_err);
				let resp = Response::from_error(
					self_.id.clone().unwrap_or_default(),
					Error::new(ErrorCode::ServerError(ServerErrors::PreawaitPanic as _)),
				);
				Poll::Ready(Ok(Some(resp)))
			}
			KindProj::Future { future } => match ready!(future.poll(cx)) {
				Ok(inner) => Poll::Ready(inner),
				Err(panic_err) => {
					handle_panic_err(panic_err.as_ref());
					let resp = Response::from_error(
						self_.id.clone().unwrap_or_default(),
						Error::new(ErrorCode::ServerError(ServerErrors::PostawaitPanic as _)),
					);
					Poll::Ready(Ok(Some(resp)))
				}
			},
		}
	}
}
