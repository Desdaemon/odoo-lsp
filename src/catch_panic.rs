//! copied from tower_http::catch_panic

use std::any::Any;
use std::panic::AssertUnwindSafe;
use std::pin::Pin;
use std::task::{Context, Poll};

use futures::FutureExt;
use futures::{future::CatchUnwind, Future};
use tower::Service;

struct CatchPanic<S>(S);
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

	fn poll(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Self::Output> {
		todo!()
		// match self.project().kind.project() {
		// 	KindProj::Panicked { panic_err } => Poll::Ready(Ok(Some({

		// 	}))),
		// }
	}
}
