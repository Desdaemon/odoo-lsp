use std::collections::HashMap;
use std::time::Duration;

use tower::retry::backoff::{Backoff, ExponentialBackoff, MakeBackoff};
use tower::retry::{backoff::ExponentialBackoffMaker, Policy};
use tower::util::rng::HasherRng;
use tower_lsp::jsonrpc::Id;

#[derive(Clone)]
pub struct LspRetryPolicy {
	backoff_factory: ExponentialBackoffMaker,
	backoffs: HashMap<Id, ExponentialBackoff>,
}

impl LspRetryPolicy {
	pub fn new(max_retry_delay: usize) -> Self {
		LspRetryPolicy {
			backoff_factory: ExponentialBackoffMaker::new(
				Duration::from_secs(0),
				Duration::from_secs(max_retry_delay as _),
				25.0,
				HasherRng::new(),
			)
			.expect("invalid backoff maker config"),
			backoffs: Default::default(),
		}
	}
}

impl<E> Policy<tower_lsp::jsonrpc::Request, Option<tower_lsp::jsonrpc::Response>, E> for LspRetryPolicy {
	type Future = tokio::time::Sleep;

	fn retry(
		&mut self,
		req: &mut tower_lsp::jsonrpc::Request,
		result: &mut Result<Option<tower_lsp::jsonrpc::Response>, E>,
	) -> Option<Self::Future> {
		match result {
			Ok(_) => {
				if let Some(id) = req.id() {
					self.backoffs.remove(id);
				}
				None
			}
			Err(_) => {
				let Some(id) = req.id() else { return None };
				Some(
					self.backoffs
						.entry(id.clone())
						.or_insert_with(|| self.backoff_factory.make_backoff())
						.next_backoff(),
				)
			}
		}
	}

	fn clone_request(&mut self, req: &tower_lsp::jsonrpc::Request) -> Option<tower_lsp::jsonrpc::Request> {
		Some(req.clone())
	}
}
