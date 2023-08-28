use std::marker::PhantomData;

use serde::{Deserialize, Serialize};
use tower_lsp::{
	lsp_types::{notification::Notification, CompletionResponse, ProgressToken},
	Client,
};

#[derive(Debug)]
pub struct Partial<T>(PhantomData<T>);

impl<T> Notification for Partial<T>
where
	T: Serialize + for<'de> Deserialize<'de>,
{
	type Params = PartialParams<T>;
	const METHOD: &'static str = "$/progress";
}

#[derive(Serialize, Deserialize)]
pub struct PartialParams<T> {
	pub token: ProgressToken,
	pub value: T,
}

pub async fn emit_partial<T, U>(client: &Client, token: ProgressToken, value: U)
where
	T: Notification<Params = PartialParams<U>>,
{
	_ = client.send_notification::<T>(PartialParams { token, value }).await;
}

/// If the first response is [CompletionResponse::List],
/// subsequent [CompletionResponse::Array] responses add to the original list.
pub type PartialCompletions = Partial<CompletionResponse>;
