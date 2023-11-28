use std::{
	future::Future,
	future::Ready,
	pin::Pin,
	task::{Context, Poll},
};

use futures::{future::BoxFuture, FutureExt};

pub enum FutureOr<'a, T> {
	Ready(Ready<T>),
	Pending(BoxFuture<'a, T>),
}

impl<T> Future for FutureOr<'_, T> {
	type Output = T;
	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		match self.get_mut() {
			Self::Ready(ready) => ready.poll_unpin(cx),
			Self::Pending(fut) => fut.as_mut().poll(cx),
		}
	}
}
