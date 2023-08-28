use std::pin::Pin;

use futures::Future;
use tokio::{
	runtime::Builder,
	sync::{mpsc, oneshot},
	task::LocalSet,
};

struct AssertSend<T>(T);
unsafe impl<T> Send for AssertSend<T> {}

#[derive(Clone)]
pub struct Isolate {
	send: mpsc::UnboundedSender<AssertSend<Pin<Box<dyn Future<Output = ()>>>>>,
}

impl Isolate {
	pub fn new() -> Self {
		let (send, mut recv) = mpsc::unbounded_channel();
		let rt = Builder::new_current_thread().enable_all().build().unwrap();

		std::thread::spawn(move || {
			let local = LocalSet::new();
			local.spawn_local(async move {
				while let Some(AssertSend(fut)) = recv.recv().await {
					tokio::task::spawn_local(fut);
				}
			});
			rt.block_on(local);
		});
		Self { send }
	}
	pub fn send_task<T, F>(&self, future: impl FnOnce(oneshot::Sender<T>) -> F) -> oneshot::Receiver<T>
	where
		F: Future<Output = ()>,
	{
		let (send, recv) = oneshot::channel();
		let fut: Box<dyn Future<Output = ()>> = Box::new(future(send));
		let fut: Pin<Box<dyn Future<Output = ()> + 'static>> = unsafe { Pin::new_unchecked(core::mem::transmute(fut)) };
		self.send.send(AssertSend(fut)).expect("Isolate has shut down");
		recv
	}
}

#[cfg(test)]
mod tests {
	use super::Isolate;

	#[test]
	fn test_sanity_check() {
		let isolate = Isolate::new();
		let item = Box::leak(Box::new(123)) as *mut i32;
		let recv = isolate.send_task(|send| async move {
			let item = unsafe { Box::from_raw(item) };
			_ = send.send(*item)
		});
		assert_eq!(recv.blocking_recv(), Ok(123));
	}
}
