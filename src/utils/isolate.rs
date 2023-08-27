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
