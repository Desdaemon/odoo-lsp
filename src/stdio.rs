//! Copied from [`async_lsp::stdio`]  
//! Link: https://github.com/oxalica/async-lsp/blob/main/src/stdio.rs  
//! Original source is licensed under MIT or Apache v2

use std::io::{self, Error, ErrorKind, IoSlice, Read, Result, StdinLock, StdoutLock, Write};
use std::os::unix::io::{AsFd, AsRawFd, BorrowedFd, RawFd};

use rustix::fs::{fcntl_getfl, fcntl_setfl, fstat, FileType, OFlags};

#[derive(Debug)]
struct NonBlocking<T: AsFd> {
	inner: T,
	prev_flags: OFlags,
}

impl<T: AsFd> NonBlocking<T> {
	fn new(inner: T) -> Result<Self> {
		let ft = FileType::from_raw_mode(fstat(&inner)?.st_mode);
		if !matches!(ft, FileType::Fifo | FileType::Socket | FileType::CharacterDevice) {
			return Err(Error::new(
				ErrorKind::Other,
				format!("File type {ft:?} is not pipe-like"),
			));
		}

		let prev_flags = fcntl_getfl(&inner)?;
		fcntl_setfl(&inner, prev_flags | OFlags::NONBLOCK)?;
		Ok(Self { inner, prev_flags })
	}
}

impl<T: AsFd> Drop for NonBlocking<T> {
	fn drop(&mut self) {
		let _: std::result::Result<_, _> = fcntl_setfl(&self.inner, self.prev_flags);
	}
}

/// Locked stdin for asynchronous read.
#[derive(Debug)]
pub struct PipeStdin {
	inner: NonBlocking<StdinLock<'static>>,
}

impl PipeStdin {
	/// Lock stdin with pipe-like backend and set it to asynchronous mode.
	///
	/// # Errors
	///
	/// Fails if the underlying FD is not pipe-like, or error occurs when setting mode.
	/// See [module level documentation](index.html) for more details.
	pub fn lock() -> Result<Self> {
		let inner = NonBlocking::new(io::stdin().lock())?;
		Ok(Self { inner })
	}
}

impl AsFd for PipeStdin {
	fn as_fd(&self) -> BorrowedFd<'_> {
		self.inner.inner.as_fd()
	}
}

impl AsRawFd for PipeStdin {
	fn as_raw_fd(&self) -> RawFd {
		self.inner.inner.as_raw_fd()
	}
}

// NB. Bypass the internal buffer of `StdinLock` here to keep this in sync with the readiness of
// the underlying FD (which is relied by the I/O re/actor).
impl Read for &'_ PipeStdin {
	fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
		rustix::io::read(self, buf).map_err(Into::into)
	}

	fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> Result<usize> {
		rustix::io::readv(self, bufs).map_err(Into::into)
	}
}

impl Read for PipeStdin {
	fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
		<&PipeStdin>::read(&mut &*self, buf)
	}

	fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> Result<usize> {
		<&PipeStdin>::read_vectored(&mut &*self, bufs)
	}
}

/// Locked stdout for asynchronous read.
#[derive(Debug)]
pub struct PipeStdout {
	inner: NonBlocking<StdoutLock<'static>>,
}

impl PipeStdout {
	/// Lock stdout with pipe-like backend and set it to asynchronous mode.
	///
	/// # Errors
	/// Fails if the underlying FD is not pipe-like, or error occurs when setting mode.
	/// See [module level documentation](index.html) for more details.
	pub fn lock() -> Result<Self> {
		let inner = NonBlocking::new(io::stdout().lock())?;
		Ok(Self { inner })
	}
}

impl AsFd for PipeStdout {
	fn as_fd(&self) -> BorrowedFd<'_> {
		self.inner.inner.as_fd()
	}
}

impl AsRawFd for PipeStdout {
	fn as_raw_fd(&self) -> RawFd {
		self.inner.inner.as_raw_fd()
	}
}

// NB. See `Read` impl.
impl Write for &'_ PipeStdout {
	fn write(&mut self, buf: &[u8]) -> Result<usize> {
		rustix::io::write(self, buf).map_err(Into::into)
	}

	fn flush(&mut self) -> Result<()> {
		Ok(())
	}

	fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> Result<usize> {
		rustix::io::writev(self, bufs).map_err(Into::into)
	}
}

impl Write for PipeStdout {
	fn write(&mut self, buf: &[u8]) -> Result<usize> {
		<&PipeStdout>::write(&mut &*self, buf)
	}

	fn flush(&mut self) -> Result<()> {
		<&PipeStdout>::flush(&mut &*self)
	}

	fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> Result<usize> {
		<&PipeStdout>::write_vectored(&mut &*self, bufs)
	}
}

mod tokio_impl {
	use std::pin::Pin;
	use std::task::{Context, Poll};

	use futures::ready;
	use tokio::io::unix::AsyncFd;
	use tokio::io::{Interest, ReadBuf};

	use super::*;

	pub struct TokioPipeStdin {
		inner: AsyncFd<PipeStdin>,
	}

	impl futures::AsyncRead for TokioPipeStdin {
		fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut [u8]) -> Poll<Result<usize>> {
			loop {
				let mut guard = ready!(self.inner.poll_read_ready(cx))?;
				match guard.try_io(|inner| inner.get_ref().read(buf)) {
					Ok(ret) => return Poll::Ready(ret),
					Err(_would_block) => continue,
				}
			}
		}

		fn poll_read_vectored(
			self: Pin<&mut Self>,
			cx: &mut Context<'_>,
			bufs: &mut [io::IoSliceMut<'_>],
		) -> Poll<Result<usize>> {
			loop {
				let mut guard = ready!(self.inner.poll_read_ready(cx))?;
				match guard.try_io(|inner| inner.get_ref().read_vectored(bufs)) {
					Ok(ret) => return Poll::Ready(ret),
					Err(_would_block) => continue,
				}
			}
		}
	}

	impl tokio::io::AsyncRead for TokioPipeStdin {
		fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<io::Result<()>> {
			let len = loop {
				let mut guard = ready!(self.inner.poll_read_ready(cx))?;
				match guard.try_io(|inner| {
					// SAFETY: `read()` does not de-initialize any byte.
					let (written, _) = rustix::io::read(inner, unsafe { buf.unfilled_mut() })?;
					Ok(written.len())
				}) {
					Ok(ret) => break ret?,
					Err(_would_block) => continue,
				}
			};
			unsafe {
				buf.assume_init(len);
			}
			buf.advance(len);
			Poll::Ready(Ok(()))
		}
	}

	impl PipeStdin {
		/// Shortcut to [`PipeStdin::lock`] and then [`PipeStdin::try_into_tokio`].
		///
		/// # Errors
		///
		/// Fails if cannot create [`AsyncFd`].
		pub fn lock_tokio() -> Result<TokioPipeStdin> {
			Self::lock()?.try_into_tokio()
		}

		/// Register the FD to the tokio runtime and return a tokio compatible reader.
		///
		/// # Errors
		///
		/// Fails if cannot create [`AsyncFd`].
		pub fn try_into_tokio(self) -> Result<TokioPipeStdin> {
			let inner = AsyncFd::with_interest(self, Interest::READABLE)?;
			Ok(TokioPipeStdin { inner })
		}
	}

	pub struct TokioPipeStdout {
		inner: AsyncFd<PipeStdout>,
	}

	impl futures::AsyncWrite for TokioPipeStdout {
		fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
			loop {
				let mut guard = ready!(self.inner.poll_write_ready(cx))?;
				match guard.try_io(|inner| inner.get_ref().write(buf)) {
					Ok(result) => return Poll::Ready(result),
					Err(_would_block) => continue,
				}
			}
		}

		fn poll_write_vectored(
			self: Pin<&mut Self>,
			cx: &mut Context<'_>,
			bufs: &[IoSlice<'_>],
		) -> Poll<Result<usize>> {
			loop {
				let mut guard = ready!(self.inner.poll_write_ready(cx))?;
				match guard.try_io(|inner| inner.get_ref().write_vectored(bufs)) {
					Ok(result) => return Poll::Ready(result),
					Err(_would_block) => continue,
				}
			}
		}

		fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
			Poll::Ready(Ok(()))
		}

		fn poll_close(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
			Poll::Ready(Ok(()))
		}
	}

	impl tokio::io::AsyncWrite for TokioPipeStdout {
		fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
			<Self as futures::AsyncWrite>::poll_write(self, cx, buf)
		}

		fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
			Poll::Ready(Ok(()))
		}

		fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
			Poll::Ready(Ok(()))
		}
	}

	impl PipeStdout {
		/// Shortcut to [`PipeStdout::lock`] and then [`PipeStdout::try_into_tokio`].
		///
		/// # Errors
		///
		/// Fails if cannot create [`AsyncFd`].
		pub fn lock_tokio() -> Result<TokioPipeStdout> {
			Self::lock()?.try_into_tokio()
		}

		/// Register the FD to the tokio runtime and return a tokio compatible writer.
		///
		/// # Errors
		///
		/// Fails if cannot create [`AsyncFd`].
		pub fn try_into_tokio(self) -> Result<TokioPipeStdout> {
			let inner = AsyncFd::with_interest(self, Interest::WRITABLE)?;
			Ok(TokioPipeStdout { inner })
		}
	}
}
