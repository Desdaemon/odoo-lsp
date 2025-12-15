//! Hazmat module to isolate unsafe operations to and from atomic values.

use core::marker::PhantomData;
use core::num::NonZero;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread::ThreadId;

use bytemuck::{NoUninit, Pod, PodInOption, ZeroableInOption};

#[derive(NoUninit, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub(super) struct PodThreadId(NonZero<u64>);

// SAFETY: NonZero<u64> is also implemented these by bytemuck
unsafe impl ZeroableInOption for PodThreadId {}
unsafe impl PodInOption for PodThreadId {}

impl From<ThreadId> for PodThreadId {
	fn from(value: ThreadId) -> Self {
		const { assert!(size_of::<Self>() == size_of::<ThreadId>()) }
		// SAFETY: Guaranteed by the above assertion
		unsafe { core::mem::transmute(value) }
	}
}

impl From<PodThreadId> for ThreadId {
	fn from(value: PodThreadId) -> Self {
		const { assert!(size_of::<Self>() == size_of::<PodThreadId>()) }
		// SAFETY: Guaranteed by the above assertion
		unsafe { core::mem::transmute(value) }
	}
}

/// In-house replacement for [std::sync::atomic::Atomic] while it is unstable.
#[repr(transparent)]
pub(super) struct AtomicCell<T>(AtomicUsize, PhantomData<T>);

impl<T: Pod> AtomicCell<T> {
	fn checked_transmute(inner: T) -> usize {
		const {
			assert!(
				size_of::<T>() <= size_of::<usize>(),
				"Cannot instantiate AtomicCell<T> where T is bigger than usize"
			);
		}
		let mut buf = [0u8; size_of::<usize>()];
		let src = bytemuck::bytes_of(&inner);
		buf[..src.len()].copy_from_slice(src);
		usize::from_ne_bytes(buf)
	}
	pub fn new(inner: T) -> Self {
		Self(AtomicUsize::new(Self::checked_transmute(inner)), PhantomData)
	}
	pub fn set(&self, inner: T) {
		self.0.store(Self::checked_transmute(inner), Ordering::SeqCst)
	}
	pub fn get(&self) -> T {
		let value = self.0.load(Ordering::SeqCst).to_ne_bytes();
		*bytemuck::from_bytes(&value[..size_of::<T>()])
	}
}

impl<T: Default + Pod> Default for AtomicCell<T> {
	fn default() -> Self {
		Self::new(Default::default())
	}
}
