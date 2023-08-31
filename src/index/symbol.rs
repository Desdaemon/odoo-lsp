use std::{hash::Hash, marker::PhantomData, ops::Deref};

use lasso::Spur;

/// Type-safe wrapper around [Spur].
#[derive(Debug)]
pub struct Symbol<T> {
	inner: Spur,
	_kind: PhantomData<T>,
}

// TODO: Remove in favor of strict-type symbols
impl<T> From<Spur> for Symbol<T> {
	#[inline]
	fn from(inner: Spur) -> Self {
		Symbol {
			inner,
			_kind: PhantomData,
		}
	}
}

impl<T> Clone for Symbol<T> {
	#[inline]
	fn clone(&self) -> Self {
		*self
	}
}

impl<T> Copy for Symbol<T> {}
impl<T> Eq for Symbol<T> {}
impl<T> PartialEq for Symbol<T> {
	#[inline]
	fn eq(&self, other: &Self) -> bool {
		self.inner.eq(&other.inner)
	}
}

impl<T> Hash for Symbol<T> {
	#[inline]
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.inner.hash(state)
	}
}

impl<T> Deref for Symbol<T> {
	type Target = Spur;
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}
