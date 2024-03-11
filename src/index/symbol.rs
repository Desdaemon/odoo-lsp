use std::any::type_name;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use lasso::Spur;

use super::Interner;

/// Type-safe wrapper around [Spur].
#[repr(transparent)]
pub struct Symbol<T> {
	inner: Spur,
	_kind: PhantomData<T>,
}

/// An interned path split into (root, subpath) to save memory.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PathSymbol(pub Spur, pub Spur);

impl PathSymbol {
	/// Panics if `root` is not a parent of `path`.
	pub fn strip_root(root: Spur, path: &Path) -> Self {
		let path = path.strip_prefix(interner().resolve(&root)).unwrap();
		let path = interner().get_or_intern(&path.to_string_lossy());
		PathSymbol(root, path)
	}
	pub fn empty() -> Self {
		let empty = interner().get_or_intern_static(".");
		PathSymbol(empty, empty)
	}
	pub fn as_path(&self) -> PathBuf {
		let root = Path::new(interner().resolve(&self.0));
		root.join(interner().resolve(&self.1))
	}
}

impl Display for PathSymbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let interner = interner();
		f.write_fmt(format_args!(
			"{}/{}",
			interner.resolve(&self.0),
			interner.resolve(&self.1)
		))
	}
}

impl Debug for PathSymbol {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("PathSymbol")
			.field(&interner().resolve(&self.0))
			.field(&interner().resolve(&self.1))
			.finish()
	}
}

impl<T> Debug for Symbol<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Symbol")
			.field(&type_name::<T>())
			.field(&interner().resolve(&self.inner))
			.finish()
	}
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

impl<T> From<Symbol<T>> for Spur {
	#[inline]
	fn from(value: Symbol<T>) -> Self {
		value.inner
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

impl<T> PartialOrd for Symbol<T> {
	#[inline]
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.inner.partial_cmp(&other.inner)
	}
}

impl<T> Ord for Symbol<T> {
	#[inline]
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.inner.cmp(&other.inner)
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

pub fn interner() -> &'static Interner {
	static INTERNER: OnceLock<Interner> = OnceLock::new();
	INTERNER.get_or_init(|| {
		let interner = Interner::default();
		let common_names = include_str!("common_names.txt").lines();
		for name in common_names {
			interner.get_or_intern_static(name);
		}
		interner
	})
}
