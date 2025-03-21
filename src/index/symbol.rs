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
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathSymbol(Spur, Spur);

impl PathSymbol {
	/// Panics if `root` is not a parent of `path`.
	pub fn strip_root(root: Spur, path: &Path) -> Self {
		let path = path.strip_prefix(interner().resolve(&root)).unwrap();
		let path = _I(path.to_string_lossy());
		PathSymbol(root, path)
	}
	pub fn empty() -> Self {
		let empty = interner().get_or_intern_static(".");
		PathSymbol(empty, empty)
	}
	pub fn to_path(&self) -> PathBuf {
		let root = Path::new(interner().resolve(&self.0));
		root.join(interner().resolve(&self.1))
	}
	pub fn as_string(&self) -> String {
		let path = self.to_path();
		path.to_string_lossy().into_owned()
	}
	#[inline]
	pub fn subpath(&self) -> &str {
		interner().resolve(&self.1)
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
		let tpath = type_name::<T>();
		let name = tpath.rsplit_once("::").map(|(_, last)| last).unwrap_or(tpath);
		f.debug_tuple(&format!("Symbol<{name}>"))
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
		Some(self.inner.cmp(&other.inner))
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

fn interner() -> &'static Interner {
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

#[inline]
#[allow(non_snake_case)]
#[doc(alias = "get_or_intern")]
pub fn _I<T: AsRef<str>>(string: T) -> Spur {
	fn impl_(string: &str) -> Spur {
		interner().get_or_intern(string)
	}
	impl_(string.as_ref())
}

#[inline]
#[allow(non_snake_case)]
#[doc(alias = "intern_resolve")]
pub fn _R<T: Into<Spur>>(token: T) -> &'static str {
	interner().resolve(&token.into())
}

#[inline]
#[allow(non_snake_case)]
#[doc(alias = "intern_get")]
pub fn _G<T: AsRef<str>>(string: T) -> Option<Spur> {
	fn impl_(string: &str) -> Option<Spur> {
		interner().get(string)
	}
	impl_(string.as_ref())
}

#[inline]
#[allow(non_snake_case)]
#[doc(alias = "get_or_intern_path")]
pub fn _P<T: AsRef<Path>>(string: T) -> Spur {
	fn impl_(string: &Path) -> Spur {
		interner().get_or_intern_path(string)
	}
	impl_(string.as_ref())
}

impl super::Interner {
	pub fn report_usage() -> serde_json::Value {
		let self_ = interner();
		let items = self_
			.get_counter()
			.iter()
			.map(|entry| (*entry.key(), *entry.value()))
			.collect::<Vec<_>>();

		let most_common = {
			let mut items = items.clone();
			items.sort_by(|(_, a), (_, z)| z.cmp(a));
			items
				.into_iter()
				.take(20)
				.map(|(key, value)| (self_.resolve(&key), value))
				.collect::<Vec<_>>()
		};

		let longest = {
			let mut items = items
				.into_iter()
				.map(|(key, value)| (self_.resolve(&key), value))
				.collect::<Vec<_>>();
			items.sort_by(|(a, _), (z, _)| z.len().cmp(&a.len()));
			items.truncate(30);
			items
		};

		serde_json::json!({
			"most_common": most_common,
			"longest": longest,
		})
	}
}
