use core::mem::size_of;
use std::borrow::Borrow;
use std::fmt::Display;
use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use const_format::assertcp_eq;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;

/// Immutable, [`String`]-sized clone-friendly string.
#[derive(Clone)]
pub struct ImStr(Repr);

const INLINE_BYTES: usize = if cfg!(target_pointer_width = "64") { 23 } else { 11 };
#[derive(Clone)]
pub(crate) enum Repr {
	Arc(Arc<str>),
	Inline(u23, [u8; INLINE_BYTES]),
	Static(&'static str),
}

#[allow(non_camel_case_types)]
#[rustfmt::skip]
#[derive(IntoPrimitive, TryFromPrimitive, Clone, Copy)]
#[repr(u8)]
pub(crate) enum u23 {
	_0, _1, _2, _3, _4, _5, _6, _7, _8, _9,
	_10, _11, _12, _13, _14, _15, _16, _17, _18, _19,
	_20, _21, _22, _23, 
}

impl Deref for ImStr {
	type Target = str;
	#[inline]
	fn deref(&self) -> &Self::Target {
		match &self.0 {
			Repr::Arc(inner) => inner,
			Repr::Static(inner) => inner,
			Repr::Inline(len, bytes) => {
				let slice = &bytes[..(*len as usize)];
				unsafe { std::str::from_utf8_unchecked(slice) }
			}
		}
	}
}

impl Borrow<[u8]> for ImStr {
	fn borrow(&self) -> &[u8] {
		self.as_str().as_bytes()
	}
}

impl From<&str> for ImStr {
	fn from(value: &str) -> Self {
		let src = value.as_bytes();
		if src.len() <= INLINE_BYTES {
			let mut bytes = [0u8; INLINE_BYTES];
			let slice = &mut bytes[..src.len()];
			slice.copy_from_slice(src);
			Self(Repr::Inline(u23::try_from(src.len() as u8).unwrap(), bytes))
		} else {
			Self(Repr::Arc(Arc::from(value)))
		}
	}
}

impl From<String> for ImStr {
	#[inline]
	fn from(value: String) -> Self {
		value.as_str().into()
	}
}

impl AsRef<str> for ImStr {
	#[inline]
	fn as_ref(&self) -> &str {
		self.deref()
	}
}

impl Display for ImStr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		Display::fmt(self.deref(), f)
	}
}

impl std::fmt::Debug for ImStr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		std::fmt::Debug::fmt(self.deref(), f)
	}
}

impl<T> PartialEq<T> for ImStr
where
	T: AsRef<str>,
{
	#[inline]
	fn eq(&self, other: &T) -> bool {
		self.deref() == other.as_ref()
	}
}

impl<T> PartialOrd<T> for ImStr
where
	T: AsRef<str>,
{
	#[inline]
	fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
		self.deref().partial_cmp(other.as_ref())
	}
}

impl PartialEq<ImStr> for str {
	fn eq(&self, other: &ImStr) -> bool {
		self == other.deref()
	}
}

impl Eq for ImStr {}
impl Ord for ImStr {
	#[inline]
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.deref().cmp(other.deref())
	}
}

impl std::hash::Hash for ImStr {
	#[inline]
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.as_bytes().hash(state)
	}
}

impl AsRef<Path> for ImStr {
	fn as_ref(&self) -> &Path {
		Path::new(self.as_str())
	}
}

impl ImStr {
	#[inline]
	pub fn as_str(&self) -> &str {
		self.deref()
	}
	#[inline]
	pub const fn from_static(inner: &'static str) -> Self {
		Self(Repr::Static(inner))
	}
}

const _: () = {
	assertcp_eq!(
		size_of::<ImStr>(),
		size_of::<String>(),
		"ImStr must be as large as String"
	);
	assertcp_eq!(
		size_of::<Option<ImStr>>(),
		size_of::<ImStr>(),
		"An optional ImStr must be as large as ImStr"
	);
};
