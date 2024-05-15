use core::mem::size_of;
use std::borrow::{Borrow, Cow};
use std::fmt::{Debug, Display};
use std::io::{Read, Write};
use std::ops::Deref;
use std::path::Path;
use std::sync::Arc;

use libflate::deflate::{Decoder, Encoder};
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;

use crate::utils::{Usage, UsageInfo};

/// Immutable, [String]-sized clone-friendly string.
#[derive(Clone)]
pub struct ImStr(pub(crate) Repr);

const INLINE_BYTES: usize = 23;
#[derive(Clone)]
pub(crate) enum Repr {
	Arc(Arc<str>),
	Inline(u23, [u8; INLINE_BYTES]),
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
		self.deref().hash(state)
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
}

/// [String]-sized clone-friendly container optimized for freeform text.
#[derive(Clone)]
pub struct Text(TextRepr);

#[derive(Clone)]
enum TextRepr {
	Inline(u23, [u8; INLINE_BYTES]),
	Arc(Arc<str>),
	Compressed(Arc<[u8]>),
}

const _: () = {
	assert!(size_of::<ImStr>() == size_of::<String>());
	assert!(size_of::<Text>() == size_of::<String>());
};

impl Debug for Text {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match &self.0 {
			TextRepr::Inline(..) | TextRepr::Arc(..) => Debug::fmt(&self.to_string(), f),
			TextRepr::Compressed(..) => f.debug_tuple("Text").field(&"<compressed>").finish(),
		}
	}
}

impl TryFrom<&str> for Text {
	type Error = std::io::Error;
	fn try_from(value: &str) -> Result<Self, Self::Error> {
		let bytes = value.as_bytes();
		if bytes.len() <= INLINE_BYTES {
			let mut buf = [0u8; INLINE_BYTES];
			let slice = &mut buf[..bytes.len()];
			slice.copy_from_slice(bytes);
			return Ok(Text(TextRepr::Inline(u23::try_from(bytes.len() as u8).unwrap(), buf)));
		}
		// TODO: Basic heuristics for determining cost/benefit ratio of compression
		let mut enc = Encoder::new(Vec::new());
		enc.write_all(bytes)?;
		let data = enc.finish().into_result()?;
		if data.len() >= bytes.len() {
			Ok(Self(TextRepr::Arc(value.into())))
		} else {
			Ok(Self(TextRepr::Compressed(data.into())))
		}
	}
}

impl Text {
	pub fn to_string(&self) -> Cow<str> {
		match &self.0 {
			TextRepr::Inline(len, bytes) => {
				let slice = &bytes[..(*len as usize)];
				let slice = unsafe { std::str::from_utf8_unchecked(slice) };
				Cow::Borrowed(slice)
			}
			TextRepr::Arc(ptr) => Cow::Borrowed(ptr),
			TextRepr::Compressed(encoded) => {
				let mut buf = vec![];
				let mut dec = Decoder::new(&encoded[..]);
				dec.read_to_end(&mut buf).expect("decoding error");
				// SAFETY: As long as the decoder returns valid UTF-8.
				let str = unsafe { String::from_utf8_unchecked(buf) };
				Cow::Owned(str)
			}
		}
	}
}

impl Usage for Text {
	fn usage(&self) -> UsageInfo<Self> {
		match &self.0 {
			TextRepr::Inline(_, _) => UsageInfo::new(0),
			TextRepr::Arc(ptr) => UsageInfo::new(ptr.len()),
			TextRepr::Compressed(ptr) => UsageInfo::new(ptr.len()),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::Text;

	#[test]
	fn test_sanity_check() {
		const TEXTS: &[&str] = &[
			"Short text",
			"Short text goes over 22",
			"Hey there, you, you're awake. You were trying to cross the border?",
			"The quick brown fox jumps over the lazy dog. Five plus five equals ten, a monad is a monoid in the category of endofunctors."
		];
		for text in TEXTS {
			let enc = Text::try_from(*text).unwrap();
			let dec = enc.to_string();
			assert_eq!(*text, dec);
		}
	}
}
