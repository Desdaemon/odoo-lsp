use std::fmt::Display;
use std::ops::Deref;
use std::sync::Arc;

/// Immutable, [String]-sized clone-friendly string.
#[derive(Clone)]
pub struct ImStr(Repr);

const INLINE_BYTES: usize = 22;
#[derive(Clone)]
enum Repr {
	Arc(Arc<str>),
	Inline(u8, [u8; INLINE_BYTES]),
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

impl From<&str> for ImStr {
	fn from(value: &str) -> Self {
		let src = value.as_bytes();
		if src.len() <= INLINE_BYTES {
			let mut bytes = [0u8; INLINE_BYTES];
			let slice = &mut bytes[..src.len()];
			slice.copy_from_slice(src);
			Self(Repr::Inline(src.len() as u8, bytes))
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

impl std::borrow::Borrow<str> for ImStr {
	#[inline]
	fn borrow(&self) -> &str {
		self.deref()
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
		self.deref().fmt(f)
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

impl ImStr {
	#[inline]
	pub fn as_str(&self) -> &str {
		self.deref()
	}
}
