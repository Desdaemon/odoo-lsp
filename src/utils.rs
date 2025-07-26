use core::ops::{Add, Sub};
use std::borrow::Cow;
use std::ffi::OsStr;
use std::fmt::Display;
use std::future::Future;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};

use dashmap::try_result::TryResult;
use futures::future::BoxFuture;
use ropey::RopeSlice;
use smart_default::SmartDefault;
use tower_lsp_server::lsp_types::*;
use tracing::warn;
use xmlparser::{StrSpan, TextPos, Token};

mod visitor;
pub use visitor::PreTravel;

mod catch_panic;
pub use catch_panic::CatchPanic;

use crate::index::PathSymbol;

#[cfg(not(windows))]
pub use std::fs::canonicalize as strict_canonicalize;

/// Unwraps the option in the context of a function that returns [`Result<Option<_>>`].
#[macro_export]
macro_rules! some {
	($opt:expr) => {
		match $opt {
			Some(it) => it,
			None => {
				tracing::trace!(concat!(stringify!($opt), " = None"));
				return Ok(None);
			}
		}
	};
}

#[macro_export]
macro_rules! dig {
	() => { None };
	($start:expr, $($rest:tt)+) => {
		dig!(@inner Some($start), $($rest)+)
	};
	(@inner $node:expr, $kind:ident($idx:literal).$($rest:tt)+) => {
		dig!(
			@inner
			if let Some(node) = $node && let Some(child) = node.named_child($idx) && child.kind() == stringify!($kind) { Some(child) } else { None },
			$($rest)+
		)
	};

	(@inner $node:expr, $kind:ident.$($rest:tt)+) => {
		dig!(@inner $node, $kind(0).$($rest)+)
	};
	(@inner $node:expr, $kind:ident($idx:literal)) => {
		if let Some(node) = $node && let Some(child) = node.named_child($idx) && child.kind() == stringify!($kind) { Some(child) } else { None }
	};
	(@inner $node:expr, $kind:ident) => {
		dig!(@inner $node, $kind(0))
	};
}

/// Early return, with optional message passed to [`format_loc`](crate::format_loc!).
#[macro_export]
macro_rules! ok {
    ($res:expr $(,)?) => {
    	anyhow::Context::context($res, concat!($crate::loc!(), " ", stringify!($res)))?
    };
    ($res:expr, $($tt:tt)+) => {
		anyhow::Context::with_context($res, || $crate::format_loc!($($tt)+))?
    }
}

#[macro_export]
macro_rules! await_did_open_document {
	($self:expr, $path:expr) => {
		let mut blocker = None;
		{
			if let Some(document) = $self.document_map.get($path)
				&& document.setup.should_wait()
			{
				blocker = Some(document.setup.clone());
			}
		}
		if let Some(blocker) = blocker {
			blocker.wait().await;
		}
	};
}

#[repr(transparent)]
#[derive(SmartDefault)]
pub struct EarlyReturn<'a, T>(
	// By default, trait objects are bound to a 'static lifetime.
	// This allows closures to capture references instead.
	// However, any values must still be Send.
	#[default(None)] Option<Box<dyn FnOnce() -> BoxFuture<'a, T> + 'a + Send>>,
);

impl<'a, T> EarlyReturn<'a, T> {
	/// Lifts a certain async computation out of the current scope to be executed later.
	pub fn lift<F, Fut>(&mut self, closure: F)
	where
		F: FnOnce() -> Fut + 'a + Send,
		Fut: Future<Output = T> + 'a + Send,
	{
		self.0 = Some(Box::new(move || Box::pin(async move { closure().await })));
	}
	#[inline]
	pub fn is_none(&self) -> bool {
		self.0.is_none()
	}
	pub fn call(self) -> Option<BoxFuture<'a, T>> {
		Some(self.0?())
	}
}

/// A more economical version of [Location].
#[derive(Clone, Debug)]
pub struct MinLoc {
	pub path: PathSymbol,
	pub range: Range,
}

impl Display for MinLoc {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!(
			"{}:{}:{}",
			self.path,
			self.range.start.line + 1,
			self.range.start.character + 1
		))
	}
}

impl From<MinLoc> for Location {
	fn from(value: MinLoc) -> Self {
		Location {
			uri: format!("file://{}", value.path).parse().unwrap(),
			range: value.range,
		}
	}
}

pub struct SpanAdapter<T>(T);
pub struct RopeAdapter<'a, T>(T, RopeSlice<'a>);

/// Infallible version of [rope_conv] that doesn't require a [Rope].
/// Available conversions:
/// - [xmlparser::TextPos] -> [Position]
/// - [tree_sitter::Range] -> [Range]
#[inline]
pub fn span_conv<T, U>(src: T) -> U
where
	U: From<SpanAdapter<T>>,
{
	SpanAdapter(src).into()
}

/// Specializations for conversion between several types of offsets and ranges.  
/// Available conversions:
/// - [ByteOffset] <-> [Position]
/// - [Range] -> [CharRange]
/// - [Range] <-> [ByteRange]
#[inline]
pub fn rope_conv<T, U>(src: T, rope: RopeSlice<'_>) -> Result<U, <U as TryFrom<RopeAdapter<'_, T>>>::Error>
where
	for<'a> U: TryFrom<RopeAdapter<'a, T>>,
{
	RopeAdapter(src, rope).try_into()
}

impl<'a> TryFrom<RopeAdapter<'a, ByteOffset>> for Position {
	type Error = ropey::Error;
	fn try_from(value: RopeAdapter<'a, ByteOffset>) -> Result<Self, Self::Error> {
		let RopeAdapter(offset, rope) = value;
		let line = rope.try_byte_to_line(offset.0)?;
		let line_start_char = rope.try_line_to_char(line)?;
		let char_offset = rope.try_byte_to_char(offset.0)?;
		let column = char_offset - line_start_char;
		Ok(Position::new(line as u32, column as u32))
	}
}

impl<'a> TryFrom<RopeAdapter<'a, Position>> for ByteOffset {
	type Error = ropey::Error;
	fn try_from(value: RopeAdapter<'a, Position>) -> Result<Self, Self::Error> {
		let RopeAdapter(position, rope) = value;
		let CharOffset(char_offset) = position_to_char(position, rope)?;
		let byte_offset = rope.try_char_to_byte(char_offset)?;
		Ok(ByteOffset(byte_offset))
	}
}

impl<'a> TryFrom<RopeAdapter<'a, Range>> for CharRange {
	type Error = ropey::Error;
	fn try_from(value: RopeAdapter<'a, Range>) -> Result<Self, Self::Error> {
		let RopeAdapter(range, rope) = value;
		let start = position_to_char(range.start, rope)?;
		let end = position_to_char(range.end, rope)?;
		Ok(start..end)
	}
}
impl<'a> TryFrom<RopeAdapter<'a, Range>> for ByteRange {
	type Error = ropey::Error;
	fn try_from(value: RopeAdapter<'a, Range>) -> Result<Self, Self::Error> {
		let RopeAdapter(range, rope) = value;
		let start = rope_conv(range.start, rope)?;
		let end = rope_conv(range.end, rope)?;
		Ok(start..end)
	}
}

impl<'a> TryFrom<RopeAdapter<'a, ByteRange>> for Range {
	type Error = ropey::Error;
	fn try_from(value: RopeAdapter<'a, ByteRange>) -> Result<Self, Self::Error> {
		let RopeAdapter(range, rope) = value;
		let start = rope_conv(range.start, rope)?;
		let end = rope_conv(range.end, rope)?;
		Ok(Range { start, end })
	}
}

fn position_to_char(position: Position, rope: RopeSlice<'_>) -> ropey::Result<CharOffset> {
	let line_offset_in_char = rope.try_line_to_char(position.line as usize)?;
	Ok(CharOffset(line_offset_in_char + position.character as usize))
}

impl From<SpanAdapter<TextPos>> for Position {
	#[inline]
	fn from(value: SpanAdapter<TextPos>) -> Self {
		let SpanAdapter(position) = value;
		Position {
			line: position.row - 1_u32,
			character: position.col - 1_u32,
		}
	}
}

impl From<SpanAdapter<tree_sitter::Range>> for Range {
	#[inline]
	fn from(value: SpanAdapter<tree_sitter::Range>) -> Self {
		let SpanAdapter(range) = value;
		Range {
			start: Position {
				line: range.start_point.row as u32,
				character: range.start_point.column as u32,
			},
			end: Position {
				line: range.end_point.row as u32,
				character: range.end_point.column as u32,
			},
		}
	}
}

pub fn token_span<'r, 't>(token: &'r Token<'t>) -> &'r StrSpan<'t> {
	match token {
		Token::Declaration { span, .. }
		| Token::ProcessingInstruction { span, .. }
		| Token::Comment { span, .. }
		| Token::DtdStart { span, .. }
		| Token::EmptyDtd { span, .. }
		| Token::EntityDeclaration { span, .. }
		| Token::DtdEnd { span, .. }
		| Token::ElementStart { span, .. }
		| Token::Attribute { span, .. }
		| Token::ElementEnd { span, .. }
		| Token::Text { text: span, .. }
		| Token::Cdata { span, .. } => span,
	}
}

/// Similar to [`str::split_once`]
///
/// Returns `src` if the string cannot be split by `sep`.
pub fn cow_split_once<'src>(
	mut src: Cow<'src, str>,
	sep: &str,
) -> Result<(Cow<'src, str>, Cow<'src, str>), Cow<'src, str>> {
	match src {
		Cow::Borrowed(inner) => inner
			.split_once(sep)
			.map(|(lhs, rhs)| (Cow::Borrowed(lhs), Cow::Borrowed(rhs)))
			.ok_or(src),
		Cow::Owned(ref mut inner) => {
			let Some(offset) = inner.find(sep) else {
				return Err(src);
			};
			let mut rhs = inner.split_off(offset);
			rhs.replace_range(0..sep.len(), "");
			Ok((Cow::Owned(core::mem::take(inner)), Cow::Owned(rhs)))
		}
	}
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ByteOffset(pub usize);
pub type ByteRange = core::ops::Range<ByteOffset>;

impl From<usize> for ByteOffset {
	#[inline]
	fn from(value: usize) -> Self {
		ByteOffset(value as _)
	}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CharOffset(pub usize);
pub type CharRange = core::ops::Range<CharOffset>;

pub trait RangeExt {
	type Unit;
	fn map_unit<F, V>(self, op: F) -> std::ops::Range<V>
	where
		F: FnMut(Self::Unit) -> V;

	fn shrink(self, value: Self::Unit) -> std::ops::Range<Self::Unit>
	where
		Self: Sized,
		Self::Unit: Add<Self::Unit, Output = Self::Unit> + Sub<Self::Unit, Output = Self::Unit> + Copy;

	fn contains_end(&self, value: Self::Unit) -> bool
	where
		Self::Unit: PartialOrd;
}

pub trait Erase {
	fn erase(&self) -> core::ops::Range<usize>;
	fn intersects(&self, other: core::ops::Range<usize>) -> bool {
		let this = self.erase();
		this.end >= other.start || this.start < other.end
	}
}

impl Erase for ByteRange {
	#[inline]
	fn erase(&self) -> core::ops::Range<usize> {
		self.clone().map_unit(|unit| unit.0)
	}
}

impl Erase for CharRange {
	#[inline]
	fn erase(&self) -> core::ops::Range<usize> {
		self.clone().map_unit(|unit| unit.0)
	}
}

impl<T> RangeExt for core::ops::Range<T> {
	type Unit = T;

	#[inline]
	fn map_unit<F, V>(self, mut op: F) -> core::ops::Range<V>
	where
		F: FnMut(Self::Unit) -> V,
	{
		op(self.start)..op(self.end)
	}

	fn shrink(self, value: Self::Unit) -> core::ops::Range<Self::Unit>
	where
		Self: Sized,
		Self::Unit: Add<Self::Unit, Output = Self::Unit> + Sub<Self::Unit, Output = Self::Unit> + Copy,
	{
		self.start + value..self.end - value
	}

	#[inline]
	fn contains_end(&self, value: Self::Unit) -> bool
	where
		Self::Unit: PartialOrd,
	{
		self.contains(&value) || self.end == value
	}
}

#[macro_export]
macro_rules! loc {
	() => {
		concat!("[", file!(), ":", line!(), ":", column!(), "]")
	};
}

#[macro_export]
macro_rules! errloc {
	($msg:literal $(, $($tt:tt)* )?) => {
		::anyhow::anyhow!(concat!($crate::loc!(), " ", $msg) $(, $($tt)* )?)
	}
}

/// [format] preceded with file location information.
/// If no arguments are passed, a string literal is returned.
#[macro_export]
macro_rules! format_loc {
	($tpl:literal) => {
		concat!($crate::loc!(), " ", $tpl)
	};
	($tpl:literal $($tt:tt)*) => {
		format!($crate::format_loc!($tpl) $($tt)*)
	};
}

#[derive(Default)]
pub struct MaxVec<T>(Vec<T>);

impl<T> MaxVec<T> {
	pub fn new(limit: usize) -> Self {
		MaxVec(Vec::with_capacity(limit))
	}
	#[inline]
	fn remaining_space(&self) -> usize {
		self.0.capacity().saturating_sub(self.0.len())
	}
	#[inline]
	pub fn has_space(&self) -> bool {
		self.remaining_space() > 0
	}
	pub fn extend(&mut self, items: impl Iterator<Item = T>) {
		self.0.extend(items.take(self.remaining_space()));
	}
	pub fn push_checked(&mut self, item: T) {
		if self.has_space() {
			self.0.push(item);
		}
	}
	#[inline]
	pub fn into_inner(self) -> Vec<T> {
		self.0
	}
}

impl<T> std::convert::AsMut<[T]> for MaxVec<T> {
	#[inline]
	fn as_mut(&mut self) -> &mut [T] {
		&mut self.0
	}
}

impl<T> std::ops::Deref for MaxVec<T> {
	type Target = Vec<T>;
	#[inline]
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

pub trait TryResultExt {
	type Result: Sized;
	/// Panics if this is [`TryResult::Locked`].
	fn expect(self, msg: &str) -> Option<Self::Result>;
}

impl<T: Sized> TryResultExt for TryResult<T> {
	type Result = T;
	fn expect(self, msg: &str) -> Option<Self::Result> {
		match self {
			TryResult::Present(item) => Some(item),
			TryResult::Absent => None,
			TryResult::Locked => panic!("{msg}"),
		}
	}
}

#[cfg(test)]
pub fn init_for_test() {
	use std::sync::Once;
	use tracing_subscriber::{EnvFilter, layer::SubscriberExt, util::SubscriberInitExt};

	static INIT: Once = Once::new();
	INIT.call_once(|| {
		tracing_subscriber::registry()
			.with(tracing_subscriber::fmt::layer())
			.with(EnvFilter::from("info,odoo_lsp=trace"))
			.init();
	});
}

pub struct Semaphore {
	should_wait: AtomicBool,
	notifier: tokio::sync::Notify,
}

impl Default for Semaphore {
	fn default() -> Self {
		Self {
			should_wait: AtomicBool::new(true),
			notifier: Default::default(),
		}
	}
}

pub struct Blocker<'a>(&'a Semaphore);

impl Semaphore {
	pub fn block(&self) -> Blocker<'_> {
		self.should_wait.store(true, Ordering::SeqCst);
		Blocker(self)
	}

	pub const WAIT_LIMIT: std::time::Duration = std::time::Duration::from_secs(10);

	/// Waits for a maximum of [`WAIT_LIMIT`][Self::WAIT_LIMIT] for a notification.
	pub async fn wait(&self) {
		if self.should_wait.load(Ordering::SeqCst) {
			tokio::select! {
				_ = self.notifier.notified() => {}
				_ = tokio::time::sleep(Self::WAIT_LIMIT) => {
					warn!("WAIT_LIMIT elapsed (thread={:?})", std::thread::current().id());
				}
			}
		}
	}

	#[inline]
	pub fn should_wait(&self) -> bool {
		self.should_wait.load(Ordering::SeqCst)
	}
}

impl Drop for Blocker<'_> {
	fn drop(&mut self) {
		self.0.should_wait.store(false, Ordering::SeqCst);
		self.0.notifier.notify_waiters();
	}
}

/// Custom display trait to bypass orphan rules.
/// Implemented on [`Option<_>`] to default to printing nothing.
pub trait DisplayExt {
	fn display(self) -> impl Display;
}

impl<T: Display> DisplayExt for Option<T> {
	fn display(self) -> impl Display {
		struct Adapter<T>(Option<T>);
		impl<T: Display> Display for Adapter<T> {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				match &self.0 {
					Some(value) => value.fmt(f),
					None => Ok(()),
				}
			}
		}
		Adapter(self)
	}
}

impl<T: Display> DisplayExt for &T {
	fn display(self) -> impl Display {
		self as &dyn Display
	}
}

impl DisplayExt for std::fmt::Arguments<'_> {
	fn display(self) -> impl Display {
		#[repr(transparent)]
		struct Adapter<'a>(std::fmt::Arguments<'a>);
		impl Display for Adapter<'_> {
			#[inline]
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				f.write_fmt(self.0)
			}
		}
		Adapter(self)
	}
}

pub fn path_contains(path: impl AsRef<Path>, needle: impl AsRef<OsStr>) -> bool {
	path.as_ref().components().any(|c| c.as_os_str() == needle.as_ref())
}

static WSL: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
	#[cfg(not(unix))]
	return false;

	#[cfg(unix)]
	return rustix::system::uname()
		.release()
		.to_str()
		.is_ok_and(|release| release.contains("WSL"));
});

#[inline]
fn wsl_to_windows_path(path: impl AsRef<OsStr>) -> Option<String> {
	fn impl_(path: &OsStr) -> Result<String, String> {
		let mut out = std::process::Command::new("wslpath")
			.arg("-w")
			.arg(path)
			.output()
			.map_err(|err| format_loc!("wslpath failed: {}", err))?;
		let code = out.status.code().unwrap_or(-1);
		if code != 0 {
			return Err(format_loc!("wslpath failed with code={}", code));
		}

		Ok(String::from_utf8(core::mem::take(&mut out.stdout))
			.map_err(|err| format_loc!("wslpath returned non-utf8 path: {}", err))?
			.trim()
			.to_string())
	}
	impl_(path.as_ref()).map_err(|err| tracing::error!("{err}")).ok()
}

/// Returns a path suitable for display on code editors e.g. VSCode.
///
/// Transforms the path on WSL only.
pub fn to_display_path(path: impl AsRef<Path>) -> String {
	if *WSL {
		return wsl_to_windows_path(path.as_ref()).unwrap_or_else(|| path.as_ref().to_string_lossy().into_owned());
	}

	path.as_ref().to_string_lossy().into_owned()
}

/// On Windows, rewrites the wide path prefix `\\?\C:` to `C:`  
/// Source: https://stackoverflow.com/a/70970317
#[inline]
#[cfg(windows)]
pub fn strict_canonicalize<P: AsRef<Path>>(path: P) -> anyhow::Result<std::path::PathBuf> {
	use anyhow::Context;
	use std::path::PathBuf;

	fn impl_(path: PathBuf) -> anyhow::Result<PathBuf> {
		let head = path.components().next().context("empty path")?;
		let disk_;
		let head = if let std::path::Component::Prefix(prefix) = head {
			if let std::path::Prefix::VerbatimDisk(disk) = prefix.kind() {
				disk_ = format!("{}:", disk as char);
				Path::new(&disk_)
					.components()
					.next()
					.context("failed to parse disk component")?
			} else {
				head
			}
		} else {
			head
		};
		Ok(std::iter::once(head).chain(path.components().skip(1)).collect())
	}
	let canon = std::fs::canonicalize(path)?;
	impl_(canon)
}

/// Replacement for `collect` since tree-sitter's StreamingIterator cannot be collected
pub fn acc_vec<T>(mut acc: Vec<T>, item: &mut T) -> Vec<T>
where
	T: Default,
{
	acc.push(core::mem::take(item));
	acc
}

#[cfg(test)]
mod tests {
	use super::{WSL, to_display_path};
	use pretty_assertions::assert_eq;

	#[test]
	fn test_to_display_path() {
		if *WSL {
			assert_eq!(to_display_path("/mnt/c"), r"C:\");
			let unix_path = to_display_path("/usr");
			assert!(unix_path.starts_with(r"\\wsl"));
			assert!(unix_path.ends_with(r"\usr"));
		}
	}

	#[test]
	#[cfg(windows)]
	fn test_idempotent_canonicalization() {
		use super::strict_canonicalize;
		use std::path::Path;

		let lhs = strict_canonicalize(Path::new(".")).unwrap();
		let rhs = strict_canonicalize(&lhs).unwrap();
		assert_eq!(lhs, rhs);
	}
}
