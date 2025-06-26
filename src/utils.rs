use core::ops::{Add, Sub};
use std::borrow::Cow;
use std::ffi::OsStr;
use std::fmt::Display;
use std::future::Future;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};

use dashmap::try_result::TryResult;
use futures::future::BoxFuture;
use ropey::{Rope, RopeSlice};
use smart_default::SmartDefault;
use tower_lsp_server::lsp_types::*;
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

/// Early return, with optional message passed to [`format_loc`](crate::format_loc!).
#[macro_export]
macro_rules! ok {
    ($res:expr $(,)?) => { ($res)? };
    ($res:expr, $($tt:tt)+) => {
		anyhow::Context::with_context($res, || format_loc!($($tt)+))?
    }
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

pub fn offset_to_position(offset: ByteOffset, rope: Rope) -> Option<Position> {
	let line = rope.try_byte_to_line(offset.0).ok()?;
	let line_start_char = rope.try_line_to_char(line).ok()?;
	let char_offset = rope.try_byte_to_char(offset.0).ok()?;
	let column = char_offset - line_start_char;
	Some(Position::new(line as u32, column as u32))
}

pub fn position_to_offset(position: Position, rope: &Rope) -> Option<ByteOffset> {
	let CharOffset(char_offset) = position_to_char(position, rope)?;
	let byte_offset = rope.try_char_to_byte(char_offset).ok()?;
	Some(ByteOffset(byte_offset))
}

pub fn position_to_offset_slice(position: Position, slice: &RopeSlice) -> Option<ByteOffset> {
	let CharOffset(char_offset) = position_to_char_slice(position, slice)?;
	let byte_offset = slice.try_char_to_byte(char_offset).ok()?;
	Some(ByteOffset(byte_offset))
}

fn position_to_char(position: Position, rope: &Rope) -> Option<CharOffset> {
	let line_offset_in_char = rope.try_line_to_char(position.line as usize).ok()?;
	Some(CharOffset(line_offset_in_char + position.character as usize))
}

fn position_to_char_slice(position: Position, rope: &RopeSlice) -> Option<CharOffset> {
	let line_offset_in_char = rope.try_line_to_char(position.line as usize).ok()?;
	Some(CharOffset(line_offset_in_char + position.character as usize))
}

pub fn lsp_range_to_char_range(range: Range, rope: &Rope) -> Option<CharRange> {
	let start = position_to_char(range.start, rope)?;
	let end = position_to_char(range.end, rope)?;
	Some(start..end)
}

pub fn lsp_range_to_offset_range(range: Range, rope: &Rope) -> Option<ByteRange> {
	let start = position_to_offset(range.start, rope)?;
	let end = position_to_offset(range.end, rope)?;
	Some(start..end)
}

pub fn offset_range_to_lsp_range(range: ByteRange, rope: Rope) -> Option<Range> {
	let start = offset_to_position(range.start, rope.clone())?;
	let end = offset_to_position(range.end, rope)?;
	Some(Range { start, end })
}

pub fn xml_position_to_lsp_position(position: TextPos) -> Position {
	Position {
		line: position.row - 1_u32,
		character: position.col - 1_u32,
	}
}

pub fn ts_range_to_lsp_range(range: tree_sitter::Range) -> Range {
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
	use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

	tracing_subscriber::registry()
		.with(tracing_subscriber::fmt::layer())
		.with(EnvFilter::from("info,odoo_lsp=trace"))
		.init();
}

pub struct CondVar {
	should_wait: AtomicBool,
	notifier: tokio::sync::Notify,
}

impl Default for CondVar {
	fn default() -> Self {
		Self {
			should_wait: AtomicBool::new(true),
			notifier: Default::default(),
		}
	}
}

pub struct Blocker<'a>(&'a CondVar);

impl CondVar {
	pub fn block(&self) -> Blocker {
		self.should_wait.store(true, Ordering::SeqCst);
		Blocker(self)
	}

	pub const WAIT_LIMIT: std::time::Duration = std::time::Duration::from_secs(15);

	/// Waits for a maximum of [`WAIT_LIMIT`][Self::WAIT_LIMIT] for a notification.
	pub async fn wait(&self) {
		if self.should_wait.load(Ordering::SeqCst) {
			tokio::select! {
				_ = self.notifier.notified() => {}
				_ = tokio::time::sleep(Self::WAIT_LIMIT) => {
					tracing::warn!("WAIT_LIMIT elapsed (thread={:?})", std::thread::current().id());
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
pub fn strict_canonicalize<P: AsRef<Path>>(path: P) -> anyhow::Result<PathBuf> {
	use anyhow::Context;

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

#[cfg(test)]
mod tests {
	use super::{to_display_path, WSL};
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
		let lhs = strict_canonicalize(Path::new(".")).unwrap();
		let rhs = strict_canonicalize(&lhs).unwrap();
		assert_eq!(lhs, rhs);
	}
}
