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
use tower_lsp::lsp_types::*;
use xmlparser::{StrSpan, TextPos, Token};

mod visitor;
pub use visitor::PreTravel;

use crate::index::PathSymbol;

mod usage;
pub use usage::{Usage, UsageInfo};

/// Unwraps the option in the context of a function that returns [`Result<Option<_>>`].
#[macro_export]
macro_rules! some {
	($opt:expr) => {
		match $opt {
			Some(it) => it,
			None => {
				tracing::trace!("{}", $crate::format_loc!("{}", concat!(stringify!($opt), " = None")));
				return Ok(None);
			}
		}
	};
}

/// Early return, with optional message passed to [`format_loc`](crate::format_loc!).
#[macro_export]
macro_rules! ok {
    ($res:expr $(,)?) => {
        miette::IntoDiagnostic::into_diagnostic($res)?
    };
    ($res:expr, $($tt:tt)+) => {
    	miette::WrapErr::wrap_err_with(miette::IntoDiagnostic::into_diagnostic($res), || format_loc!($($tt)+))?
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
	pub async fn wait(&self) {
		if self.should_wait.load(Ordering::SeqCst) {
			self.notifier.notified().await;
		}
	}
}

impl Drop for Blocker<'_> {
	fn drop(&mut self) {
		self.0.should_wait.store(false, Ordering::SeqCst);
		self.0.notifier.notify_waiters();
	}
}

// /// Custom display trait to bypass orphan rules.
// /// Implemented on [`Option<_>`] to default to printing nothing.
pub trait DisplayExt {
	fn display(self) -> impl Display;
}

// impl<T: Display> DisplayExt for Option<T> {
// 	fn display(&self) -> impl Display {
// 		#[repr(transparent)]
// 		struct OptionDisplay<'a, T>(Option<&'a T>);
// 		impl<T: Display> Display for OptionDisplay<'_, T> {
// 			#[inline]
// 			fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
// 				match &self.0 {
// 					Some(value) => value.fmt(f),
// 					None => Ok(()),
// 				}
// 			}
// 		}
// 		OptionDisplay(self.as_ref())
// 	}
// }

// impl<T: Display> DisplayExt for Option<&T> {
// 	fn display(self) -> impl Display {
// 		match self {
// 			Some(value) => value as &dyn Display,
// 			None => &"" as &dyn Display,
// 		}
// 	}
// }
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
