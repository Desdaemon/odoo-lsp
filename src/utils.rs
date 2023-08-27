use std::fmt::Display;
use std::ops::{Add, Sub};

use dashmap::DashSet;
use log::warn;
use qp_trie::wrapper::BString;
use ropey::Rope;
use tower_lsp::lsp_types::*;
use xmlparser::{StrSpan, Token};

pub mod isolate;
pub mod str;
pub use str::ImStr;

pub type PrefixTrie = qp_trie::Trie<BString, DashSet<ImStr>>;

/// A more economical version of [Location].
#[derive(Clone, Debug)]
pub struct MinLoc {
	pub path: ImStr,
	pub range: Range,
}

impl From<MinLoc> for Location {
	fn from(value: MinLoc) -> Self {
		Location {
			uri: format!("file://{}", value.path).parse().unwrap(),
			range: value.range,
		}
	}
}

pub fn offset_to_position(offset: usize, rope: Rope) -> Option<Position> {
	let line = rope.try_char_to_line(offset).ok()?;
	let first_char_of_line = rope.try_line_to_char(line).ok()?;
	let column = offset - first_char_of_line;
	Some(Position::new(line as u32, column as u32))
}

pub fn position_to_offset(position: Position, rope: Rope) -> Option<ByteOffset> {
	let CharOffset(offset) = position_to_char(position, rope.clone())?;
	let byte_offset = rope.try_char_to_byte(offset).ok()?;
	Some(ByteOffset(byte_offset))
}

pub fn position_to_char(position: Position, rope: Rope) -> Option<CharOffset> {
	let line_offset_in_char = rope.try_line_to_char(position.line as usize).ok()?;
	let line_end = line_offset_in_char + position.character as usize;
	Some(CharOffset(line_end))
}

pub fn char_to_position(offset: usize, rope: Rope) -> Option<Position> {
	let line = rope.try_char_to_line(offset).ok()?;
	let line_offset = rope.try_line_to_char(line).ok()?;
	Some(Position::new(line as u32, (offset - line_offset) as u32))
}

pub fn lsp_range_to_char_range(range: Range, rope: Rope) -> Option<std::ops::Range<CharOffset>> {
	let start = position_to_char(range.start, rope.clone())?;
	let end = position_to_char(range.end, rope.clone())?;
	Some(start..end)
}

pub fn char_range_to_lsp_range(range: std::ops::Range<CharOffset>, rope: Rope) -> Option<Range> {
	let start = char_to_position(range.start.0, rope.clone())?;
	let end = char_to_position(range.end.0, rope)?;
	Some(Range { start, end })
}

pub fn position_to_point(position: Position) -> tree_sitter::Point {
	tree_sitter::Point {
		row: position.line as usize,
		column: position.character as usize,
	}
}

pub fn offset_range_to_lsp_range(range: std::ops::Range<ByteOffset>, rope: Rope) -> Option<Range> {
	let start = offset_to_position(range.start.0, rope.clone())?;
	let end = offset_to_position(range.end.0, rope)?;
	Some(Range { start, end })
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ByteOffset(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CharOffset(pub usize);

pub trait RangeExt {
	type Unit;
	fn map_unit<F, V>(self, op: F) -> std::ops::Range<V>
	where
		F: FnMut(Self::Unit) -> V;

	fn contract(self, value: Self::Unit) -> std::ops::Range<Self::Unit>
	where
		Self: Sized,
		Self::Unit: Add<Self::Unit, Output = Self::Unit> + Sub<Self::Unit, Output = Self::Unit> + Copy;
}

impl<T> RangeExt for std::ops::Range<T> {
	type Unit = T;

	#[inline]
	fn map_unit<F, V>(self, mut op: F) -> std::ops::Range<V>
	where
		F: FnMut(Self::Unit) -> V,
	{
		op(self.start)..op(self.end)
	}

	fn contract(self, value: Self::Unit) -> std::ops::Range<Self::Unit>
	where
		Self: Sized,
		Self::Unit: Add<Self::Unit, Output = Self::Unit> + Sub<Self::Unit, Output = Self::Unit> + Copy,
	{
		self.start + value..self.end - value
	}
}

pub trait Report {
	fn report<S: Display>(self, context: impl FnOnce() -> S);
}

impl<T> Report for miette::Result<T> {
	/// Consumes self and reports the results of a computation.
	fn report<S: Display>(self, context: impl FnOnce() -> S) {
		if let Err(err) = self {
			warn!("{}:\n{err}", context());
		}
	}
}

/// [format] preceded with file location information.
/// If no arguments are passed, a string literal is returned.
#[macro_export]
macro_rules! format_loc {
	($tpl:literal) => {
		concat!("[", file!(), ":", line!(), ":", column!(), "] ", $tpl)
	};
	($tpl:literal $($tt:tt)*) => {
		format!($crate::format_loc!($tpl) $($tt)*)
	};
}
