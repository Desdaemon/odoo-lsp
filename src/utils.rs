use ropey::Rope;
use tower_lsp::lsp_types::*;

// fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
//     let line = rope.try_char_to_line(offset).ok()?;
//     let first_char_of_line = rope.try_line_to_char(line).ok()?;

//     let column = offset - first_char_of_line;
//     Some(Position::new(line as u32, column as u32))
// }

pub fn position_to_offset(position: Position, rope: &Rope) -> Option<ByteOffset> {
	// let line_char_start = rope.try_line_to_char(position.line as usize).ok()?;
	// let line_char_end = line_char_start + position.character as usize;
	let CharOffset(offset) = position_to_char_offset(position, rope)?;
	let byte_offset = rope.try_char_to_byte(offset).ok()?;
	Some(ByteOffset(byte_offset))
}

pub fn position_to_char_offset(position: Position, rope: &Rope) -> Option<CharOffset> {
	let line_offset_in_char = rope.try_line_to_char(position.line as usize).ok()?;
	let line_end = line_offset_in_char + position.character as usize;
	Some(CharOffset(line_end))
}

pub fn char_offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
	let line = rope.try_char_to_line(offset).ok()?;
	let line_offset = rope.try_line_to_char(line).ok()?;
	Some(Position::new(line as u32, (offset - line_offset) as u32))
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
}
