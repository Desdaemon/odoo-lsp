use std::iter::FusedIterator;

use tree_sitter::Node;

pub struct PreTravel<'a> {
	depth: u32,
	cursor: Option<tree_sitter::TreeCursor<'a>>,
}

impl<'node> PreTravel<'node> {
	pub fn new(node: Node<'node>) -> Self {
		Self {
			depth: 0,
			cursor: Some(node.walk()),
		}
	}
}

impl FusedIterator for PreTravel<'_> {}
impl<'a> Iterator for PreTravel<'a> {
	type Item = Node<'a>;
	fn next(&mut self) -> Option<Self::Item> {
		// copied from tree-sitter-traversal
		let cursor = self.cursor.as_mut()?;

		let node = cursor.node();
		if cursor.goto_first_child() {
			self.depth += 1;
			return Some(node);
		} else if cursor.goto_next_sibling() {
			return Some(node);
		}

		loop {
			if self.depth == 0 {
				self.cursor = None;
				break;
			}
			assert!(cursor.goto_parent());
			self.depth -= 1;
			if cursor.goto_next_sibling() {
				break;
			}
		}

		Some(node)
	}
}
