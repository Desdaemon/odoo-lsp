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

#[cfg(test)]
mod tests {
	use super::*;
	use pretty_assertions::assert_eq;
	use tree_sitter::Parser;

	#[test]
	fn test_simple_call() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = b"foo.mapped(lambda f: f.bar)";
		let asts = PreTravel::new(parser.parse(contents, None).unwrap().root_node())
			.flat_map(|node| {
				node.is_named().then(|| {
					(node.kind(), unsafe {
						core::str::from_utf8_unchecked(&contents[node.byte_range()])
					})
				})
			})
			.collect::<Vec<_>>();
		assert_eq!(
			asts,
			[
				("module", "foo.mapped(lambda f: f.bar)"),
				("expression_statement", "foo.mapped(lambda f: f.bar)"),
				("call", "foo.mapped(lambda f: f.bar)"),
				("attribute", "foo.mapped"),
				("identifier", "foo"),
				("identifier", "mapped"),
				("argument_list", "(lambda f: f.bar)"),
				("lambda", "lambda f: f.bar"),
				("lambda_parameters", "f"),
				("identifier", "f"),
				("attribute", "f.bar"),
				("identifier", "f"),
				("identifier", "bar")
			]
		);
	}
}
