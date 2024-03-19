use std::io::Read;
use tree_sitter::QueryCursor;
use ts_macros::query;

query! {
	#[lang = "tree_sitter_scheme::language"]
	IndentQuery(Indent, Dedent, Newline);

[ "(" "[" ] @INDENT
[ ")" "]" ] @DEDENT
((symbol) @NEWLINE
  (#match? @NEWLINE "^@"))
}

fn trim_lines(input: &str, indent: usize) -> String {
	let Some((first, rest)) = input.split_once('\n') else {
		return input.trim_start().to_string();
	};
	let indent = " ".repeat(indent);
	let rest = rest
		.lines()
		.map(|s| {
			let trimmed = s.trim_start();
			if trimmed.is_empty() {
				String::new()
			} else {
				indent.clone() + trimmed
			}
		})
		.collect::<Vec<_>>()
		.join("\n");
	format!("{}\n{indent}{}", first.trim(), rest)
}

fn main() {
	let mut input = String::new();
	std::io::stdin().read_to_string(&mut input).unwrap();
	let mut output = String::with_capacity(input.len());

	let mut parser = tree_sitter::Parser::new();
	parser.set_language(tree_sitter_scheme::language()).unwrap();
	let tree = parser.parse(&input, None).unwrap();

	let mut cursor = QueryCursor::new();
	let mut last_offset = 0;
	let num_spaces = 2;
	let mut indent_stack = vec![0];
	let mut last_indented_line = 0;

	for (match_, _) in cursor.captures(IndentQuery::query(), tree.root_node(), input.as_bytes()) {
		for capture in match_.captures {
			match IndentQuery::from(capture.index).unwrap() {
				IndentQuery::Indent => {
					if last_indented_line != capture.node.start_position().row {
						last_indented_line = capture.node.start_position().row;
						indent_stack.push(0);
					}

					*indent_stack.last_mut().unwrap() += 1;

					let contents = &input[last_offset..capture.node.start_byte()];
					if contents.starts_with(' ') {
						output.push(' ');
					}
					output.push_str(&trim_lines(&contents, (indent_stack.len() - 1) * num_spaces));

					// balance += 1;
					output.push_str(&input[capture.node.byte_range()]);
					last_offset = capture.node.end_byte();
				}
				IndentQuery::Dedent => {
					let contents = &input[last_offset..capture.node.start_byte()];
					if contents.starts_with(' ') {
						output.push(' ');
					}
					output.push_str(&trim_lines(contents, indent_stack.len() * num_spaces));
					output.push_str(&input[capture.node.byte_range()]);

					let last = indent_stack.last_mut().unwrap();
					if *last == 1 {
						indent_stack.pop();
					} else {
						*last -= 1;
					}

					last_offset = capture.node.end_byte();
				}
				IndentQuery::Newline => {
					let contents = &input[last_offset..capture.node.start_byte()];
					output.push_str(&trim_lines(contents, indent_stack.len() * num_spaces));
					if !output.ends_with(' ') {
						output.push(' ');
					}
					output.push_str(&input[capture.node.byte_range()]);
					last_offset = capture.node.end_byte();
				}
			}
		}
	}

	println!("{output}");
}
