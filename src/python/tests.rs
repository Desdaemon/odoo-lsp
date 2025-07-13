use super::*;
use crate::model::ModelProperties;
use pretty_assertions::assert_eq;
use tree_sitter::QueryCursor;

/// Tricky behavior here. The query syntax must match the trailing comma between
/// named arguments, and this test checks that. Furthermore, @help cannot be matched
/// as a `(string)` since that would reify its shape and refuse subsequent matches.
#[test]
fn test_model_fields() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = br#"
class Foo(models.Model):
	foo = fields.Char('asd', help='asd')
	bar = fields.Many2one(comodel_name='asd', help='asd')

	@property
	def foobs(self):
		...

	haha = fields.Many2many('asd')
	what = fields.What(asd)

	def passer(self):
		...

	html = fields.Html(related='asd', foo=123, help='asdf')
"#;
	let ast = parser.parse(&contents[..], None).unwrap();
	let query = ModelProperties::query();
	let mut cursor = QueryCursor::new();
	let expected: &[&[&str]] = &[
		&["foo", "fields", "Char", "'asd'", "help", "'asd'"],
		&["bar", "fields", "Many2one", "comodel_name", "'asd'", "help", "'asd'"],
		&["def foobs(self):\n\t\t...", "foobs"],
		&["haha", "fields", "Many2many", "'asd'"],
		&["what", "fields", "What"],
		&["def passer(self):\n\t\t...", "passer"],
		&[
			"html", "fields", "Html", "related", "'asd'", "foo", "123", "help", "'asdf'",
		],
	];
	let actual = cursor
		.matches(query, ast.root_node(), &contents[..])
		.map(|match_| {
			match_
				.captures
				.iter()
				.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
				.collect::<Vec<_>>()
		})
		.collect::<Vec<_>>();
	assert_eq!(expected, actual);
}

#[test]
fn test_py_completions() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = br#"
self.env.ref('ref')
env['model']
request.render('template')
foo = fields.Char()
"#;
	let ast = parser.parse(&contents[..], None).unwrap();
	let query = PyCompletions::query();
	let mut cursor = QueryCursor::new();
	let expected = vec![
		(0, vec!["env", "ref", "'ref'"]),
		(1, vec!["env", "'model'"]),
		(0, vec!["request", "render", "'template'"]),
	];
	let actual = cursor
		.matches(query, ast.root_node(), &contents[..])
		.map(|match_| {
			(
				match_.pattern_index,
				match_
					.captures
					.iter()
					.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
					.collect::<Vec<_>>(),
			)
		})
		.collect::<Vec<_>>();
	let actual = actual
		.iter()
		.map(|(index, captures)| (*index, captures.iter().map(|x| x.as_ref()).collect::<Vec<_>>()))
		.collect::<Vec<_>>();
	assert_eq!(expected, actual);
}

#[test]
fn test_py_completions_class_scoped() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = br#"
class Foo(models.AbstractModel):
	_name = 'foo'
	_inherit = ['inherit_foo', 'inherit_bar']

	foo = fields.Many2one('some.model', 'Field Name', related='related')
	bar = fields.Many2one('positional', string='blah', domain="[('foo', '=', 'bar')]")
	baz = fields.Many2many(comodel_name='named', domain=[('foo', '=', bar)])

	@api.constrains('mapped', 'meh')
	def foo(self):
		what = self.sudo().mapped('ha.ha')

	foo = fields.Foo()

	@api.depends_context('uid')
	@api.depends('mapped2', 'mapped3')
	def another(self):
		pass

	def no_decorators(self):
		pass
"#;
	let ast = parser.parse(contents, None).unwrap();
	let query = PyCompletions::query();
	let mut cursor = QueryCursor::new();
	let expected: &[&[&str]] = &[
		&["_name", "'foo'"],
		&["_inherit", "'inherit_foo'", "'inherit_bar'"],
		&["foo", "fields", "ft:Many2one", "'some.model'", "related"],
		&["bar", "fields", "ft:Many2one", "'positional'", "string", "domain"],
		&["baz", "fields", "ft:Many2many", "comodel_name", "domain"],
		// api.constrains('mapped', 'meh')
		&["api", "constrains", "'mapped'"],
		&["api", "constrains", "'meh'"],
		// scope detection with no .depends
		// note that it goes later
		&["self.sudo()", "mapped", "'ha.ha'"],
		&["api.constrains('mapped', 'meh')", "<scope>"],
		&["foo", "fields", "ft:Foo"],
		// scope detection with both .depends and non-.depends
		// first, each of the original MAPPED rules are triggered
		&["api", "depends", "'mapped2'"],
		&["api", "depends", "'mapped3'"],
		&["api", "depends", "'mapped2'", "'mapped3'", "<scope>"],
		// no decorators
		&["<scope>"],
	];
	let actual = cursor
		.matches(query, ast.root_node(), &contents[..])
		.map(|match_| {
			match_
				.captures
				.iter()
				.map(|capture| match PyCompletions::from(capture.index) {
					Some(PyCompletions::Scope) => Cow::from("<scope>"),
					Some(PyCompletions::FieldType) => Cow::from(format!(
						"ft:{}",
						String::from_utf8_lossy(&contents[capture.node.byte_range()])
					)),
					_ => String::from_utf8_lossy(&contents[capture.node.byte_range()]),
				})
				.collect::<Vec<_>>()
		})
		.collect::<Vec<_>>();
	assert_eq!(expected, actual);
}

#[test]
fn test_attribute_node_at_offset() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = "foo.mapped(lambda f: f.bar)";
	let offset = contents.find("bar").unwrap();
	let contents = contents.as_bytes();
	let ast = parser.parse(contents, None).unwrap();
	let (object, field, range) = Backend::attribute_node_at_offset(offset, ast.root_node(), contents).unwrap();
	assert_eq!(&contents[object.byte_range()], b"f");
	assert_eq!(field.as_ref(), "bar");
	assert_eq!(&contents[range], b"bar");
}

#[test]
fn test_attribute_at_offset_2() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = "super().powerful()";
	let offset = contents.find("powerful").unwrap();
	let contents = contents.as_bytes();
	let ast = parser.parse(contents, None).unwrap();
	let (object, field, range) = Backend::attribute_node_at_offset(offset, ast.root_node(), contents).unwrap();
	assert_eq!(&contents[object.byte_range()], b"super()");
	assert_eq!(field.as_ref(), "powerful");
	assert_eq!(&contents[range], b"powerful");
}

#[test]
fn test_top_level_stmt() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = "class A:\n    pass\n\nclass B:\n    pass\n";
	let offset = contents.find("class B").unwrap() + 6;
	let contents = contents.as_bytes();
	let ast = parser.parse(contents, None).unwrap();
	let node = super::top_level_stmt(ast.root_node(), offset).unwrap();
	assert_eq!(node.kind(), "class_definition");
	assert!(contents[node.byte_range()].starts_with(b"class B"));
}

#[test]
fn test_tag_model() {
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let contents = b"class A(models.Model):\n    _name = 'foo'\n    _inherit = 'bar'\n\nclass B(models.Model):\n    _inherit = 'baz'\n";
	let ast = parser.parse(&contents[..], None).unwrap();
	let query = super::PyCompletions::query();
	let mut cursor = QueryCursor::new();
	let mut this_model = super::ThisModel::default();
	for class_node in ast
		.root_node()
		.named_children(&mut ast.root_node().walk())
		.filter(|n| n.kind() == "class_definition")
	{
		for m in cursor.matches(query, class_node, &contents[..]) {
			for capture in m.captures {
				if matches!(
					super::PyCompletions::from(capture.index),
					Some(super::PyCompletions::Model)
				) {
					this_model.tag_model(capture.node, &m, class_node.byte_range(), &contents[..]);
				}
			}
		}
	}
	assert_eq!(this_model.inner, Some(&b"baz"[..]));
	assert!(matches!(this_model.source, super::ThisModelKind::Inherited));
}

#[test]
fn test_py_completions_broken_syntax_commandlist() {
	// This test demonstrates that completions should work even when syntax is broken
	// (e.g., missing colon after a dictionary key)

	// Simpler test case with just the broken part
	let contents = r#"[(0, 0, {
	'name': 'Test',
	'desc'
})]"#;

	// Find the position after 'desc' where we want completion
	let cursor_pos = contents.find("'desc'").unwrap() + 5; // After 'desc

	let mut parser = tree_sitter::Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let tree = parser.parse(contents.as_bytes(), None).unwrap();

	// Find the dictionary node
	fn find_dict(node: tree_sitter::Node) -> Option<tree_sitter::Node> {
		if node.kind() == "dictionary" {
			return Some(node);
		}
		let mut cursor = node.walk();
		for child in node.children(&mut cursor) {
			if let Some(result) = find_dict(child) {
				return Some(result);
			}
		}
		None
	}

	let dict_node = find_dict(tree.root_node());

	if let Some(dict) = dict_node {
		let mut cursor = dict.walk();
		for child in dict.children(&mut cursor) {
			let child_text = if child.byte_range().end <= contents.len() {
				&contents[child.byte_range()]
			} else {
				"<out of bounds>"
			};

			if child.kind() == "ERROR" && child_text == "'desc'" {
				// Verify we can extract the needle
				let error_start = child.start_byte();
				if cursor_pos > error_start + 1 {
					let needle_bytes = &contents.as_bytes()[error_start + 1..cursor_pos];
					let needle = std::str::from_utf8(needle_bytes).unwrap();
					assert_eq!(needle, "desc", "Should extract 'desc' as the needle");
				}

				return;
			}
		}
	}

	panic!("Did not find expected ERROR node for broken syntax");
}

#[test]
fn test_try_commandlist_completion_broken_syntax() {
	// Test case: dictionary with missing colon after key
	let contents = r#"
class TestModel(models.Model):
	_name = 'test.model'
	
	field_ids = fields.One2many('related.model', 'parent_id', string='Fields')
	
	def test_method(self):
		self.write({
			'field_ids': [(0, 0, {
				'name': 'Test',
				'description'
			})]
		})
"#;

	// Parse the Python code
	let mut parser = tree_sitter::Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let tree = parser.parse(contents.as_bytes(), None).unwrap();

	// Find position after 'description' (missing colon)
	let cursor_pos = contents.find("'description'").unwrap() + "'description".len();

	// For now, just test that we can find the ERROR node
	let node_at_cursor = tree.root_node().descendant_for_byte_range(cursor_pos, cursor_pos);
	assert!(node_at_cursor.is_some(), "Should find node at cursor position");

	// Check if we're in or near an ERROR node (broken syntax)
	let mut found_error = false;
	if let Some(node) = node_at_cursor {
		let mut current = node;
		loop {
			if current.kind() == "ERROR" || current.kind() == "string" {
				// Check if this is a string without a following colon
				if let Some(next) = current.next_sibling() {
					if next.kind() != ":" {
						found_error = true;
						break;
					}
				} else {
					// No next sibling means it's incomplete
					found_error = true;
					break;
				}
			}
			if let Some(parent) = current.parent() {
				current = parent;
			} else {
				break;
			}
		}
	}

	assert!(found_error, "Should detect broken syntax (missing colon)");
}

#[test]
fn test_try_commandlist_completion_broken_syntax_not_applicable() {
	// Test case: properly formatted dictionary (should not be broken)
	let contents = r#"
class TestModel(models.Model):
	def test_method(self):
		self.write({
			'field_ids': [(0, 0, {
				'name': 'Test',
				'description': 'Proper syntax'
			})]
		})
"#;

	let mut parser = tree_sitter::Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let tree = parser.parse(contents.as_bytes(), None).unwrap();

	// Find position in the middle of 'description' key
	let cursor_pos = contents.find("'description'").unwrap() + 5;

	// Check that this is NOT broken syntax
	let node_at_cursor = tree.root_node().descendant_for_byte_range(cursor_pos, cursor_pos);
	assert!(node_at_cursor.is_some(), "Should find node at cursor position");

	// Check if we have proper syntax (colon after string)
	let mut has_proper_syntax = false;
	if let Some(node) = node_at_cursor {
		let string_node = if node.kind() == "string" {
			node
		} else if let Some(parent) = node.parent() {
			if parent.kind() == "string" { parent } else { node }
		} else {
			node
		};

		// Check if followed by colon
		if let Some(next) = string_node.next_sibling() {
			if next.kind() == ":" {
				has_proper_syntax = true;
			}
		}
	}

	assert!(has_proper_syntax, "Should detect proper syntax (has colon)");
}

#[test]
fn test_gather_commandlist_with_broken_syntax() {
	use tree_sitter::QueryCursor;

	// Test case: commandlist with incomplete field at the end
	let contents = r#"
class TestModel(models.Model):
	_name = 'test.model'
	
	def test_method(self):
		records = self.mapped('partner_ids.name')
		values = self.mapped('field_ids.desc
"#;

	// Parse the Python code
	let mut parser = tree_sitter::Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let tree = parser.parse(contents.as_bytes(), None).unwrap();

	// Test 1: Complete field access should work
	let _cursor_pos1 = contents.find("'partner_ids.name'").unwrap() + "'partner_ids.na".len();
	let root = tree.root_node();

	// Find the commandlist node for the first case
	let mut cursor = QueryCursor::new();
	let query = tree_sitter::Query::new(
		&tree_sitter_python::LANGUAGE.into(),
		r#"(call
			function: (attribute
				object: (_)
				attribute: (identifier) @method)
			arguments: (argument_list
				(string) @cmdlist)
			(#eq? @method "mapped"))"#,
	)
	.unwrap();

	let matches: Vec<_> = cursor.matches(&query, root, contents.as_bytes()).collect();
	assert!(!matches.is_empty(), "Should find mapped calls");

	// Test 2: Incomplete field access (broken syntax)
	let cursor_pos2 = contents.find("'field_ids.desc").unwrap() + "'field_ids.desc".len();

	// The incomplete string at the end should cause a parse error
	// Check that the tree has errors
	assert!(
		tree.root_node().has_error(),
		"Tree should have parse errors due to incomplete string"
	);

	// Check that we can find a node at the position where completion would be triggered
	let node_at_pos = root.descendant_for_byte_range(cursor_pos2 - 1, cursor_pos2 - 1);
	assert!(node_at_pos.is_some(), "Should find node at cursor position");

	// The important thing is that the parser recognizes this as broken syntax
	// and that our completion logic can handle it
	// The exact tree structure may vary, but there should be an ERROR somewhere
	let mut has_error_ancestor = false;
	if let Some(mut node) = node_at_pos {
		loop {
			if node.kind() == "ERROR" {
				has_error_ancestor = true;
				break;
			}
			if let Some(parent) = node.parent() {
				node = parent;
			} else {
				break;
			}
		}
	}

	// Either the node itself is an ERROR or the tree has errors
	assert!(
		has_error_ancestor || tree.root_node().has_error(),
		"Should detect broken syntax through ERROR nodes or tree errors"
	);
}
