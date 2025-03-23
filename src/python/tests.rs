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
bar = fields.Many2one('positional', string='blah', domain="[('foo', '=', 'bar')]")
baz = fields.Many2many(comodel_name='named', domain=[('foo', '=', bar)])
"#;
	let ast = parser.parse(&contents[..], None).unwrap();
	let query = PyCompletions::query();
	let mut cursor = QueryCursor::new();
	let expected = vec![
		(0, vec!["env", "ref", "'ref'"]),
		(1, vec!["env", "'model'"]),
		(0, vec!["request", "render", "'template'"]),
		(4, vec!["fields", "Many2one", "'positional'", "string", "domain"]),
		(4, vec!["fields", "Many2many", "comodel_name", "domain"]),
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

	foo = fields.Char(related='related')

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
		&["foo", "fields", "related"],
		// api.constrains('mapped', 'meh')
		&["api", "constrains", "'mapped'"],
		&["api", "constrains", "'meh'"],
		// scope detection with no .depends
		// note that it goes later
		&["self.sudo()", "mapped", "'ha.ha'"],
		&["api.constrains('mapped', 'meh')", "<scope>"],
		&["foo", "fields"],
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
