//! Methods related to type analysis. The two most important methods are
//! [`Backend::model_of_range`] and [`Backend::type_of`].

use std::{borrow::Borrow, collections::HashMap, iter::FusedIterator, ops::ControlFlow};

use log::trace;
use tree_sitter::{Node, QueryCursor};

use odoo_lsp::{
	index::interner,
	model::ModelName,
	utils::{ByteRange, Erase, PreTravel, RangeExt},
	ImStr,
};
use ts_macros::query;

use crate::Backend;

static MODEL_METHODS: phf::Set<&[u8]> = phf::phf_set!(
	b"create",
	b"copy",
	b"name_create",
	b"browse",
	b"filtered",
	b"filtered_domain",
	b"sorted",
	b"search",
	b"name_search",
	b"ensure_one",
	b"with_context",
	b"with_user",
	b"with_company",
	b"with_env",
	b"sudo",
	b"_for_xml_id",
	// TODO: Limit to Forms only
	b"new",
);

/// The subset of types that may resolve to a model.
#[derive(Clone)]
pub enum Type {
	Env,
	/// \*.env.ref()
	RefFn,
	/// Functions that return another model, regardless of input.
	ModelFn(ImStr),
	Model(ImStr),
	/// Unresolved model.
	Record(ImStr),
	Super,
	Method(ModelName, &'static str),
	/// Can never be resolved, useful for non-model bindings.
	Value,
}

/// The current environment, populated from the AST statement by statement.
#[derive(Default, Clone)]
pub struct Scope {
	pub variables: HashMap<String, Type>,
	pub parent: Option<Box<Scope>>,
	/// TODO: Allow super(_, \<self>)
	pub super_: Option<ImStr>,
}

pub fn normalize<'r, 'n>(node: &'r mut Node<'n>) -> &'r mut Node<'n> {
	let mut cursor = node.walk();
	while matches!(
		node.kind(),
		"expression_statement" | "parenthesized_expression" | "module"
	) {
		let Some(child) = node.named_children(&mut cursor).find(|child| child.kind() != "comment") else {
			break;
		};
		*node = child;
	}
	node
}

impl Scope {
	fn new(parent: Option<Scope>) -> Self {
		Self {
			parent: parent.map(Box::new),
			..Default::default()
		}
	}
	pub fn get(&self, key: impl Borrow<str>) -> Option<&Type> {
		fn impl_<'me>(self_: &'me Scope, key: &str) -> Option<&'me Type> {
			self_
				.variables
				.get(key)
				.or_else(|| self_.parent.as_ref().and_then(|parent| parent.get(key)))
		}
		impl_(self, key.borrow())
	}
	pub fn insert(&mut self, key: String, value: Type) {
		self.variables.insert(key, value);
	}
	pub fn enter(&mut self) {
		*self = Scope::new(Some(core::mem::take(self)));
	}
	pub fn exit(&mut self) {
		if let Some(parent) = self.parent.take() {
			*self = *parent;
		}
	}
	/// Iterates over the current scope and its parents.
	/// If the same variable is defined multiple times in the tree,
	/// each definition is returned from innermost to outermost.
	pub fn iter(&self) -> impl Iterator<Item = (&str, &Type)> {
		Iter {
			variables: self.variables.iter(),
			parent: self.parent.as_deref(),
		}
	}
}

struct Iter<'a> {
	variables: std::collections::hash_map::Iter<'a, String, Type>,
	parent: Option<&'a Scope>,
}

impl FusedIterator for Iter<'_> {}

impl<'a> Iterator for Iter<'a> {
	type Item = (&'a str, &'a Type);
	fn next(&mut self) -> Option<Self::Item> {
		if let Some((key, value)) = self.variables.next() {
			return Some((key.as_str(), value));
		}

		let parent = self.parent.take()?;
		self.parent = parent.parent.as_deref();
		self.variables = parent.variables.iter();
		let (key, value) = self.variables.next()?;
		Some((key.as_str(), value))
	}
}

#[rustfmt::skip]
query! {
	#[derive(Debug)]
	FieldCompletion(Name, SelfParam, Scope);
((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @_name (string) @NAME))?
    [
      (decorated_definition
        (function_definition
          (parameters . (identifier) @SELF_PARAM) (block) @SCOPE) .)
      (function_definition (parameters . (identifier) @SELF_PARAM) (block) @SCOPE) ])) @class
  (#match? @_name "^_(name|inherit)$"))
}

#[rustfmt::skip]
query! {
	MappedCall(Callee, Iter);
((call
  (attribute (_) @CALLEE (identifier) @_mapped)
  (argument_list [
    (lambda (lambda_parameters . (identifier) @ITER))
    (keyword_argument
      (identifier) @_func
      (lambda (lambda_parameters . (identifier) @ITER)))]))
  (#match? @_func "^(func|key)$")
  (#match? @_mapped "^(mapp|filter|sort)ed$"))
}

pub type ScopeControlFlow = ControlFlow<Option<Scope>, bool>;
impl Backend {
	pub fn model_of_range(&self, node: Node<'_>, range: ByteRange, contents: &[u8]) -> Option<ModelName> {
		trace!("model_of_range {}", String::from_utf8_lossy(&contents[range.erase()]));
		// Phase 1: Determine the scope.
		let (self_type, fn_scope, self_param) = determine_scope(node, contents, range.start.0)?;

		// Phase 2: Build the scope up to offset
		// What contributes to a method scope's variables?
		// 1. Top-level statements; completely opaque to us.
		// 2. Class definitions; technically useless to us.
		//    Self-type analysis only uses a small part of the class definition.
		// 3. Parameters, e.g. self which always has a fixed type
		// 4. Assignments (including walrus-assignment)
		let mut scope = Scope::default();
		let self_type = match self_type {
			Some(type_) => &contents[type_.byte_range().shrink(1)],
			None => &[],
		};
		scope.super_ = Some(self_param.as_ref().into());
		scope.insert(
			self_param.into_owned(),
			Type::Model(String::from_utf8_lossy(self_type).as_ref().into()),
		);
		let scope = self.walk_scope(fn_scope, Some(scope), |scope, node| {
			self.build_scope(scope, node, range.end.0, contents)
		})?;

		let node_at_cursor = fn_scope.descendant_for_byte_range(range.start.0, range.end.0)?;
		let type_at_cursor = self.type_of(node_at_cursor, &scope, contents)?;
		self.resolve_type(&type_at_cursor, &scope)
	}
	/// Builds the scope up to `offset`.
	///
	/// ###### About [ScopeControlFlow]
	/// This is one of the rare occasions where [ControlFlow] is used. It is similar to
	/// [Result] in that the try-operator (?) can be used to end iteration on a
	/// [ControlFlow::Break]. Otherwise, [ControlFlow::Continue] has a continuation value
	/// that must be passed up the chain, since it indicates whether [Scope::enter] was called.
	pub fn build_scope(&self, scope: &mut Scope, node: Node, offset: usize, contents: &[u8]) -> ScopeControlFlow {
		if node.start_byte() > offset {
			return ControlFlow::Break(Some(core::mem::take(scope)));
		}
		match node.kind() {
			"assignment" | "named_expression" => {
				// (_ left right)
				let lhs = node.named_child(0).unwrap();
				if lhs.kind() == "identifier" {
					let rhs = lhs.next_named_sibling().unwrap();
					if let Some(type_) = self.type_of(rhs, scope, contents) {
						let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
						scope.insert(lhs.into_owned(), type_);
					}
				}
			}
			"for_statement" => {
				// (for_statement left right body)
				scope.enter();
				let lhs = node.named_child(0).unwrap();
				if lhs.kind() == "identifier" {
					let rhs = lhs.next_named_sibling().unwrap();
					if let Some(type_) = self.type_of(rhs, scope, contents) {
						let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
						scope.insert(lhs.into_owned(), type_);
					}
				}
				return ControlFlow::Continue(true);
			}
			"function_definition" => {
				scope.enter();
				return ControlFlow::Continue(true);
			}
			"list_comprehension" | "set_comprehension" | "dictionary_comprehension" | "generator_expression"
				if node.byte_range().contains(&offset) =>
			{
				// (_ body (for_in_clause left right))
				let for_in = node.named_child(1).unwrap();
				let lhs = for_in.named_child(0).unwrap();
				if lhs.kind() == "identifier" {
					let rhs = lhs.next_named_sibling().unwrap();
					if let Some(type_) = self.type_of(rhs, scope, contents) {
						let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
						scope.insert(lhs.into_owned(), type_);
					}
				}
			}
			"call" if node.byte_range().contains_end(offset) => {
				// model.{mapped,filtered,sorted,*}(lambda rec: ..)
				let query = MappedCall::query();
				let mut cursor = QueryCursor::new();
				if let Some(mapped_call) = cursor.matches(query, node, contents).next() {
					let callee = mapped_call
						.nodes_for_capture_index(MappedCall::Callee as _)
						.next()
						.unwrap();
					if let Some(type_) = self.type_of(callee, scope, contents) {
						let iter = mapped_call
							.nodes_for_capture_index(MappedCall::Iter as _)
							.next()
							.unwrap();
						let iter = String::from_utf8_lossy(&contents[iter.byte_range()]);
						scope.insert(iter.into_owned(), type_);
					}
				}
			}
			"with_statement" => {
				// with Form(self.env['..']) as alias:
				// TODO: Support more structures as needed
				// (with_statement
				// 	 (with_clause
				//     (with_item
				//       (as_pattern
				//         (call (identifier) ..)
				//         (as_pattern_target (identifier))))))
				let as_pattern = node
					.named_child(0)
					.expect("with_clause")
					.named_child(0)
					.expect("with_item")
					.named_child(0)
					.expect("as_pattern");
				if as_pattern.kind() == "as_pattern" {
					let value = as_pattern.named_child(0).unwrap();
					if let Some(target) = value.next_named_sibling() {
						if target.kind() == "as_pattern_target" {
							let alias = target.named_child(0).unwrap();
							if alias.kind() == "identifier" && value.kind() == "call" {
								let callee = value.named_child(0).expect("identifier");
								// TODO: Remove this hardcoded case
								if callee.kind() == "identifier" && b"Form" == &contents[callee.byte_range()] {
									if let Some(first_arg) = value.named_child(1).unwrap().named_child(0) {
										if let Some(type_) = self.type_of(first_arg, scope, contents) {
											let alias =
												String::from_utf8_lossy(&contents[alias.byte_range()]).into_owned();
											scope.insert(alias, type_);
										}
									}
								} else if let Some(type_) = self.type_of(value, scope, contents) {
									let alias = String::from_utf8_lossy(&contents[alias.byte_range()]).into_owned();
									scope.insert(alias, type_);
								}
							}
						}
					}
				}
			}
			_ => {}
		}

		ControlFlow::Continue(false)
	}
	/// [Type::Value] is not returned by this method.
	pub fn type_of(&self, mut node: Node, scope: &Scope, contents: &[u8]) -> Option<Type> {
		// What contributes to value types?
		// 1. *.env['foo'] => Model('foo')
		// 2. *.env.ref(<record-id>) => Model(<model of record-id>)

		// What preserves value types?
		// 1. for foo in bar;
		//    bar: 't => foo: 't
		// 2. foo = bar;
		//    bar: 't => foo: 't (and various other operators)
		// 3. foo.sudo();
		//    foo: 't => foo.sudo(): 't
		//    sudo, with_user, with_env, with_context, ..
		// 4. [foo for foo in bar];
		//    bar: 't => foo: 't
		// 5: foo[..]
		//    foo: 't => foo[..]: 't

		// What transforms value types?
		// 1. foo.bar;
		//    foo: Model('t) => bar: Model('t).field('bar')
		// 2. foo.mapped('..')
		//    foo: Model('t) => _: Model('t).mapped('..')
		// 3. foo.mapped(lambda rec: 't): 't
		if node.byte_range().len() <= 64 {
			trace!(
				"type_of {} '{}'",
				node.kind(),
				String::from_utf8_lossy(&contents[node.byte_range()])
			);
		} else {
			trace!("type_of {} range={:?}", node.kind(), node.byte_range());
		}
		let interner = interner();
		match normalize(&mut node).kind() {
			"subscript" => {
				let lhs = node.child_by_field_name("value")?;
				let rhs = node.child_by_field_name("subscript")?;
				let obj_ty = self.type_of(lhs, scope, contents)?;
				match obj_ty {
					Type::Env if rhs.kind() == "string" => Some(Type::Model(
						String::from_utf8_lossy(&contents[rhs.byte_range().shrink(1)])
							.as_ref()
							.into(),
					)),
					Type::Model(_) | Type::Record(_) => Some(obj_ty),
					_ => None,
				}
			}
			"attribute" => {
				let lhs = node.named_child(0)?;
				let lhs = self.type_of(lhs, scope, contents)?;
				let rhs = node.named_child(1)?;
				match &contents[rhs.byte_range()] {
					b"env" if matches!(lhs, Type::Model(..) | Type::Record(..)) => Some(Type::Env),
					b"ref" if matches!(lhs, Type::Env) => Some(Type::RefFn),
					b"user" if matches!(lhs, Type::Env) => Some(Type::Model("res.users".into())),
					b"company" | b"companies" if matches!(lhs, Type::Env) => Some(Type::Model("res.company".into())),
					b"mapped" => {
						let model = self.resolve_type(&lhs, scope)?;
						Some(Type::Method(model, "mapped"))
					}
					func if MODEL_METHODS.contains(func) => match lhs {
						Type::Model(model) => Some(Type::ModelFn(model)),
						Type::Record(xml_id) => {
							let xml_id = interner.get(xml_id)?;
							let record = self.index.records.get(&xml_id.into())?;
							Some(Type::ModelFn(interner.resolve(record.model.as_deref()?).into()))
						}
						_ => None,
					},
					ident if rhs.kind() == "identifier" => self.type_of_attribute(&lhs, ident, scope),
					_ => None,
				}
			}
			"identifier" => {
				let key = String::from_utf8_lossy(&contents[node.byte_range()]);
				if key == "super" {
					return Some(Type::Super);
				}
				scope.get(key).cloned()
			}
			"assignment" => {
				let rhs = node.named_child(1)?;
				self.type_of(rhs, scope, contents)
			}
			"call" => {
				let func = node.named_child(0)?;
				let func = self.type_of(func, scope, contents)?;
				match func {
					Type::RefFn => {
						// (call (_) @func (argument_list . (string) @xml_id))
						let xml_id = node.named_child(1)?.named_child(0)?;
						matches!(xml_id.kind(), "string").then(|| {
							Type::Record(
								String::from_utf8_lossy(&contents[xml_id.byte_range().shrink(1)])
									.as_ref()
									.into(),
							)
						})
					}
					Type::ModelFn(model) => Some(Type::Model(model)),
					Type::Super => scope.get(scope.super_.as_deref()?).cloned(),
					Type::Method(model, "mapped") => {
						// (call (_) @func (argument_list . [(string) (lambda)] @mapped))
						let mapped = node.named_child(1)?.named_child(0)?;
						match mapped.kind() {
							"string" => {
								let mut model = model.into();
								let mapped = String::from_utf8_lossy(&contents[mapped.byte_range().shrink(1)]);
								let mut mapped = mapped.as_ref();
								self.index.models.resolve_mapped(&mut model, &mut mapped, None).ok()?;
								self.type_of_attribute(
									&Type::Model(interner.resolve(&model).into()),
									mapped.as_bytes(),
									scope,
								)
							}
							"lambda" => {
								// (lambda (lambda_parameters)? body: (_))
								let mut scope = Scope::new(Some(scope.clone()));
								if let Some(params) = mapped.child_by_field_name(b"parameters") {
									let first_arg = params.named_child(0)?;
									if first_arg.kind() == "identifier" {
										let first_arg = String::from_utf8_lossy(&contents[first_arg.byte_range()]);
										scope.insert(
											first_arg.into_owned(),
											Type::Model(interner.resolve(&model).into()),
										);
									}
								}
								let body = mapped.child_by_field_name(b"body")?;
								self.type_of(body, &scope, contents)
							}
							_ => None,
						}
					}
					Type::Env | Type::Record(..) | Type::Method(..) | Type::Model(..) | Type::Value => None,
				}
			}
			"binary_operator" | "boolean_operator" => {
				// (_ left right)
				self.type_of(node.child_by_field_name("left")?, &scope, contents)
			}
			_ => None,
		}
	}
	pub fn type_of_attribute(&self, type_: &Type, attr: &[u8], scope: &Scope) -> Option<Type> {
		let model = self.resolve_type(type_, scope)?;
		let attr = String::from_utf8_lossy(attr);
		self.index.models.populate_field_names(model, &[])?;
		let attr = interner().get(attr.as_ref())?;
		let relation = self.index.models.normalize_field_relation(attr.into(), model.into())?;
		Some(Type::Model(interner().resolve(&relation).into()))
	}
	pub fn has_attribute(&self, type_: &Type, attr: &[u8], scope: &Scope) -> bool {
		(|| -> Option<()> {
			let model = self.resolve_type(type_, scope)?;
			let attr = String::from_utf8_lossy(attr);
			let entry = self.index.models.populate_field_names(model, &[])?;
			let attr = interner().get(attr.as_ref())?;
			if entry.fields.as_ref().unwrap().contains_key(&attr.into()) {
				Some(())
			} else {
				None
			}
		})()
		.is_some()
	}
	/// Call this method if it's unclear whether `type_` is a [`Type::Model`] and you just want the model's name.
	pub fn resolve_type(&self, type_: &Type, scope: &Scope) -> Option<ModelName> {
		match type_ {
			Type::Model(model) => Some(interner().get(model)?.into()),
			Type::Record(xml_id) => {
				// TODO: Refactor into method
				let xml_id = interner().get(xml_id)?;
				let record = self.index.records.get(&xml_id.into())?;
				record.model
			}
			Type::Super => self.resolve_type(scope.get(scope.super_.as_deref()?)?, scope),
			_ => None,
		}
	}
	/// Iterates depth-first over `node` using [PreTravel]. Automatically calls [`Scope::exit`] at suitable points.
	///
	/// [`ControlFlow::Continue`] accepts a boolean to indicate whether [`Scope::enter`] was called.
	///
	/// To accumulate bindings into a scope, use [`Backend::build_scope`].
	pub fn walk_scope<T>(
		&self,
		node: Node,
		scope: Option<Scope>,
		mut step: impl FnMut(&mut Scope, Node) -> ControlFlow<Option<T>, bool>,
	) -> Option<T> {
		let mut scope = scope.unwrap_or_default();
		let mut scope_ends = vec![];
		for node in PreTravel::new(node) {
			if !node.is_named() {
				continue;
			}
			if let Some(end) = scope_ends.last() {
				if node.start_byte() > *end {
					scope.exit();
					scope_ends.pop();
				}
			}
			match step(&mut scope, node) {
				ControlFlow::Break(value) => return value,
				ControlFlow::Continue(entered) => {
					if entered {
						scope_ends.push(node.end_byte());
					}
				}
			}
		}
		None
	}
}

/// Returns `(self_type, fn_scope, self_param)`
pub fn determine_scope<'out, 'node>(
	node: Node<'node>,
	contents: &'out [u8],
	offset: usize,
) -> Option<(Option<Node<'node>>, Node<'node>, std::borrow::Cow<'out, str>)> {
	let query = FieldCompletion::query();
	let mut self_type = None;
	let mut self_param = None;
	let mut fn_scope = None;
	let mut cursor = QueryCursor::new();
	'scoping: for match_ in cursor.matches(query, node, contents) {
		// @class
		let class = match_.captures.first()?;
		if !class.node.byte_range().contains_end(offset) {
			continue;
		}
		for capture in match_.captures {
			match FieldCompletion::from(capture.index) {
				Some(FieldCompletion::Name) => {
					self_type = Some(capture.node);
				}
				Some(FieldCompletion::SelfParam) => {
					self_param = Some(capture.node);
				}
				Some(FieldCompletion::Scope) if capture.node.byte_range().contains_end(offset) => {
					fn_scope = Some(capture.node);
					break 'scoping;
				}
				Some(FieldCompletion::Scope) | None => {}
			}
		}
	}
	let fn_scope = fn_scope?;
	let self_param = String::from_utf8_lossy(&contents[self_param?.byte_range()]);
	Some((self_type, fn_scope, self_param))
}

#[cfg(test)]
mod tests {
	use tree_sitter::{Parser, QueryCursor};

	use crate::analyze::FieldCompletion;

	#[test]
	fn test_field_completion() {
		let mut parser = Parser::new();
		parser.set_language(tree_sitter_python::language()).unwrap();
		let contents = br#"
class Foo(models.AbstractModel):
	_name = 'foo'
	_description = 'What?'
	_inherit = 'inherit_foo'
	foo = fields.Char(related='related')
	@api.depends('mapped')
	def foo(self):
		pass
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = FieldCompletion::query();
		let mut cursor = QueryCursor::new();
		// let expected: &[&[u32]] = &[];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.map(|capture| FieldCompletion::from(capture.index))
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		// Allow nested patterns
		let actual = actual.iter().map(Vec::as_slice).collect::<Vec<_>>();
		use FieldCompletion as T;
		assert!(
			matches!(
				&actual[..],
				[
					[None, None, Some(T::Name), Some(T::SelfParam), Some(T::Scope)],
					[None, None, Some(T::Name), Some(T::SelfParam), Some(T::Scope)]
				]
			),
			"{actual:?}"
		)
	}
}
