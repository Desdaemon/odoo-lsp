use std::{borrow::Borrow, collections::HashMap};

use futures::executor::block_on;
use log::debug;
use tree_sitter::{Node, QueryCursor};

use odoo_lsp::{
	index::interner,
	model::{FieldKind, ModelName},
	utils::{ByteRange, Erase, RangeExt},
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
	b"sudo"
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
}

#[derive(Default)]
pub struct Scope {
	variables: HashMap<String, Type>,
	parent: Option<Box<Scope>>,
	/// TODO: Allow super(_, \<self>)
	super_: Option<ImStr>,
}

fn normalize<'r, 'n>(node: &'r mut Node<'n>) -> &'r mut Node<'n> {
	while matches!(node.kind(), "expression_statement" | "parenthesized_expression") {
		*node = node.named_child(0).unwrap();
	}
	node
}

impl Scope {
	#[inline]
	pub fn new(parent: Option<Scope>) -> Scope {
		Self {
			variables: Default::default(),
			parent: parent.map(Box::new),
			super_: None,
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
	pub fn insert(&mut self, key: String, value: Type) -> bool {
		if let Some(parent) = &mut self.parent {
			if parent.insert(key.clone(), value.clone()) {
				return true;
			}
		}
		if self.variables.contains_key(&key) {
			return false;
		}
		self.variables.insert(key, value);
		true
	}
}

query! {
	FieldCompletion(NAME, INHERIT, SELF, SCOPE);
r#"
((class_definition
	(block
		[
			 (expression_statement
				(assignment (identifier) @_inherit [
					(string) @INHERIT
					(list ((string) @INHERIT ","?)*)
				])
			)
			(expression_statement (assignment (identifier) @_name (string) @NAME))
		]+
		[
			(decorated_definition
				(function_definition
					(parameters . (identifier) @SELF) (block) @SCOPE) .)
			(function_definition (parameters . (identifier) @SELF) (block) @SCOPE)
		])
) @class
(#eq? @_name "_name")
(#eq? @_inherit "_inherit"))"#
}

impl Backend {
	pub fn model_of_range(
		&self,
		node: Node<'_>,
		range: ByteRange,
		scope: Option<Scope>,
		contents: &[u8],
	) -> Option<ModelName> {
		debug!("model_of_range {}", String::from_utf8_lossy(&contents[range.erase()]));
		// Phase 1: Determine the scope.
		let query = FieldCompletion::query();
		let mut self_type = None; // (string)
		let mut self_param = None; // (identifier)
		let mut fn_scope = None; // (block)
		let mut cursor = QueryCursor::new();
		'scoping: for match_ in cursor.matches(query, node, contents) {
			// @class
			let class = match_.captures.first()?;
			if !class.node.byte_range().contains(&range.end.0) && class.node.end_byte() != range.end.0 {
				continue;
			}
			for capture in match_.captures {
				if capture.index == FieldCompletion::NAME {
					self_type = Some(capture.node);
				} else if capture.index == FieldCompletion::INHERIT && self_type.is_none() {
					// @inherit
					self_type = Some(capture.node);
				} else if capture.index == FieldCompletion::SELF {
					self_param = Some(capture.node);
				} else if capture.index == FieldCompletion::SCOPE
					&& (capture.node.byte_range().contains(&range.end.0) || capture.node.end_byte() == range.end.0)
				{
					// @scope
					fn_scope = Some(capture.node);
					break 'scoping;
				}
			}
		}
		let node = fn_scope?;
		let self_param = String::from_utf8_lossy(&contents[self_param?.byte_range()]);

		// Phase 2: Build the scope up to offset
		// What contributes to a method scope's variables?
		// 1. Top-level statements; completely opaque to us.
		// 2. Class definitions; technically useless to us.
		//    Self-type analysis only uses a small part of the class definition.
		// 3. Parameters, e.g. self which always has a fixed type
		// 4. Assignments (including walrus-assignment)
		let mut scope = scope.unwrap_or_default();
		let self_type = match self_type {
			Some(type_) => &contents[type_.byte_range().contract(1)],
			None => &[],
		};
		scope.super_ = Some(self_param.as_ref().into());
		scope.insert(
			self_param.into_owned(),
			Type::Model(String::from_utf8_lossy(self_type).as_ref().into()),
		);
		let mut cursor = node.walk();
		let mut children = node.named_children(&mut cursor).peekable();
		let mut stack = vec![];
		let mut target = None;
		while let Some(mut child) = children.next() {
			// What creates local scope, but doesn't contribute to method scope?
			// 1. For-statements: only within the block
			// 2. List comprehension: only within the object
			match normalize(&mut child).kind() {
				"assignment" => {
					let lhs = child.named_child(0)?;
					let rhs = child.named_child(1)?;
					if lhs.kind() == "identifier" {
						let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
						let type_ = self.type_of(rhs, &scope, contents)?;
						scope.insert(lhs.into_owned(), type_);
					}
				}
				"for_statement" => {
					let iter = child.named_child(0)?;
					let src = child.named_child(1)?;
					if iter.kind() == "identifier" {
						let type_ = self.type_of(src, &scope, contents)?;
						if let Some(next) = children.peek().cloned() {
							stack.push(next);
						}
						drop(children);
						// (for_statement (_) @iteratee (_) @src (block))
						children = child.named_child(2)?.named_children(&mut cursor).peekable();
						scope = Scope::new(Some(scope));
						let iter = String::from_utf8_lossy(&contents[iter.byte_range()]);
						scope.insert(iter.into_owned(), type_);
					}
				}
				"list_comprehension" | "set_comprehension" | "dictionary_comprehension" => {
					// (_ body: (_) (for_in_clause))
					let clause = child.named_child(1)?;
					let iter = clause.named_child(0)?;
					let src = clause.named_child(1)?;
					if iter.kind() == "identifier" {
						let type_ = self.type_of(src, &scope, contents)?;
						if let Some(next) = children.peek().cloned() {
							stack.push(next);
						}
						drop(children);
						children = clause.named_children(&mut cursor).peekable();
						scope = Scope::new(Some(scope));
						let iter = String::from_utf8_lossy(&contents[iter.byte_range()]);
						scope.insert(iter.into_owned(), type_);
					}
				}
				_ => {}
			}
			if child.byte_range().contains(&range.end.0) || child.end_byte() == range.end.0 {
				target = Some(child);
				break;
			}
			if children.peek().is_none() {
				if let Some(next) = stack.pop() {
					drop(children);
					children = next.named_children(&mut cursor).peekable();
					scope = *scope.parent?;
				}
			}
		}
		// Phase 3: Determine type of cursor expression using recursive ascent
		let mut descendant = Some(target?.descendant_for_byte_range(range.start.0, range.end.0)?);
		while let Some(descendant_) = descendant {
			match self.type_of(descendant_, &scope, contents) {
				Some(Type::Model(model)) => {
					return interner().get(model).map(Into::into);
				}
				Some(Type::Record(xml_id)) => {
					// TODO: Refactor into method
					let interner = interner();
					let xml_id = interner.get(xml_id)?;
					let record = self.index.records.get(&xml_id.into())?;
					return record.model;
				}
				_ => {}
			}
			descendant = descendant_.parent();
		}
		None
	}
	fn type_of(&self, mut node: Node, scope: &Scope, contents: &[u8]) -> Option<Type> {
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

		// What transforms value types?
		// 1. foo.bar;
		//    foo: Model('t) => bar: Model('t).field('bar')
		let interner = interner();
		let kind = normalize(&mut node).kind();
		match kind {
			"subscript" => {
				let rhs = node.named_child(1)?;
				let rhs_range = rhs.byte_range().contract(1);
				if rhs.kind() != "string" {
					return None;
				}
				let lhs = node.named_child(0)?;
				let obj_ty = self.type_of(lhs, scope, contents)?;
				matches!(obj_ty, Type::Env)
					.then(|| Type::Model(String::from_utf8_lossy(&contents[rhs_range]).as_ref().into()))
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
					func if MODEL_METHODS.contains(func) => match lhs {
						Type::Model(model) => Some(Type::ModelFn(model)),
						Type::Record(xml_id) => {
							let xml_id = interner.get(xml_id)?;
							let record = self.index.records.get(&xml_id.into())?;
							Some(Type::ModelFn(interner.resolve(record.model.as_deref()?).into()))
						}
						_ => None,
					},
					ident if rhs.kind() == "identifier" => {
						let model = match lhs {
							Type::Model(model) => ModelName::from(interner.get(model.as_str())?),
							Type::Record(xml_id) => {
								let xml_id = interner.get(xml_id)?;
								let record = self.index.records.get(&xml_id.into())?;
								record.model?
							}
							_ => return None,
						};
						let ident = String::from_utf8_lossy(ident);
						let ident = interner.get_or_intern(ident.as_ref());
						let mut entry = self.index.models.get_mut(&model)?;
						let fields = block_on(self.populate_field_names(&mut entry));
						let field = fields.ok()?.get(&ident.into())?;
						match field.kind {
							FieldKind::Relational(model) => Some(Type::Model(interner.resolve(&model).into())),
							FieldKind::Value => None,
						}
					}
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
								String::from_utf8_lossy(&contents[xml_id.byte_range().contract(1)])
									.as_ref()
									.into(),
							)
						})
					}
					Type::ModelFn(model) => Some(Type::Model(model)),
					Type::Super => scope.get(scope.super_.as_deref()?).cloned(),
					Type::Env | Type::Record(..) | Type::Model(..) => None,
				}
			}
			_ => None,
		}
	}
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
	_inherit = ['inherit_foo', 'inherit_bar']
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
			.map(|match_| match_.captures.iter().map(|capture| capture.index).collect::<Vec<_>>())
			.collect::<Vec<_>>();
		// Allow nested patterns
		let actual = actual.iter().map(Vec::as_slice).collect::<Vec<_>>();
		use FieldCompletion as T;
		assert!(
			matches!(
				&actual[..],
				[[_, _, T::NAME, _, T::INHERIT, T::INHERIT, T::SELF, T::SCOPE]]
			),
			"{actual:?}"
		)
	}
}
