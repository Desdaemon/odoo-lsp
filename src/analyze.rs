use std::{borrow::Borrow, collections::HashMap};

use futures::executor::block_on;
use log::debug;
use tree_sitter::{Node, QueryCursor};

use odoo_lsp::{
	index::interner,
	model::{FieldKind, ModelName},
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
}

query! {
	FieldCompletion(NAME, SELF, SCOPE);
r#"
((class_definition
	(block
		(expression_statement
			(assignment (identifier) @_name (string) @NAME))
		[
			(decorated_definition
				(function_definition
					(parameters . (identifier) @SELF) (block) @SCOPE) .)
			(function_definition (parameters . (identifier) @SELF) (block) @SCOPE)
		])) @class
(#match? @_name "^_(name|inherit)$"))"#
}

query! {
	MappedCall(CALLEE, ITER);
r#"
((call
	(attribute (_) @CALLEE (identifier) @_mapped)
	(argument_list
		[(lambda (lambda_parameters . (identifier) @ITER))
		 (keyword_argument
			(identifier) @_func
			(lambda (lambda_parameters . (identifier) @ITER)))]))
(#match? @_func "^(func|key)$")
(#match? @_mapped "^(mapp|filter|sort)ed$"))"#
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
			if !class.node.byte_range().contains_end(range.end.0) {
				continue;
			}
			for capture in match_.captures {
				if capture.index == FieldCompletion::NAME {
					self_type = Some(capture.node);
				} else if capture.index == FieldCompletion::SELF {
					self_param = Some(capture.node);
				} else if capture.index == FieldCompletion::SCOPE && capture.node.byte_range().contains_end(range.end.0)
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
		let mut scope_ends = Vec::<usize>::new();
		let self_type = match self_type {
			Some(type_) => &contents[type_.byte_range().shrink(1)],
			None => &[],
		};
		scope.super_ = Some(self_param.as_ref().into());
		scope.insert(
			self_param.into_owned(),
			Type::Model(String::from_utf8_lossy(self_type).as_ref().into()),
		);
		let mut cursor = PreTravel::new(node);
		while let Some(node) = cursor.next() {
			if !node.is_named() {
				continue;
			};
			if node.start_byte() > range.end.0 {
				break;
			}
			if let Some(end) = scope_ends.last() {
				if node.start_byte() > *end {
					scope.exit();
					scope_ends.pop();
				}
			}
			match node.kind() {
				"assignment" | "named_expression" => {
					// (_ left right)
					let lhs = node.named_child(0).unwrap();
					if lhs.kind() == "identifier" {
						let rhs = lhs.next_named_sibling().unwrap();
						if let Some(type_) = self.type_of(rhs, &scope, contents) {
							let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
							scope.insert(lhs.into_owned(), type_);
						}
					}
				}
				"for_statement" => {
					// (for_statement left right body)
					scope.enter();
					scope_ends.push(node.end_byte());
					let lhs = node.named_child(0).unwrap();
					if lhs.kind() == "identifier" {
						let rhs = lhs.next_named_sibling().unwrap();
						if let Some(type_) = self.type_of(rhs, &scope, contents) {
							let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
							scope.insert(lhs.into_owned(), type_);
						}
					}
				}
				"list_comprehension" | "set_comprehension" | "dictionary_comprehension" | "generator_expression"
					if node.byte_range().contains(&range.end.0) =>
				{
					// (_ body (for_in_clause left right))
					let for_in = node.named_child(1).unwrap();
					let lhs = for_in.named_child(0).unwrap();
					if lhs.kind() == "identifier" {
						let rhs = lhs.next_named_sibling().unwrap();
						if let Some(type_) = self.type_of(rhs, &scope, contents) {
							let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
							scope.insert(lhs.into_owned(), type_);
						}
					}
				}
				"call" if node.byte_range().contains_end(range.end.0) => {
					// model.{mapped,filtered,sorted,*}(lambda rec: ..)
					let query = MappedCall::query();
					let mut cursor = QueryCursor::new();
					if let Some(mapped_call) = cursor.matches(query, node, contents).next() {
						let callee = mapped_call.nodes_for_capture_index(MappedCall::CALLEE).next().unwrap();
						if let Some(type_) = self.type_of(callee, &scope, contents) {
							let iter = mapped_call.nodes_for_capture_index(MappedCall::ITER).next().unwrap();
							let iter = String::from_utf8_lossy(&contents[iter.byte_range()]);
							scope.insert(iter.into_owned(), type_);
						}
					}
				}
				_ => {}
			}
		}

		let node_at_cursor = node.descendant_for_byte_range(range.start.0, range.end.0)?;
		let type_at_cursor = self.type_of(node_at_cursor, &scope, contents)?;
		self.resolve_type(&type_at_cursor, &scope)
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
		// 5. self.mapped(lambda rec: ..)
		//    self: 't => rec: 't
		//    mapped, filtered, ..

		// What transforms value types?
		// 1. foo.bar;
		//    foo: Model('t) => bar: Model('t).field('bar')
		if node.byte_range().len() <= 64 {
			debug!(
				"type_of {} '{}'",
				node.kind(),
				String::from_utf8_lossy(&contents[node.byte_range()])
			);
		} else {
			debug!("type_of {} range={:?}", node.kind(), node.byte_range());
		}
		let interner = interner();
		match normalize(&mut node).kind() {
			"subscript" => {
				let rhs = node.named_child(1)?;
				let rhs_range = rhs.byte_range().shrink(1);
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
						let entry = self.populate_field_names(model, &[])?;
						let entry = block_on(entry);
						let field = entry.fields.as_ref()?.get(&ident.into())?;
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
								String::from_utf8_lossy(&contents[xml_id.byte_range().shrink(1)])
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
	fn resolve_type(&self, type_: &Type, scope: &Scope) -> Option<ModelName> {
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
			.map(|match_| match_.captures.iter().map(|capture| capture.index).collect::<Vec<_>>())
			.collect::<Vec<_>>();
		// Allow nested patterns
		let actual = actual.iter().map(Vec::as_slice).collect::<Vec<_>>();
		use FieldCompletion as T;
		assert!(
			matches!(
				&actual[..],
				[[_, _, T::NAME, T::SELF, T::SCOPE], [_, _, T::NAME, T::SELF, T::SCOPE]]
			),
			"{actual:?}"
		)
	}
}
