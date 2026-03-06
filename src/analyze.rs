//! Methods related to type analysis. The two most important methods are
//! [`Index::model_of_range`] and [`Index::type_of`].

use std::{
	fmt::{Debug, Write},
	ops::ControlFlow,
	sync::{Arc, OnceLock, RwLock, atomic::Ordering},
};

use dashmap::DashMap;
use fomat_macros::fomat;
use lasso::Spur;
use ropey::Rope;
use tracing::instrument;
use tree_sitter::{Node, Parser, QueryCursor, StreamingIterator};

use crate::{
	ImStr, dig, format_loc,
	index::{_G, _I, _R, Index, Symbol},
	model::{Method, ModelName, PropertyInfo},
	test_utils,
	utils::{ByteOffset, ByteRange, Defer, PreTravel, RangeExt, TryResultExt, python_next_named_sibling, rope_conv},
};
use ts_macros::query;

mod scope;
pub use scope::Scope;

pub fn type_cache() -> &'static TypeCache {
	static CACHE: OnceLock<TypeCache> = OnceLock::new();
	CACHE.get_or_init(TypeCache::default)
}

macro_rules! _T {
	(@ $builtin:expr) => {
		$crate::analyze::type_cache().get_or_intern(Type::PyBuiltin($builtin.into()))
	};
	($model:literal) => {
		$crate::analyze::type_cache().get_or_intern(Type::Model($model.into()))
	};
	($expr:expr) => {
		$crate::analyze::type_cache().get_or_intern($expr)
	};
}

pub static MODEL_METHODS: phf::Set<&str> = phf::phf_set!(
	"create",
	"copy",
	"name_create",
	"browse",
	"filtered",
	"filtered_domain",
	"sorted",
	"search",
	"search_fetch",
	"name_search",
	"ensure_one",
	"with_context",
	"with_user",
	"with_company",
	"with_env",
	"sudo",
	"exists",
	// TODO: Limit to Forms only
	"new",
	"edit",
	"save",
);

/// The subset of types that may resolve to a model.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
	Method(ModelName, ImStr),
	/// To hardcode some methods, such as dict.items()
	PythonMethod(TypeId, ImStr),
	/// `odoo.http.request`
	HttpRequest,
	Dict(TypeId, TypeId),
	/// A bag of enumerated properties and their types
	DictBag(Vec<(DictKey, TypeId)>),
	/// Equivalent to Value, but may have a better semantic name
	PyBuiltin(ImStr),
	List(ListElement),
	Tuple(Vec<TypeId>),
	Iterable(Option<TypeId>),
	/// Can never be resolved, useful for non-model bindings.
	Value,
}

impl Type {
	#[inline]
	fn is_dict(&self) -> bool {
		matches!(self, Type::Dict(..))
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ListElement {
	Vacant,
	Occupied(TypeId),
}

impl Debug for ListElement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Vacant => f.write_str("..."),
			Self::Occupied(inner) => inner.fmt(f),
		}
	}
}

impl From<ListElement> for Option<TypeId> {
	#[inline]
	fn from(value: ListElement) -> Self {
		match value {
			ListElement::Vacant => None,
			ListElement::Occupied(inner) => Some(inner),
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum DictKey {
	String(ImStr),
	Type(TypeId),
}

impl Debug for DictKey {
	#[inline]
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::String(key) => key.fmt(f),
			Self::Type(key) => key.fmt(f),
		}
	}
}

#[derive(Clone, Debug)]
pub enum FunctionParam {
	Param(ImStr),
	/// `(positional_separator)`
	PosEnd,
	/// `(keyword_separator)` or `(list_splat_pattern)`
	EitherEnd(Option<ImStr>),
	/// `(default_parameter)`
	Named(ImStr),
	Kwargs(ImStr),
}

impl core::fmt::Display for FunctionParam {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FunctionParam::Param(param) => f.write_str(param),
			FunctionParam::PosEnd => f.write_char('/'),
			FunctionParam::EitherEnd(None) => f.write_char('*'),
			FunctionParam::EitherEnd(Some(param)) => write!(f, "*{param}"),
			FunctionParam::Named(param) => write!(f, "{param}=..."),
			FunctionParam::Kwargs(param) => write!(f, "**{param}"),
		}
	}
}

#[derive(Default)]
pub struct TypeCache {
	types: RwLock<Vec<Type>>,
	ids: DashMap<Type, TypeId>,
}

impl TypeCache {
	#[inline]
	pub fn get_or_intern(&self, type_: Type) -> TypeId {
		if let Some(id) = self.ids.get(&type_) {
			return *id;
		}
		self.intern(type_)
	}
	fn intern(&self, type_: Type) -> TypeId {
		let mut types = self.types.write().unwrap();
		let id = TypeId(types.len() as u32);
		types.push(type_.clone());
		self.ids.insert(type_, id);
		id
	}
	#[inline]
	pub fn resolve(&self, id: TypeId) -> Type {
		self.types.read().unwrap()[id.0 as usize].clone()
	}
	pub fn is_dictlike(&self, id: TypeId) -> bool {
		let types = self.types.read().unwrap();
		matches!(
			&unsafe { types.get_unchecked(id.0 as usize) },
			Type::Dict(..) | Type::DictBag(..)
		)
	}
	pub fn is_dict(&self, id: TypeId) -> bool {
		let types = self.types.read().unwrap();
		matches!(&unsafe { types.get_unchecked(id.0 as usize) }, Type::Dict(..))
	}
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl Debug for TypeId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		type_cache().resolve(*self).fmt(f)
	}
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

#[rustfmt::skip]
query! {
	#[derive(Debug)]
	FieldCompletion(Name, SelfParam, Scope);
((class_definition
  (block
    (expression_statement [
      (assignment (identifier) @_name (string) @NAME)
	  (assignment (identifier) @_inherit (list . (string) @NAME)) ])?
    [
      (decorated_definition
        (function_definition
          (parameters . (identifier) @SELF_PARAM)) @SCOPE)
      (function_definition (parameters . (identifier) @SELF_PARAM)) @SCOPE])) @class
  (#eq? @_inherit "_inherit")
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
  (#match? @_mapped "^(mapp|filter|sort|group)ed$"))
}

#[rustfmt::skip]
query! {
	PythonBuiltinCall(Append, AppendList, AppendMap, AppendMapKey, AppendValue, UpdateMap, UpdateArgs);
// value.append(...) OR value['foobar'].append(...)
(call
  (attribute
    (subscript (identifier) @APPEND_MAP (string (string_content) @APPEND_MAP_KEY))
    (identifier) @_append)
  (argument_list . (_) @APPEND_VALUE)
  (#eq? @_append "append"))

(call
  (attribute
    (identifier) @APPEND_LIST
    (identifier) @APPEND)
  (argument_list . (_) @APPEND_VALUE)
  (#eq? @_append "append"))

// value.update(...)
(call
  (attribute
  	(identifier) @UPDATE_MAP
  	(identifier) @_update)
  (argument_list) @UPDATE_ARGS
  (#eq? @_update "update"))
}

pub type ScopeControlFlow = ControlFlow<Option<Scope>, bool>;
impl Index {
	#[inline]
	pub fn model_of_range(&self, node: Node<'_>, range: ByteRange, contents: &str) -> Option<ModelName> {
		let (type_at_cursor, scope) = self.type_of_range(node, range, contents)?;
		self.try_resolve_model(&type_cache().resolve(type_at_cursor), &scope)
	}
	pub fn type_of_range(&self, root: Node<'_>, range: ByteRange, contents: &str) -> Option<(TypeId, Scope)> {
		// Phase 1: Determine the scope.
		let (self_type, fn_scope, self_param) = determine_scope(root, contents, range.start.0)?;

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
			None => "",
		};
		scope.super_ = Some(self_param.into());
		scope.insert(self_param.to_string(), Type::Model(self_type.into()));
		let (orig_scope, scope) = Self::walk_scope(fn_scope, Some(scope), |scope, node| {
			self.build_scope(scope, node, range.end.0, contents)
		});
		let scope = scope.unwrap_or(orig_scope);

		// Phase 3: With the proper context available, determine the type of the node.
		let node_at_cursor = fn_scope.descendant_for_byte_range(range.start.0, range.end.0)?;

		// special case: is this a property?
		// TODO: fields
		if node_at_cursor.kind() == "identifier" && fn_scope.child_by_field_name("name") == Some(node_at_cursor) {
			return Some((
				_T!(Type::Method(
					_I(self_type).into(),
					contents[node_at_cursor.byte_range()].into()
				)),
				scope,
			));
		}

		let type_at_cursor = self.type_of(node_at_cursor, &scope, contents)?;
		Some((type_at_cursor, scope))
	}
	/// Builds the scope up to `offset`, in bytes.
	///
	/// #### About [ScopeControlFlow]
	/// This is one of the rare occasions where [ControlFlow] is used. It is similar to
	/// [Result] in that the try-operator (?) can be used to end iteration on a
	/// [ControlFlow::Break]. Otherwise, [ControlFlow::Continue] has a continuation value
	/// that must be passed up the chain, since it indicates whether [Scope::enter] was called.
	pub fn build_scope(&self, scope: &mut Scope, node: Node, offset: usize, contents: &str) -> ScopeControlFlow {
		if node.start_byte() > offset {
			return ControlFlow::Break(Some(core::mem::take(scope)));
		}
		match node.kind() {
			"assignment" | "named_expression" => {
				// (_ left right)
				let lhs = node.named_child(0).unwrap();
				if lhs.kind() == "identifier"
					&& let rhs = python_next_named_sibling(lhs).expect(format_loc!("rhs"))
					&& let Some(id) = self.type_of(rhs, scope, contents)
				{
					let lhs = &contents[lhs.byte_range()];
					scope.insert(lhs.to_string(), type_cache().resolve(id));
				} else if lhs.kind() == "subscript"
					&& let Some(map) = dig!(lhs, identifier)
					&& let Some(key) = dig!(lhs, string(1).string_content(1))
					&& let Some(rhs) = python_next_named_sibling(lhs)
					&& let type_ = self.type_of(rhs, scope, contents)
					&& let Some(Type::DictBag(properties)) = scope.variables.get_mut(&contents[map.byte_range()])
				{
					let type_ = type_.unwrap_or_else(|| _T!(Type::Value));
					let key = &contents[key.byte_range()];
					if let Some(idx) = properties.iter().position(|(prop, _)| match prop {
						DictKey::String(prop) => prop.as_str() == key,
						DictKey::Type(_) => false,
					}) {
						properties[idx].1 = type_;
					} else {
						properties.push((DictKey::String(ImStr::from(key)), type_));
					}
				} else if lhs.kind() == "pattern_list"
					&& let Some(rhs) = python_next_named_sibling(lhs)
					&& let Some(type_) = self.type_of(rhs, scope, contents)
				{
					self.destructure_into_patternlist_like(lhs, type_, scope, contents);
				}
			}
			"for_statement" => {
				// (for_statement left right body)
				scope.enter(true);
				let lhs = node.named_child(0).unwrap();

				if let Some(rhs) = python_next_named_sibling(lhs)
					&& let Some(type_) = self.type_of(rhs, scope, contents)
					&& let Some(inner) = self.type_of_iterable(type_cache().resolve(type_))
				{
					self.destructure_into_patternlist_like(lhs, inner, scope, contents);
				}
				return ControlFlow::Continue(true);
			}
			"function_definition" => {
				let mut inherit_super = false;
				let mut node = node;
				while let Some(parent) = node.parent() {
					match parent.kind() {
						"decorated_definition" => {
							node = parent;
							continue;
						}
						"block" => inherit_super = parent.parent().is_some_and(|gp| gp.kind() == "class_definition"),
						_ => {}
					}
					break;
				}
				scope.enter(inherit_super);
				return ControlFlow::Continue(true);
			}
			"list_comprehension" | "set_comprehension" | "dictionary_comprehension" | "generator_expression"
				if node.byte_range().contains(&offset) =>
			{
				// (_ body: _ (for_in_clause left: _ right: _))
				let for_in = node.named_child(1).unwrap();
				if let Some(lhs) = for_in.child_by_field_name("left")
					&& let Some(rhs) = for_in.child_by_field_name("right")
					&& let Some(tid) = self.type_of(rhs, scope, contents)
					&& let Some(inner) = self.type_of_iterable(type_cache().resolve(tid))
				{
					self.destructure_into_patternlist_like(lhs, inner, scope, contents);
				}
			}
			"call" if node.byte_range().contains_end(offset) => {
				// model.{mapped,filtered,sorted,*}(lambda rec: ..)
				let query = MappedCall::query();
				let mut cursor = QueryCursor::new();
				let mut matches = cursor.matches(query, node, contents.as_bytes());
				if let Some(mapped_call) = matches.next()
					&& let callee = mapped_call
						.nodes_for_capture_index(MappedCall::Callee as _)
						.next()
						.unwrap() && let Some(tid) = self.type_of(callee, scope, contents)
				{
					let iter = mapped_call
						.nodes_for_capture_index(MappedCall::Iter as _)
						.next()
						.unwrap();
					let iter = &contents[iter.byte_range()];
					scope.insert(iter.to_string(), type_cache().resolve(tid));
				}
			}
			"call" => {
				let query = PythonBuiltinCall::query();
				let mut cursor = QueryCursor::new();
				let mut matches = cursor.matches(query, node, contents.as_bytes());
				let Some(call) = matches.next() else {
					return ControlFlow::Continue(false);
				};
				if let Some(value) = call.nodes_for_capture_index(PythonBuiltinCall::AppendValue as _).next()
					&& let Some(tid) = self.type_of(value, scope, contents)
				{
					if let Some(list) = call.nodes_for_capture_index(PythonBuiltinCall::AppendList as _).next() {
						if let Some(Type::List(slot @ ListElement::Vacant)) =
							scope.variables.get_mut(&contents[list.byte_range()])
						{
							*slot = ListElement::Occupied(tid);
						}
					} else if let Some(map) = call.nodes_for_capture_index(PythonBuiltinCall::AppendMap as _).next() {
						let Some(key) = call
							.nodes_for_capture_index(PythonBuiltinCall::AppendMapKey as _)
							.next()
						else {
							return ControlFlow::Continue(false);
						};
						let key = &contents[key.byte_range()];

						if let Some(Type::DictBag(properties)) = scope.variables.get_mut(&contents[map.byte_range()])
							&& let Some((_, slot)) = properties.iter_mut().find(|(prop, id)| match prop {
								DictKey::String(prop) => {
									prop.as_str() == key && _T!(Type::List(ListElement::Vacant)) == *id
								}
								DictKey::Type(_) => false,
							}) {
							*slot = _T!(Type::List(ListElement::Occupied(tid)));
						}
					}
				} else if let Some(map) = call.nodes_for_capture_index(PythonBuiltinCall::UpdateMap as _).next() {
					let Some(Type::DictBag(properties)) = scope.variables.get_mut(&contents[map.byte_range()]) else {
						return ControlFlow::Continue(false);
					};
					let Some(args) = call.nodes_for_capture_index(PythonBuiltinCall::UpdateArgs as _).next() else {
						return ControlFlow::Continue(false);
					};

					let mut properties = core::mem::take(properties);
					let mut cursor = args.walk();
					let mut children = args.named_children(&mut cursor);
					if let Some(first) = children.by_ref().next()
						&& let Some(tid) = self.type_of(first, scope, contents)
						&& let Type::DictBag(update_props) = type_cache().resolve(tid)
					{
						properties.extend(update_props);
					}

					for named_arg in children {
						if named_arg.kind() == "keyword_argument"
							&& let Some(name) = named_arg.child_by_field_name("name")
							&& let Some(value) = named_arg.child_by_field_name("value")
						{
							let key = &contents[name.byte_range()];
							let type_ = self.type_of(value, scope, contents).unwrap_or_else(|| _T!(Type::Value));
							if let Some(idx) = properties.iter().position(|(prop, _)| match prop {
								DictKey::String(prop) => prop.as_str() == key,
								DictKey::Type(_) => false,
							}) {
								properties[idx].1 = type_;
							} else {
								properties.push((DictKey::String(ImStr::from(key)), type_));
							}
						} else if named_arg.kind() == "dictionary_splat"
							&& let Some(value) = named_arg.named_child(0)
							&& let Some(tid) = self.type_of(value, scope, contents)
							&& let Type::DictBag(update_props) = type_cache().resolve(tid)
						{
							properties.extend(update_props);
						}
					}

					scope
						.variables
						.insert(contents[map.byte_range()].to_string(), Type::DictBag(properties));
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
				if let Some(value) = dig!(node, with_clause.with_item.as_pattern.call)
					&& let Some(target) = python_next_named_sibling(value)
					&& target.kind() == "as_pattern_target"
					&& let Some(alias) = dig!(target, identifier)
					&& let Some(callee) = value.named_child(0)
				{
					// TODO: Remove this hardcoded case
					if callee.kind() == "identifier"
						&& "Form" == &contents[callee.byte_range()]
						&& let Some(first_arg) = value.named_child(1).expect("call args").named_child(0)
						&& let Some(type_) = self.type_of(first_arg, scope, contents)
					{
						let alias = &contents[alias.byte_range()];
						scope.insert(alias.to_string(), type_cache().resolve(type_));
					} else if let Some(type_) = self.type_of(value, scope, contents) {
						let alias = &contents[alias.byte_range()];
						scope.insert(alias.to_string(), type_cache().resolve(type_));
					}
				}
			}
			_ => {}
		}

		ControlFlow::Continue(false)
	}
	/// [Type::Value] is not returned by this method.
	pub fn type_of(&self, mut node: Node, scope: &Scope, contents: &str) -> Option<TypeId> {
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
		#[cfg(debug_assertions)]
		if node.byte_range().len() <= 64 {
			tracing::trace!("type_of {} '{}'", node.kind(), &contents[node.byte_range()]);
		} else {
			tracing::trace!("type_of {} range={:?}", node.kind(), node.byte_range());
		}
		match normalize(&mut node).kind() {
			"subscript" => {
				let lhs = node.child_by_field_name("value")?;
				let rhs = node.child_by_field_name("subscript")?;
				let obj_ty = self.type_of(lhs, scope, contents)?;
				match type_cache().resolve(obj_ty) {
					Type::Env if rhs.kind() == "string" => {
						Some(_T!(Type::Model(contents[rhs.byte_range().shrink(1)].into())))
					}
					Type::Env => Some(_T!["unknown"]),
					Type::Model(_) | Type::Record(_) => Some(obj_ty),
					Type::Dict(key, value) => {
						let rhs = self.type_of(rhs, scope, contents);
						// FIXME: We trust that the user makes the correct judgment here and returns the type requested.
						rhs.is_none_or(|rhs| rhs == key).then_some(value)
					}
					Type::DictBag(properties) => {
						// compare by key
						if let Some(rhs) = dig!(rhs, string_content(1)) {
							let rhs = &contents[rhs.byte_range()];
							for (key, value) in properties {
								match key {
									DictKey::String(key) if key.as_str() == rhs => {
										return Some(value);
									}
									DictKey::String(_) | DictKey::Type(_) => {}
								}
							}
							return None;
						}

						// compare by type
						let rhs = self.type_of(rhs, scope, contents)?;
						for (key, value) in properties {
							match key {
								DictKey::Type(key) if key == rhs => return Some(value),
								DictKey::Type(_) | DictKey::String(_) => {}
							}
						}

						None
					}
					// FIXME: Again, just trust that the user is doing the right thing.
					Type::List(ListElement::Occupied(slot)) => Some(slot),
					_ => None,
				}
			}
			"attribute" => self.type_of_attribute_node(node, scope, contents),
			"identifier" => {
				if let Some(parent) = node.parent()
					&& parent.kind() == "attribute"
					&& parent.named_child(0).unwrap() != node
				{
					return self.type_of_attribute_node(parent, scope, contents);
				}

				let key = &contents[node.byte_range()];
				if key == "super" {
					return Some(_T!(Type::Super));
				}
				if let Some(type_) = scope.get(key) {
					return Some(_T!(type_.clone()));
				}
				if key == "request" {
					return Some(_T!(Type::HttpRequest));
				}
				None
			}
			"assignment" => {
				let rhs = node.named_child(1)?;
				self.type_of(rhs, scope, contents)
			}
			"call" => self.type_of_call_node(node, scope, contents),
			"binary_operator" | "boolean_operator" => {
				// (_ left right)
				if let Some(left) = node.child_by_field_name("left")
					&& let Some(typ) = self.type_of(left, scope, contents)
				{
					Some(typ)
				} else {
					self.type_of(node.child_by_field_name("right")?, scope, contents)
				}
			}
			"conditional_expression" => {
				// a if b else c
				let ty = node
					.named_child(0)
					.and_then(|child| self.type_of(child, scope, contents));
				ty.or_else(|| {
					node.named_child(2)
						.and_then(|child| self.type_of(child, scope, contents))
				})
			}
			"dictionary_comprehension" => {
				let pair = dig!(node, pair)?;
				let mut comprehension_scope;
				let mut pair_scope = scope;
				if let Some(for_in_clause) = dig!(node, for_in_clause(1))
					&& let Some(scrutinee) = for_in_clause.child_by_field_name("left")
					&& let Some(iteratee) = for_in_clause.child_by_field_name("right")
					&& let Some(iter_ty) = self.type_of(iteratee, scope, contents)
					&& let Some(iter_ty) = self.type_of_iterable(type_cache().resolve(iter_ty))
				{
					// FIXME: How to prevent this clone?
					comprehension_scope = Scope::new(Some(scope.clone()));
					self.destructure_into_patternlist_like(scrutinee, iter_ty, &mut comprehension_scope, contents);
					pair_scope = &comprehension_scope;
				}
				let lhs = pair
					.named_child(0)
					.and_then(|lhs| self.type_of(lhs, pair_scope, contents));
				let rhs = pair
					.named_child(1)
					.and_then(|lhs| self.type_of(lhs, pair_scope, contents));
				if lhs.is_some() || rhs.is_some() {
					let value_id = _T!(Type::Value);
					Some(_T!(Type::Dict(lhs.unwrap_or(value_id), rhs.unwrap_or(value_id))))
				} else {
					None
				}
			}
			"dictionary" => {
				let mut properties = vec![];
				for child in node.named_children(&mut node.walk()) {
					if child.kind() == "pair"
						&& let Some(lhs) = child.child_by_field_name("key")
						&& let Some(rhs) = child.child_by_field_name("value")
					{
						let key;
						if let Some(lhs) = dig!(lhs, string_content(1)) {
							key = DictKey::String(ImStr::from(&contents[lhs.byte_range()]));
						} else if matches!(lhs.kind(), "true" | "false" | "string" | "none" | "float" | "integer") {
							key = DictKey::Type(_T!( @contents[lhs.byte_range()]));
						} else if let Some(lhs) = self.type_of(lhs, scope, contents) {
							key = DictKey::Type(lhs);
						} else {
							continue;
						}

						let value = self.type_of(rhs, scope, contents).unwrap_or_else(|| _T!(Type::Value));
						properties.push((key, value));
					}
				}
				Some(_T!(Type::DictBag(properties)))
			}
			"list" => {
				let mut slot = ListElement::Vacant;
				for child in node.named_children(&mut node.walk()) {
					if let Some(child) = self.type_of(child, scope, contents) {
						slot = ListElement::Occupied(child);
						break;
					}
				}
				Some(_T!(Type::List(slot)))
			}
			"expression_list" | "tuple" => {
				let mut cursor = node.walk();
				let value_id = _T!(Type::Value);
				let tuple = node.named_children(&mut cursor).filter_map(|child| {
					if child.kind() == "comment" {
						return None;
					}
					Some(self.type_of(child, scope, contents).unwrap_or(value_id))
				});
				Some(_T!(Type::Tuple(tuple.collect())))
			}
			"string" => Some(_T!( @ "str")),
			"integer" => Some(_T!( @ "int")),
			"float" => Some(_T!( @ "float")),
			"true" | "false" | "comparison_operator" => Some(_T!( @ "bool")),
			_ => None,
		}
	}
	fn type_of_iterable(&self, type_: Type) -> Option<TypeId> {
		match type_ {
			Type::Model(_) => Some(_T!(type_)),
			Type::List(inner) => inner.into(),
			Type::Iterable(inner) => inner,
			// TODO: tuple -> union
			_ => None,
		}
	}
	fn wrap_in_container<F: FnOnce(Type) -> Type>(type_: Type, producer: F) -> Type {
		match type_ {
			Type::Model(..) => type_,
			_ => producer(type_),
		}
	}
	fn type_of_call_node(&self, call: Node<'_>, scope: &Scope, contents: &str) -> Option<TypeId> {
		let func = call.named_child(0)?;
		if func.kind() == "identifier" {
			match &contents[func.byte_range()] {
				"zip" => {
					let args = call.named_child(1)?;
					let mut cursor = args.walk();
					let value_id = _T!(Type::Value);
					let children = args.named_children(&mut cursor).map(|child| {
						let type_ = type_cache().resolve(self.type_of(child, scope, contents).unwrap_or(value_id));
						self.type_of_iterable(type_).unwrap_or(value_id)
					});
					let tuple = _T!(Type::Tuple(children.collect()));
					return Some(_T!(Type::Iterable(Some(tuple))));
				}
				"enumerate" => {
					let arg = call.named_child(1)?.named_child(0);
					let arg = arg
						.and_then(|arg| self.type_of(arg, scope, contents))
						.unwrap_or_else(|| _T!(Type::Value));
					let intid = _T!(Type::PyBuiltin("int".into()));
					let tuple = _T!(Type::Tuple(vec![intid, arg]));
					return Some(_T!(Type::Iterable(Some(tuple))));
				}
				"tuple" => {
					let args = call.named_child(1)?;
					if args.kind() == "argument_list" {
						let mut cursor = args.walk();
						let value_id = _T!(Type::Value);
						let children = args
							.named_children(&mut cursor)
							.map(|child| self.type_of(child, scope, contents).unwrap_or(value_id));
						return Some(_T!(Type::Tuple(children.collect())));
					}
				}
				"super" => {}
				_ => return None,
			};
		}

		let func = self.type_of(func, scope, contents)?;
		match type_cache().resolve(func) {
			Type::RefFn => {
				// (call (_) @func (argument_list . (string) @xml_id))
				let xml_id = call.named_child(1)?.named_child(0)?;
				if xml_id.kind() == "string" {
					Some(_T!(Type::Record(contents[xml_id.byte_range().shrink(1)].into())))
				} else {
					None
				}
			}
			Type::ModelFn(model) => Some(_T!(Type::Model(model))),
			Type::Super => Some(_T!(scope.get(scope.super_.as_deref()?).cloned()?)),
			Type::Method(model, mapped) if mapped == "mapped" => {
				// (call (_) @func (argument_list . [(string) (lambda)] @mapped))
				let mapped = call.named_child(1)?.named_child(0)?;
				match mapped.kind() {
					"string" => {
						let mut model: Spur = model.into();
						let mut mapped = &contents[mapped.byte_range().shrink(1)];
						self.models.resolve_mapped(&mut model, &mut mapped, None).ok()?;
						let type_ = self.type_of_attribute(&Type::Model(_R(model).into()), mapped, scope)?;
						let type_ = Index::wrap_in_container(type_, |it| Type::List(ListElement::Occupied(_T!(it))));
						Some(_T!(type_))
					}
					"lambda" => {
						// (lambda (lambda_parameters)? body: (_))
						let mut scope = Scope::new(Some(scope.clone()));
						if let Some(params) = mapped.child_by_field_name(b"parameters") {
							let first_arg = params.named_child(0)?;
							if first_arg.kind() == "identifier" {
								let first_arg = &contents[first_arg.byte_range()];
								scope.insert(first_arg.to_string(), Type::Model(_R(model).into()));
							}
						}
						let body = mapped.child_by_field_name(b"body")?;
						let type_ = self.type_of(body, &scope, contents).unwrap_or_else(|| _T!(Type::Value));
						let type_ = Index::wrap_in_container(type_cache().resolve(type_), |it| {
							Type::List(ListElement::Occupied(_T!(it)))
						});
						Some(_T!(type_))
					}
					_ => None,
				}
			}
			Type::Method(model, grouped) if grouped == "grouped" => {
				// (call (_) @func (argument_list . [(string) (lambda)] @mapped))
				let grouped = call.named_child(1)?.named_child(0)?;
				match grouped.kind() {
					"string" => {
						let mut model: Spur = model.into();
						let mut grouped = &contents[grouped.byte_range().shrink(1)];
						self.models.resolve_mapped(&mut model, &mut grouped, None).ok()?;
						let model = Type::Model(_R(model).into());
						let groupby = self.type_of_attribute(&model, grouped, scope)?;
						Some(_T!(Type::Dict(_T!(groupby), _T!(model))))
					}
					"lambda" => {
						let mut scope = Scope::new(Some(scope.clone()));
						if let Some(params) = grouped.child_by_field_name(b"parameters") {
							let first_arg = params.named_child(0)?;
							if first_arg.kind() == "identifier" {
								let first_arg = &contents[first_arg.byte_range()];
								scope.insert(first_arg.to_string(), Type::Model(_R(model).into()));
							}
						}
						let body = grouped.child_by_field_name(b"body")?;
						let groupby = self.type_of(body, &scope, contents).unwrap_or_else(|| _T!(Type::Value));
						let model = Type::Model(_R(model).into());
						Some(_T!(Type::Dict(groupby, _T!(model))))
					}
					_ => None,
				}
			}
			Type::Method(model, read_group) if read_group == "_read_group" => {
				let mut groupby = vec![];
				let mut aggs = vec![];
				let args = call.named_child(1)?;

				fn gather_attributes<'out>(contents: &'out str, arg: Node, out: &mut Vec<&'out str>) {
					let mut cursor = arg.walk();
					for field in arg.named_children(&mut cursor) {
						if let Some(field) = dig!(field, string_content(1)) {
							let mut field = &contents[field.byte_range()];
							if let Some((inner, _)) = field.split_once(':') {
								field = inner;
							}
							out.push(field);
						}
					}
				}

				for (idx, arg) in args.named_children(&mut args.walk()).enumerate().take(3) {
					if arg.kind() == "keyword_argument" {
						let out = match &contents[arg.child_by_field_name("key")?.byte_range()] {
							"groupby" => &mut groupby,
							"aggregates" => &mut aggs,
							_ => continue,
						};
						let Some(arg) = arg.child_by_field_name("value") else {
							continue;
						};
						if arg.kind() != "list" {
							continue;
						}
						gather_attributes(contents, arg, out);
						continue;
					}

					if arg.kind() != "list" || idx > 2 || idx == 0 {
						continue;
					}

					let out = if idx == 1 { &mut groupby } else { &mut aggs };
					gather_attributes(contents, arg, out);
				}

				groupby.extend(aggs);
				groupby.dedup();
				let model = Type::Model(_R(model).into());
				let value_id = _T!(Type::Value);
				// FIXME: This is not quite correct as only recordset and numeric aggregations make sense.
				let aggs = groupby
					.into_iter()
					.map(|attr| match self.type_of_attribute(&model, attr, scope) {
						Some(type_) => _T!(type_),
						None => value_id,
					});
				let tuple = _T!(Type::Tuple(aggs.collect()));
				Some(_T!(Type::List(ListElement::Occupied(tuple))))
			}
			Type::Method(model, method) => {
				let method = _G(&method)?;
				let args = self.prepare_call_scope(model, method.into(), call, scope, contents);
				Some(self.eval_method_rtype(method.into(), *model, args)?)
			}
			Type::PythonMethod(dict, items) if type_cache().is_dict(dict) && items == "items" => {
				let Type::Dict(lhs, rhs) = type_cache().resolve(dict) else {
					unreachable!()
				};
				let tuple = _T!(Type::Tuple(vec![lhs, rhs]));
				Some(_T!(Type::Iterable(Some(tuple))))
			}
			Type::Env
			| Type::Record(..)
			| Type::Model(..)
			| Type::HttpRequest
			| Type::Value
			| Type::PyBuiltin(..)
			| Type::Dict(..)
			| Type::DictBag(..)
			| Type::List(..)
			| Type::Iterable(..)
			| Type::Tuple(..)
			| Type::PythonMethod(..) => None,
		}
	}

	#[instrument(skip_all, fields(model, method))]
	fn prepare_call_scope(
		&self,
		model: ModelName,
		method: Symbol<Method>,
		call: Node,
		scope: &Scope,
		contents: &str,
	) -> Option<(Vec<ImStr>, Scope)> {
		// (call
		//   (arguments_list
		//     (_)
		//     (keyword_argument (identifier) (_))))
		let arguments_list = dig!(call, argument_list(1))?;

		let model = self.models.populate_properties(model, &[])?;
		let method = model.methods.as_ref()?.get(&method)?;
		let arguments = method.arguments.clone().unwrap_or_default();
		if arguments.is_empty() {
			return None;
		}

		drop(model);
		let mut argtypes = Scope::new(None);
		let mut args = vec![];
		for (idx, arg) in arguments_list.named_children(&mut arguments_list.walk()).enumerate() {
			if arg.kind() == "keyword_argument"
				&& let Some(key) = arg.child_by_field_name("key")
				&& let Some(value) = arg.child_by_field_name("value")
			{
				let key = &contents[key.byte_range()];
				if !arguments.iter().any(|arg| match arg {
					FunctionParam::Named(arg) => arg.as_str() == key,
					_ => false,
				}) {
					continue;
				}
				let Some(tid) = self.type_of(value, scope, contents) else {
					continue;
				};
				args.push(key.into());
				argtypes.insert(key.to_string(), type_cache().resolve(tid));
			} else if let Some(FunctionParam::Param(argname)) = arguments.get(idx)
				&& let Some(tid) = self.type_of(arg, scope, contents)
			{
				args.push(argname.clone());
				argtypes.insert(argname.to_string(), type_cache().resolve(tid));
			} else {
				continue;
			}
		}

		Some((args, argtypes))
	}
	#[instrument(skip_all, ret)]
	fn type_of_attribute_node(&self, attribute: Node<'_>, scope: &Scope, contents: &str) -> Option<TypeId> {
		let lhs = attribute.named_child(0)?;
		let lhsid = self.type_of(lhs, scope, contents)?;
		let lhs = type_cache().resolve(lhsid);
		let rhs = attribute.named_child(1)?;
		let attrname = &contents[rhs.byte_range()];
		match &contents[rhs.byte_range()] {
			"env" if matches!(lhs, Type::Model(..) | Type::Record(..) | Type::HttpRequest) => Some(Type::Env),
			"website" if matches!(lhs, Type::HttpRequest) => Some(Type::Model("website".into())),
			"ref" if matches!(lhs, Type::Env) => Some(Type::RefFn),
			"user" if matches!(lhs, Type::Env) => Some(Type::Model("res.users".into())),
			"company" | "companies" if matches!(lhs, Type::Env) => Some(Type::Model("res.company".into())),
			"mapped" | "grouped" | "_read_group" => {
				let model = self.try_resolve_model(&lhs, scope)?;
				Some(Type::Method(model, attrname.into()))
			}
			dict_method @ "items" if lhs.is_dict() => Some(Type::PythonMethod(lhsid, dict_method.into())),
			func if MODEL_METHODS.contains(func) => match lhs {
				Type::Model(model) => Some(Type::ModelFn(model)),
				Type::Record(xml_id) => {
					let xml_id = _G(xml_id)?;
					let record = self.records.get(&xml_id.into())?;
					Some(Type::ModelFn(_R(*record.model.as_deref()?).into()))
				}
				_ => None,
			},
			ident if rhs.kind() == "identifier" => self.type_of_attribute(&lhs, ident, scope),
			_ => None,
		}
		.map(|it| _T!(it))
	}
	#[instrument(skip_all, fields(attr=attr), ret)]
	pub fn type_of_attribute(&self, type_: &Type, attr: &str, scope: &Scope) -> Option<Type> {
		let model = self.try_resolve_model(type_, scope)?;
		let model_entry = self.models.populate_properties(model, &[])?;
		if let Some(attr_key) = _G(attr)
			&& let Some(attr_kind) = model_entry.prop_kind(attr_key)
		{
			match attr_kind {
				PropertyInfo::Field(type_) => {
					drop(model_entry);
					if let Some(relation) = self.models.resolve_related_field(attr_key.into(), model.into()) {
						return Some(Type::Model(_R(relation).into()));
					}

					match _R(type_) {
						"Selection" | "Char" | "Text" | "Html" => Some(Type::PyBuiltin("str".into())),
						"Integer" => Some(Type::PyBuiltin("int".into())),
						"Float" | "Monetary" => Some(Type::PyBuiltin("float".into())),
						"Date" => Some(Type::PyBuiltin("date".into())),
						"Datetime" => Some(Type::PyBuiltin("datetime".into())),
						_ => None,
					}
				}
				PropertyInfo::Method => Some(Type::Method(model, attr.into())),
			}
		} else {
			match attr {
				"id" if matches!(type_, Type::Model(..) | Type::Record(..)) => Some(Type::PyBuiltin("int".into())),
				"ids" if matches!(type_, Type::Model(..) | Type::Record(..)) => {
					Some(Type::List(ListElement::Occupied(_T!(Type::PyBuiltin("int".into())))))
				}
				"display_name" if matches!(type_, Type::Model(..) | Type::Record(..)) => {
					Some(Type::PyBuiltin("str".into()))
				}
				"create_date" | "write_date" if matches!(type_, Type::Model(..) | Type::Record(..)) => {
					Some(Type::PyBuiltin("datetime".into()))
				}
				"create_uid" | "write_uid" if matches!(type_, Type::Model(..) | Type::Record(..)) => {
					Some(Type::Model("res.users".into()))
				}
				"_fields" if matches!(type_, Type::Model(..) | Type::Record(..)) => {
					Some(Type::Dict(_T!(Type::PyBuiltin("str".into())), _T!["ir.model.fields"]))
				}
				"env" if matches!(type_, Type::Model(..) | Type::Record(..) | Type::HttpRequest) => Some(Type::Env),
				_ => None,
			}
		}
	}
	pub fn has_attribute(&self, type_: &Type, attr: &str, scope: &Scope) -> bool {
		(|| -> Option<()> {
			let model = self.try_resolve_model(type_, scope)?;
			let entry = self.models.populate_properties(model, &[])?;
			let attr = _G(attr)?;
			entry.prop_kind(attr).map(|_| ())
		})()
		.is_some()
	}
	/// Call this method if it's unclear whether `type_` is a [`Type::Model`] and you just want the model's name.
	pub fn try_resolve_model(&self, type_: &Type, scope: &Scope) -> Option<ModelName> {
		match type_ {
			Type::Model(model) => Some(_G(model)?.into()),
			Type::Record(xml_id) => {
				// TODO: Refactor into method
				let xml_id = _G(xml_id)?;
				let record = self.records.get(&xml_id.into())?;
				record.model
			}
			Type::Super => self.try_resolve_model(scope.get(scope.super_.as_deref()?)?, scope),
			_ => None,
		}
	}
	#[inline]
	pub fn type_display(&self, type_: TypeId) -> Option<String> {
		self.type_display_indent(type_, 0)
	}
	fn type_display_indent(&self, type_: TypeId, indent: usize) -> Option<String> {
		match type_cache().resolve(type_) {
			Type::Dict(lhs, rhs) => {
				let lhs = self.type_display_indent(lhs, indent);
				let lhs = lhs.as_deref().unwrap_or("...");
				let rhs = self.type_display_indent(rhs, indent);
				let rhs = rhs.as_deref().unwrap_or("...");
				Some(fomat! { "dict[" (lhs) ", " (rhs) "]" })
			}
			Type::DictBag(properties) => {
				let preindent = " ".repeat(indent + 2);
				let empty_properties = properties.is_empty();
				let properties_fragment = fomat! {
					for (key, value) in properties {
						(preindent)
						match key {
							DictKey::String(key) => { "\"" (key) "\"" }
							DictKey::Type(key) if type_cache().is_dictlike(key) => { "{...}" }
							DictKey::Type(key) => { (self.type_display_indent(key, indent + 2).as_deref().unwrap_or("...")) }
						} ": " (self.type_display_indent(value, indent + 2).as_deref().unwrap_or("..."))
					} sep { ",\n" }
				};
				let unindent = " ".repeat(indent);
				Some(fomat! {
					if !empty_properties {
						"{\n" (properties_fragment) "\n" (unindent) "}"
					} else {
						"{}"
					}
				})
			}
			Type::PyBuiltin(builtin) => Some(builtin.as_str().into()),
			Type::List(slot) => {
				let slot = match slot {
					ListElement::Vacant => None,
					ListElement::Occupied(slot) => self.type_display_indent(slot, indent),
				};
				Some(match slot {
					Some(slot) => format!("list[{slot}]"),
					None => "list".into(),
				})
			}
			Type::Env => Some("Environment".into()),
			Type::Model(model) => Some(format!(r#"Model["{model}"]"#)),
			Type::Record(xml_id) => {
				let xml_id = _G(xml_id)?;
				let record = self.records.get(&xml_id.into())?;
				Some(_R(record.model?).into())
			}
			Type::Tuple(items) => Some(fomat! {
				"tuple["
				for item in items {
					(self.type_display_indent(item, indent).as_deref().unwrap_or("..."))
				} sep { ", " }
				"]"
			}),
			Type::Iterable(output) => {
				let output = output.and_then(|inner| self.type_display_indent(inner, indent));
				let output = output.as_deref().unwrap_or("...");
				Some(format!("Iterable[{output}]"))
			}
			Type::Method(..) => unreachable!("Bug: this function should not handle methods"),
			Type::RefFn | Type::ModelFn(_) | Type::Super | Type::HttpRequest | Type::Value | Type::PythonMethod(..) => {
				if cfg!(debug_assertions) {
					Some(format!("{type_:?}"))
				} else {
					None
				}
			}
		}
	}
	/// Iterates depth-first over `node` using [`PreTravel`]. Automatically calls [`Scope::exit`] at suitable points.
	///
	/// [`ControlFlow::Continue`] accepts a boolean to indicate whether [`Scope::enter`] was called.
	///
	/// To accumulate bindings into a scope, use [`Index::build_scope`].
	pub fn walk_scope<T>(
		node: Node,
		scope: Option<Scope>,
		mut step: impl FnMut(&mut Scope, Node) -> ControlFlow<Option<T>, bool>,
	) -> (Scope, Option<T>) {
		let mut scope = scope.unwrap_or_default();
		let mut scope_ends = vec![];
		for node in PreTravel::new(node) {
			if !node.is_named() {
				continue;
			}
			if let Some(&end) = scope_ends.last()
				&& node.start_byte() > end
			{
				scope.exit();
				scope_ends.pop();
			}
			match step(&mut scope, node) {
				ControlFlow::Break(value) => return (scope, value),
				ControlFlow::Continue(entered) => {
					if entered {
						scope_ends.push(node.end_byte());
					}
				}
			}
		}
		(scope, None)
	}
	/// Resolves the return type of a method as well as populating its arguments and docstring.
	///
	/// `parameters` can be provided using [`Index::prepare_call_scope`].  
	#[instrument(level = "trace", ret, skip(self, model), fields(model = _R(model)))]
	pub fn eval_method_rtype(
		&self,
		method: Symbol<Method>,
		model: Spur,
		parameters: Option<(Vec<ImStr>, Scope)>,
	) -> Option<TypeId> {
		_ = self.models.populate_properties(model.into(), &[]);
		let mut model_entry = self.models.try_get_mut(&model.into()).expect(format_loc!("deadlock"))?;
		let method_obj = model_entry.methods.as_mut()?.get_mut(&method)?;

		if method_obj
			.pending_eval
			.compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
			.is_err()
		{
			return None;
		}

		let _guard = Defer(Some(|| {
			if let Some(model_entry) = self.models.get_mut(&model.into())
				&& let Some(methods) = model_entry.methods.as_ref()
				&& let Some(method) = methods.get(&method)
			{
				method.pending_eval.store(false, Ordering::Relaxed);
			}
		}));

		let (argnames, mut scope) = parameters.unwrap_or_default();
		let cache_key = argnames
			.into_iter()
			.map(|arg| _T!(scope.variables.get(&*arg).cloned().unwrap_or(Type::Value)))
			.collect::<Vec<_>>();
		if let Some(tid) = method_obj.eval_cache.get(&cache_key) {
			drop(model_entry);
			return Some(tid);
		}

		let location = method_obj.locations.first().cloned()?;
		drop(model_entry);

		let ast;
		let contents;
		let end_offset: ByteOffset;
		let path = location.path.to_path();
		if let Some(cached) = self.ast_cache.get(&path) {
			end_offset = rope_conv(location.range.end, cached.rope.slice(..));
			ast = cached.tree.clone();
			contents = String::from(cached.rope.clone());
		} else {
			contents = test_utils::fs::read_to_string(location.path.to_path()).unwrap();
			let rope = Rope::from_str(&contents);
			end_offset = rope_conv(location.range.end, rope.slice(..));
			let mut parser = Parser::new();
			parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
			ast = parser.parse(contents.as_bytes(), None)?;
			self.ast_cache.insert(
				path,
				Arc::new(crate::index::AstCacheItem {
					tree: ast.clone(),
					rope,
				}),
			);
		}

		// TODO: Improve this heuristic
		fn is_toplevel_return(mut node: Node) -> bool {
			while node.kind() != "function_definition" {
				tracing::trace!("{}", node.kind());
				match node.parent() {
					Some(parent) => node = parent,
					None => return false,
				}
			}

			fn is_block_of_class(node: Node) -> bool {
				node.kind() == "block" && node.parent().is_some_and(|parent| parent.kind() == "class_definition")
			}

			if let Some(decoration) = node.parent()
				&& decoration.kind() == "decorated_definition"
			{
				return decoration.parent().is_some_and(is_block_of_class);
			}

			node.parent().is_some_and(is_block_of_class)
		}

		let (self_type, fn_scope, self_param) = determine_scope(ast.root_node(), &contents, end_offset.0)?;
		let self_type = match self_type {
			Some(type_) => &contents[type_.byte_range().shrink(1)],
			None => "",
		};
		scope.super_ = Some(self_param.into());
		scope.insert(self_param.to_string(), Type::Model(self_type.into()));
		let offset = fn_scope.end_byte();
		let (_, type_) = Self::walk_scope(fn_scope, Some(scope), |scope, node| {
			let entered = self.build_scope(scope, node, offset, &contents).map_break(|_| None)?;
			// TODO: When implementing freestanding functions, make the toplevel check optional
			if node.kind() == "return_statement" && is_toplevel_return(node) {
				let Some(child) = node.named_child(0) else {
					return ControlFlow::Continue(entered);
				};
				let Some(type_) = self.type_of(child, scope, &contents) else {
					return ControlFlow::Continue(entered);
				};

				let type_ = type_cache().resolve(type_);
				return match self.try_resolve_model(&type_, scope) {
					Some(resolved) => ControlFlow::Break(Some(Type::Model(ImStr::from(_R(resolved))))),
					None => ControlFlow::Break(Some(type_)),
				};
			}

			ControlFlow::Continue(entered)
		});

		let mut model = self.models.try_get_mut(&model.into()).expect(format_loc!("deadlock"))?;
		let method = Arc::make_mut(model.methods.as_mut()?.get_mut(&method)?);

		let docstring = Self::parse_method_docstring(fn_scope, &contents)
			.map(|doc| ImStr::from(Method::postprocess_docstring(doc)));
		method.docstring = docstring;

		if let Some(params) = fn_scope.child_by_field_name("parameters") {
			// python parameters can be delineated by three separators:
			// - parameters before `/` are positional only (`positional_separator`)
			// - parameters between `/` and `*` can be either positional or named
			// - a catch-all positional `*args` (`keyword_separator` or `list_splat_pattern`)
			// - parameters after `*` and before `**` are named (`default_parameter`)
			// - a catch-all named `**kwargs` (`dictionary_splat_pattern` only)
			let mut cursor = params.walk();
			let args = params.named_children(&mut cursor).skip(1).filter_map(|param| {
				Some(match param.kind() {
					"identifier" => FunctionParam::Param(ImStr::from(&contents[param.byte_range()])),
					"positional_separator" => FunctionParam::PosEnd,
					"keyword_separator" => FunctionParam::EitherEnd(None),
					"list_splat_pattern" => {
						FunctionParam::EitherEnd(Some(ImStr::from(&contents[param.named_child(0)?.byte_range()])))
					}
					"dictionary_splat_pattern" => FunctionParam::Kwargs("kwargs".into()),
					"default_parameter" => {
						let name = param.named_child(0)?;
						let name = &contents[name.byte_range()];
						FunctionParam::Named(ImStr::from(name))
					}
					_ => return None,
				})
			});
			method.arguments = Some(args.collect());
		}

		method.pending_eval.store(false, Ordering::Release);
		if let Some(type_) = type_ {
			let tid = _T!(type_);
			method.eval_cache.insert(cache_key, tid);
			Some(tid)
		} else {
			None
		}
	}
	/// `pattern` is `(identifier | pattern_list | tuple_pattern)`, the `a, b` in `for a, b in ...`.
	fn destructure_into_patternlist_like(&self, pattern: Node, tid: TypeId, scope: &mut Scope, contents: &str) {
		if pattern.kind() == "identifier" {
			let name = &contents[pattern.byte_range()];
			scope.insert(name.to_string(), type_cache().resolve(tid));
		} else if matches!(pattern.kind(), "pattern_list" | "tuple_pattern") {
			if let Type::Tuple(mut inner) = type_cache().resolve(tid) {
				inner.reverse();
				for child in pattern.named_children(&mut pattern.walk()) {
					if matches!(child.kind(), "identifier" | "tuple_pattern")
						&& let Some(type_) = inner.pop()
					{
						self.destructure_into_patternlist_like(child, type_, scope, contents);
					}
				}
			} else if let Some(inner) = self.type_of_iterable(type_cache().resolve(tid)) {
				// spread this type to all params
				for child in pattern.named_children(&mut pattern.walk()) {
					if matches!(child.kind(), "identifier" | "tuple_pattern") {
						self.destructure_into_patternlist_like(child, inner, scope, contents);
					}
				}
			}
		}
	}
	fn parse_method_docstring<'out>(fn_scope: Node, contents: &'out str) -> Option<&'out str> {
		let block = fn_scope.child_by_field_name("body")?;
		dig!(block, expression_statement.string.string_content(1)).map(|node| &contents[node.byte_range()])
	}
}

/// Returns `(self_type, fn_scope, self_param)`.
///
/// `fn_scope` is customarily a `function_definition` node.
#[instrument(level = "trace", skip_all, ret)]
pub fn determine_scope<'out, 'node>(
	node: Node<'node>,
	contents: &'out str,
	offset: usize,
) -> Option<(Option<Node<'node>>, Node<'node>, &'out str)> {
	let query = FieldCompletion::query();
	let mut self_type = None;
	let mut self_param = None;
	let mut fn_scope = None;
	let mut cursor = QueryCursor::new();
	let mut matches = cursor.matches(query, node, contents.as_bytes());
	'scoping: while let Some(match_) = matches.next() {
		// @class
		let class = match_.captures.first()?;
		if !class.node.byte_range().contains_end(offset) {
			continue;
		}
		for capture in match_.captures {
			match FieldCompletion::from(capture.index) {
				Some(FieldCompletion::Name) => {
					if self_type.is_none() {
						self_type = Some(capture.node);
					}
				}
				Some(FieldCompletion::SelfParam) => {
					self_param = Some(capture.node);
				}
				Some(FieldCompletion::Scope) => {
					if !capture.node.byte_range().contains_end(offset) {
						continue 'scoping;
					}
					fn_scope = Some(capture.node);
				}
				None => {}
			}
		}
		if fn_scope.is_some() {
			break;
		}
	}
	let fn_scope = fn_scope?;
	let self_param = &contents[self_param?.byte_range()];
	Some((self_type, fn_scope, self_param))
}

#[cfg(test)]
mod tests {
	use pretty_assertions::assert_eq;
	use ropey::Rope;
	use tower_lsp_server::ls_types::Position;
	use tree_sitter::{Parser, QueryCursor, StreamingIterator, StreamingIteratorMut};

	use crate::analyze::{FieldCompletion, Type, type_cache};
	use crate::index::_I;
	use crate::utils::{ByteOffset, acc_vec, rope_conv};
	use crate::{index::Index, test_utils::cases::foo::prepare_foo_index};

	#[test]
	fn test_field_completion() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
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
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.map(|capture| FieldCompletion::from(capture.index))
					.collect::<Vec<_>>()
			})
			.fold_mut(vec![], acc_vec);
		// Allow nested patterns
		let actual = actual.iter().map(Vec::as_slice).collect::<Vec<_>>();
		use FieldCompletion as T;
		assert!(
			matches!(
				&actual[..],
				[
					[None, None, Some(T::Name), Some(T::Scope), Some(T::SelfParam)],
					[None, None, Some(T::Name), Some(T::Scope), Some(T::SelfParam)]
				]
			),
			"{actual:?}"
		)
	}

	#[test]
	fn test_determine_scope() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let contents = r#"
class Foo(models.Model):
	_name = 'foo'
	def scope(self):
		pass
"#;
		let ast = parser.parse(contents, None).unwrap();
		let rope = Rope::from(contents);
		let fn_start: ByteOffset = rope_conv(Position { line: 3, character: 1 }, rope.slice(..));
		let fn_scope = ast
			.root_node()
			.named_descendant_for_byte_range(fn_start.0, fn_start.0)
			.unwrap();
		super::determine_scope(ast.root_node(), contents, fn_start.0)
			.unwrap_or_else(|| panic!("{}", fn_scope.to_sexp()));
	}

	#[test]
	fn test_resolve_method_returntype() {
		let index = Index {
			models: prepare_foo_index(),
			..Default::default()
		};

		assert_eq!(
			index.eval_method_rtype(_I("test").into(), _I("bar"), None),
			Some(type_cache().get_or_intern(Type::Model("foo".into())))
		)
	}

	#[test]
	fn test_super_analysis() {
		let index = Index {
			models: prepare_foo_index(),
			..Default::default()
		};

		assert_eq!(
			index.eval_method_rtype(_I("test").into(), _I("quux"), None),
			Some(type_cache().get_or_intern(Type::Model("foo".into())))
		)
	}
}
