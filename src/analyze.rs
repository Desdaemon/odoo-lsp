//! Methods related to type analysis. The two most important methods are
//! [`Index::model_of_range`] and [`Index::type_of`].

use std::{
	fmt::{Debug, Write},
	ops::ControlFlow,
	sync::Arc,
};

use lasso::Spur;
use ropey::Rope;
use tracing::instrument;
use tree_sitter::{Node, Parser, QueryCursor};

use crate::{
	ImStr, dig, format_loc,
	index::{_G, _I, _R, Index, Symbol},
	model::{Method, MethodReturnType, ModelEntry, ModelName, PropertyKind},
	test_utils,
	utils::{ByteRange, PreTravel, RangeExt, TryResultExt, lsp_range_to_offset_range},
};
use ts_macros::query;

mod scope;
pub use scope::Scope;

pub static MODEL_METHODS: phf::Set<&[u8]> = phf::phf_set!(
	b"create",
	b"copy",
	b"name_create",
	b"browse",
	b"filtered",
	b"filtered_domain",
	b"sorted",
	b"search",
	b"search_fetch",
	b"name_search",
	b"ensure_one",
	b"with_context",
	b"with_user",
	b"with_company",
	b"with_env",
	b"sudo",
	b"exists",
	// TODO: Limit to Forms only
	b"new",
	b"edit",
	b"save",
);

/// The subset of types that may resolve to a model.
#[derive(Clone, Debug)]
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
	/// `odoo.http.request`
	HttpRequest,
	/// Can never be resolved, useful for non-model bindings.
	Value,
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
  (#match? @_mapped "^(mapp|filter|sort)ed$"))
}

pub type ScopeControlFlow = ControlFlow<Option<Scope>, bool>;
impl Index {
	#[inline]
	pub fn model_of_range(&self, node: Node<'_>, range: ByteRange, contents: &[u8]) -> Option<ModelName> {
		let (type_at_cursor, scope) = self.type_of_range(node, range, contents)?;
		self.try_resolve_model(&type_at_cursor, &scope)
	}
	pub fn type_of_range(&self, root: Node<'_>, range: ByteRange, contents: &[u8]) -> Option<(Type, Scope)> {
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
			None => &[],
		};
		scope.super_ = Some(self_param.as_ref().into());
		scope.insert(
			self_param.into_owned(),
			Type::Model(String::from_utf8_lossy(self_type).as_ref().into()),
		);
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
				Type::Method(
					_I(String::from_utf8_lossy(self_type)).into(),
					String::from_utf8_lossy(&contents[node_at_cursor.byte_range()])
						.as_ref()
						.into(),
				),
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
	pub fn build_scope(&self, scope: &mut Scope, node: Node, offset: usize, contents: &[u8]) -> ScopeControlFlow {
		if node.start_byte() > offset {
			return ControlFlow::Break(Some(core::mem::take(scope)));
		}
		match node.kind() {
			"assignment" | "named_expression" => {
				// (_ left right)
				let lhs = node.named_child(0).unwrap();
				if lhs.kind() == "identifier"
					&& let rhs = lhs.next_named_sibling().expect(format_loc!("rhs"))
					&& let Some(type_) = self.type_of(rhs, scope, contents)
				{
					let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
					scope.insert(lhs.into_owned(), type_);
				}
			}
			"for_statement" => {
				// (for_statement left right body)
				scope.enter(true);
				let lhs = node.named_child(0).unwrap();
				if lhs.kind() == "identifier"
					&& let rhs = lhs.next_named_sibling().expect(format_loc!("rhs"))
					&& let Some(type_) = self.type_of(rhs, scope, contents)
				{
					let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
					scope.insert(lhs.into_owned(), type_);
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
				// (_ body (for_in_clause left right))
				let for_in = node.named_child(1).unwrap();
				let lhs = for_in.named_child(0).unwrap();
				if lhs.kind() == "identifier"
					&& let rhs = lhs.next_named_sibling().expect(format_loc!("rhs"))
					&& let Some(type_) = self.type_of(rhs, scope, contents)
				{
					let lhs = String::from_utf8_lossy(&contents[lhs.byte_range()]);
					scope.insert(lhs.into_owned(), type_);
				}
			}
			"call" if node.byte_range().contains_end(offset) => {
				// model.{mapped,filtered,sorted,*}(lambda rec: ..)
				let query = MappedCall::query();
				let mut cursor = QueryCursor::new();
				if let Some(mapped_call) = cursor.matches(query, node, contents).next()
					&& let callee = mapped_call
						.nodes_for_capture_index(MappedCall::Callee as _)
						.next()
						.unwrap() && let Some(type_) = self.type_of(callee, scope, contents)
				{
					let iter = mapped_call
						.nodes_for_capture_index(MappedCall::Iter as _)
						.next()
						.unwrap();
					let iter = String::from_utf8_lossy(&contents[iter.byte_range()]);
					scope.insert(iter.into_owned(), type_);
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
					&& let Some(target) = value.next_named_sibling()
					&& target.kind() == "as_pattern_target"
					&& let Some(alias) = dig!(target, identifier)
					&& let Some(callee) = value.named_child(0)
				{
					// TODO: Remove this hardcoded case
					if callee.kind() == "identifier"
						&& b"Form" == &contents[callee.byte_range()]
						&& let Some(first_arg) = value.named_child(1).expect("call args").named_child(0)
						&& let Some(type_) = self.type_of(first_arg, scope, contents)
					{
						let alias = String::from_utf8_lossy(&contents[alias.byte_range()]).into_owned();
						scope.insert(alias, type_);
					} else if let Some(type_) = self.type_of(value, scope, contents) {
						let alias = String::from_utf8_lossy(&contents[alias.byte_range()]).into_owned();
						scope.insert(alias, type_);
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
		#[cfg(debug_assertions)]
		if node.byte_range().len() <= 64 {
			tracing::trace!(
				"type_of {} '{}'",
				node.kind(),
				String::from_utf8_lossy(&contents[node.byte_range()])
			);
		} else {
			tracing::trace!("type_of {} range={:?}", node.kind(), node.byte_range());
		}
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
			"attribute" => self.type_of_attribute_node(node, scope, contents),
			"identifier" => {
				let key = String::from_utf8_lossy(&contents[node.byte_range()]);
				if key == "super" {
					return Some(Type::Super);
				}
				if let Some(type_) = scope.get(key.as_ref()) {
					return Some(type_.clone());
				}
				if key == "request" {
					return Some(Type::HttpRequest);
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
				self.type_of(node.child_by_field_name("left")?, scope, contents)
			}
			_ => None,
		}
	}
	fn type_of_call_node(&self, call: Node<'_>, scope: &Scope, contents: &[u8]) -> Option<Type> {
		let func = call.named_child(0)?;
		let func = self.type_of(func, scope, contents)?;
		match func {
			Type::RefFn => {
				// (call (_) @func (argument_list . (string) @xml_id))
				let xml_id = call.named_child(1)?.named_child(0)?;
				if xml_id.kind() == "string" {
					Some(Type::Record(
						String::from_utf8_lossy(&contents[xml_id.byte_range().shrink(1)])
							.as_ref()
							.into(),
					))
				} else {
					None
				}
			}
			Type::ModelFn(model) => Some(Type::Model(model)),
			Type::Super => scope.get(scope.super_.as_deref()?).cloned(),
			Type::Method(model, mapped) if mapped == "mapped" => {
				// (call (_) @func (argument_list . [(string) (lambda)] @mapped))
				let mapped = call.named_child(1)?.named_child(0)?;
				match mapped.kind() {
					"string" => {
						let mut model: Spur = model.into();
						let mapped = String::from_utf8_lossy(&contents[mapped.byte_range().shrink(1)]);
						let mut mapped = mapped.as_ref();
						self.models.resolve_mapped(&mut model, &mut mapped, None).ok()?;
						self.type_of_attribute(&Type::Model(_R(model).into()), mapped.as_bytes(), scope)
					}
					"lambda" => {
						// (lambda (lambda_parameters)? body: (_))
						let mut scope = Scope::new(Some(scope.clone()));
						if let Some(params) = mapped.child_by_field_name(b"parameters") {
							let first_arg = params.named_child(0)?;
							if first_arg.kind() == "identifier" {
								let first_arg = String::from_utf8_lossy(&contents[first_arg.byte_range()]);
								scope.insert(first_arg.into_owned(), Type::Model(_R(model).into()));
							}
						}
						let body = mapped.child_by_field_name(b"body")?;
						self.type_of(body, &scope, contents)
					}
					_ => None,
				}
			}
			Type::Method(model, method) => {
				let method = _G(&method)?;
				let ret_model = self.resolve_method_returntype(method.into(), *model)?;
				Some(Type::Model(_R(ret_model).into()))
			}
			Type::Env | Type::Record(..) | Type::Model(..) | Type::HttpRequest | Type::Value => None,
		}
	}
	fn type_of_attribute_node(&self, attribute: Node<'_>, scope: &Scope, contents: &[u8]) -> Option<Type> {
		let lhs = attribute.named_child(0)?;
		let lhs = self.type_of(lhs, scope, contents)?;
		let rhs = attribute.named_child(1)?;
		match &contents[rhs.byte_range()] {
			b"env" if matches!(lhs, Type::Model(..) | Type::Record(..) | Type::HttpRequest) => Some(Type::Env),
			b"ref" if matches!(lhs, Type::Env) => Some(Type::RefFn),
			b"user" if matches!(lhs, Type::Env) => Some(Type::Model("res.users".into())),
			b"company" | b"companies" if matches!(lhs, Type::Env) => Some(Type::Model("res.company".into())),
			b"mapped" => {
				let model = self.try_resolve_model(&lhs, scope)?;
				Some(Type::Method(model, "mapped".into()))
			}
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
	}
	pub fn type_of_attribute(&self, type_: &Type, attr: &[u8], scope: &Scope) -> Option<Type> {
		let model = self.try_resolve_model(type_, scope)?;
		let attr = String::from_utf8_lossy(attr);
		let model_entry = self.models.populate_properties(model, &[])?;
		let attr_key = _G(attr.as_ref())?;
		let attr_kind = model_entry.prop_kind(attr_key)?;
		match attr_kind {
			PropertyKind::Field => {
				drop(model_entry);
				let relation = self.models.resolve_related_field(attr_key.into(), model.into())?;
				Some(Type::Model(_R(relation).into()))
			}
			PropertyKind::Method => Some(Type::Method(model, attr.as_ref().into())),
		}
	}
	pub fn has_attribute(&self, type_: &Type, attr: &[u8], scope: &Scope) -> bool {
		(|| -> Option<()> {
			let model = self.try_resolve_model(type_, scope)?;
			let attr = String::from_utf8_lossy(attr);
			let entry = self.models.populate_properties(model, &[])?;
			let attr = _G(attr.as_ref())?;
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
	#[instrument(level = "trace", ret, skip(self, model), fields(model = _R(model)))]
	pub fn resolve_method_returntype(&self, method: Symbol<Method>, model: Spur) -> Option<Symbol<ModelEntry>> {
		_ = self.models.populate_properties(model.into(), &[]);
		let mut model_entry = self.models.get_mut(&model.into())?;
		let method_obj = model_entry.methods.as_mut()?.get_mut(&method)?;
		match method_obj.return_type {
			MethodReturnType::Unprocessed => {}
			MethodReturnType::Value | MethodReturnType::Processing => return None,
			MethodReturnType::Relational(rel) => return Some(rel),
		}

		let location = method_obj.locations.first().cloned()?;
		Arc::make_mut(method_obj).return_type = MethodReturnType::Processing;
		drop(model_entry);

		let contents = String::from_utf8(test_utils::fs::read(location.path.to_path()).unwrap()).unwrap();
		let rope = Rope::from_str(&contents);
		let contents = contents.as_bytes();

		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let range = lsp_range_to_offset_range(location.range, &rope)?;
		let ast = parser.parse(contents, None)?;

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

		let (self_type, fn_scope, self_param) = determine_scope(ast.root_node(), contents, range.end.0)?;
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
		let offset = fn_scope.end_byte();
		let (_, type_) = Self::walk_scope(fn_scope, Some(scope), |scope, node| {
			let entered = self.build_scope(scope, node, offset, contents).map_break(|_| None)?;
			// TODO: When implementing freestanding functions, make the toplevel check optional
			if node.kind() == "return_statement" && is_toplevel_return(node) {
				let Some(child) = node.named_child(0) else {
					return ControlFlow::Continue(entered);
				};
				let Some(type_) = self.type_of(child, scope, contents) else {
					return ControlFlow::Continue(entered);
				};
				let Some(resolved) = self.try_resolve_model(&type_, scope) else {
					return ControlFlow::Continue(entered);
				};
				return ControlFlow::Break(Some(resolved));
			}

			ControlFlow::Continue(entered)
		});

		let mut model = self.models.try_get_mut(&model.into()).expect(format_loc!("deadlock"))?;
		let method = Arc::make_mut(model.methods.as_mut()?.get_mut(&method)?);
		match type_ {
			Some(rel) => method.return_type = MethodReturnType::Relational(rel),
			None => method.return_type = MethodReturnType::Value,
		}

		let docstring = Self::method_docstring(fn_scope, contents)
			.map(|doc| ImStr::from(Method::postprocess_docstring(String::from_utf8_lossy(doc).as_ref())));
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
					"identifier" => FunctionParam::Param(ImStr::from(
						String::from_utf8_lossy(&contents[param.byte_range()]).as_ref(),
					)),
					"positional_separator" => FunctionParam::PosEnd,
					"keyword_separator" => FunctionParam::EitherEnd(None),
					"list_splat_pattern" => FunctionParam::EitherEnd(Some(ImStr::from(
						String::from_utf8_lossy(&contents[param.named_child(0)?.byte_range()]).as_ref(),
					))),
					"dictionary_splat_pattern" => FunctionParam::Kwargs("kwargs".into()),
					"default_parameter" => {
						let name = param.named_child(0)?;
						let name = String::from_utf8_lossy(&contents[name.byte_range()]);
						FunctionParam::Named(ImStr::from(name.as_ref()))
					}
					_ => return None,
				})
			});
			method.arguments = Some(args.collect());
		}

		type_
	}
	fn method_docstring<'out>(fn_scope: Node, contents: &'out [u8]) -> Option<&'out [u8]> {
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
	let self_param = String::from_utf8_lossy(&contents[self_param?.byte_range()]);
	Some((self_type, fn_scope, self_param))
}

#[cfg(test)]
mod tests {
	use pretty_assertions::assert_eq;
	use ropey::Rope;
	use tower_lsp_server::lsp_types::Position;
	use tree_sitter::{Parser, QueryCursor};

	use crate::analyze::FieldCompletion;
	use crate::index::{_I, _R};
	use crate::{index::Index, test_utils::cases::foo::prepare_foo_index, utils::position_to_offset};

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
			.collect::<Vec<_>>();
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
		let fn_start = position_to_offset(Position { line: 3, character: 1 }, &rope).unwrap();
		let fn_scope = ast
			.root_node()
			.named_descendant_for_byte_range(fn_start.0, fn_start.0)
			.unwrap();
		super::determine_scope(ast.root_node(), contents.as_bytes(), fn_start.0)
			.unwrap_or_else(|| panic!("{}", fn_scope.to_sexp()));
	}

	#[test]
	fn test_resolve_method_returntype() {
		let index = Index {
			models: prepare_foo_index(),
			..Default::default()
		};

		assert_eq!(
			index.resolve_method_returntype(_I("test").into(), _I("bar")).map(_R),
			Some("foo")
		)
	}

	#[test]
	fn test_super_analysis() {
		let index = Index {
			models: prepare_foo_index(),
			..Default::default()
		};

		assert_eq!(
			index.resolve_method_returntype(_I("test").into(), _I("quux")).map(_R),
			Some("foo")
		)
	}
}
