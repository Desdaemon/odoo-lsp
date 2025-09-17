use std::ops::DerefMut;
use std::{collections::HashMap, path::PathBuf};

use dashmap::DashMap;
use lasso::{Key, Spur};
use smart_default::SmartDefault;
use tokio::sync::RwLock;
use tree_sitter::{Node, Parser, QueryCursor, StreamingIterator};
use ts_macros::query;

use crate::component::{ComponentTemplate, PropDescriptor, PropType};
use crate::index::{_I, PathSymbol};
use crate::utils::{ByteOffset, MinLoc, RangeExt, rope_conv, span_conv};
use crate::{ImStr, dig, errloc, format_loc, ok};

use super::{_R, Component, ComponentName, Output, TemplateName};

// Three cases:
// static props OR Foo.props: Component props
// static template OR Foo.template: Component template (XML name or inline decl)
// static components OR Foo.components: Component subcomponents
#[rustfmt::skip]
query! {
	#[lang = "tree_sitter_javascript"]
	JsQuery(Name, Prop, Parent, TemplateName, TemplateInline, Subcomponent, Registry, RegistryItem);
((class_declaration
  (identifier) @NAME
  (class_body [
    (field_definition . "static"
      property: (property_identifier) @_props 
      value: [
        (array ((string) @PROP "," ?)*)
        (object [
          (spread_element
            (member_expression
              (identifier) @PARENT (property_identifier) @_props))
          (pair key: [
            (property_identifier) @PROP 
            (string) @PROP]) ])
        (member_expression
          (identifier) @PARENT (property_identifier) @_props)])
    (field_definition . "static"
      property: (property_identifier) @_template 
      value: [ 
        (string) @TEMPLATE_NAME
        (template_string) @TEMPLATE_NAME
        (call_expression
          (identifier) @_xml (template_string) @TEMPLATE_INLINE)])
    (field_definition . "static"
      property: (property_identifier) @_components 
      value: (object [
        (pair (property_identifier) @SUBCOMPONENT)
        (shorthand_property_identifier) @SUBCOMPONENT])) ]?))
  (#eq? @_props "props")
  (#eq? @_template "template")
  (#eq? @_xml "xml")
  (#eq? @_components "components")
  (#match? @NAME "^[A-Z]")
  (#match? @PARENT "^[A-Z]")
  (#match? @SUBCOMPONENT "^[A-Z]"))


((assignment_expression
  left: (member_expression
    (identifier) @NAME (property_identifier) @_props)
  right: [ 
    (array ((string) @PROP "," ?)*)
    (object [ 
      (spread_element
        (member_expression
          (identifier) @PARENT (property_identifier) @_props))
      (pair key: [
        (property_identifier) @PROP 
        (string) @PROP]) ])
    (member_expression
      (identifier) @PARENT (property_identifier) @_props)
    (call_expression
      (member_expression
        (identifier) @_Object (property_identifier) @_assign)
      (arguments [ 
        (member_expression
          (identifier) @PARENT (property_identifier) @_props)
        (object [ 
          (spread_element
            (member_expression
              (identifier) @PARENT (property_identifier) @_props))
          (pair key: [
            (property_identifier) @PROP 
            (string) @PROP]) ]) ]))])
  (#eq? @_props "props")
  (#eq? @_Object "Object")
  (#eq? @_assign "assign")
  (#match? @NAME "^[A-Z]")
  (#match? @PARENT "^[A-Z]"))

((assignment_expression
  left: (member_expression
    (identifier) @NAME (property_identifier) @_template)
  right: [
    (string) @TEMPLATE_NAME
    (template_string) @TEMPLATE_NAME
    (call_expression (identifier) @_xml (template_string) @TEMPLATE_INLINE) ])
  (#eq? @_template "template")
  (#match? @NAME "^[A-Z]"))

((assignment_expression
  left: (member_expression
    (identifier) @NAME (property_identifier) @_components)
  right: (object [
    (pair (property_identifier) @SUBCOMPONENT)
    (shorthand_property_identifier) @SUBCOMPONENT ]))
  (#eq? @_components "components")
  (#match? @SUBCOMPONENT "^[A-Z]"))

// registry.category(CATEGORY).add(FIELD, ..)
(call_expression
  (member_expression (_) @REGISTRY (property_identifier) @_add (#eq? @_add "add"))
  (arguments . (string) @REGISTRY_ITEM))
}

pub(super) async fn add_root_js(root: Spur, pathbuf: PathBuf) -> anyhow::Result<Output> {
	let path = PathSymbol::strip_root(root, &pathbuf);
	let contents = ok!(tokio::fs::read(&pathbuf).await, "Could not read {:?}", pathbuf);
	let rope = ropey::Rope::from(String::from_utf8_lossy(&contents));
	let rope = rope.slice(..);
	let mut parser = Parser::new();
	ok!(parser.set_language(&tree_sitter_javascript::LANGUAGE.into()));
	let ast = parser.parse(&contents, None).ok_or_else(|| errloc!("AST not parsed"))?;
	let query = JsQuery::query();
	let mut cursor = QueryCursor::new();
	let mut components = HashMap::<_, Component>::default();
	let mut widgets = Vec::new();
	let mut actions = Vec::new();

	let mut matches = cursor.matches(query, ast.root_node(), contents.as_slice());
	while let Some(match_) = matches.next() {
		// let first = match_.captures.first().unwrap();
		// debug_assert_eq!(
		// 	first.index,
		// 	JsQuery::Name as u32,
		// 	"{}",
		// 	String::from_utf8_lossy(&contents[first.node.byte_range()])
		// );

		let mut component = match match_.captures.first() {
			Some(first) if first.index == JsQuery::Name as u32 => {
				let name = String::from_utf8_lossy(&contents[first.node.byte_range()]);
				let name = _I(&name);
				let component = components.entry(name.into()).or_default();
				if component.location.is_none() {
					component.location = Some(MinLoc {
						path,
						range: span_conv(first.node.range()),
					});
				}
				Some(component)
			}
			_ => None,
		};

		// let mut category = None;

		for capture in match_.captures {
			use intmap::Entry;
			match JsQuery::from(capture.index) {
				Some(JsQuery::Prop) => {
					let Some(component) = &mut component else { continue };
					let mut range = capture.node.byte_range();
					if capture.node.kind() == "string" {
						range = range.shrink(1);
					}
					let prop = String::from_utf8_lossy(&contents[range.clone()]);
					let prop = _I(prop).into_usize();
					let entry = match component.props.entry(prop as _) {
						Entry::Occupied(entry) => entry.into_mut(),
						Entry::Vacant(entry) => entry.insert(PropDescriptor {
							type_: Default::default(),
							location: MinLoc {
								path,
								range: rope_conv(range.map_unit(ByteOffset), rope),
							},
						}),
					};
					if let Some(descriptor) = capture.node.next_named_sibling() {
						entry.type_ = parse_prop_type(descriptor, &contents, Some(entry.type_));
					}
				}
				Some(JsQuery::Parent) => {
					let Some(component) = &mut component else { continue };
					let parent = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
					let parent = _I(parent);
					component.ancestors.push(parent.into());
				}
				Some(JsQuery::TemplateName) => {
					let Some(component) = &mut component else { continue };
					let name = String::from_utf8_lossy(&contents[capture.node.byte_range().shrink(1)]);
					let name = _I(&name);
					component.template = Some(ComponentTemplate::Name(name.into()));
				}
				Some(JsQuery::TemplateInline) => {
					let Some(component) = &mut component else { continue };
					let range = capture.node.byte_range().shrink(1).map_unit(ByteOffset);
					component.template = Some(ComponentTemplate::Inline(rope_conv(range, rope)));
				}
				Some(JsQuery::Subcomponent) => {
					let Some(component) = &mut component else { continue };
					let subcomponent = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
					let subcomponent = _I(&subcomponent);
					component.subcomponents.push(subcomponent.into());
				}
				Some(JsQuery::RegistryItem) => {
					let Some(registry) = match_.nodes_for_capture_index(JsQuery::Registry as _).next() else {
						continue;
					};
					let range = capture.node.byte_range().shrink(1);
					let field = String::from_utf8_lossy(&contents[range]);
					let loc = MinLoc {
						path,
						range: span_conv(capture.node.range()),
					};
					match registry_category_of_callee(registry, &contents) {
						Some(b"fields") => widgets.push((ImStr::from(field.as_ref()), loc)),
						Some(b"actions") => actions.push((ImStr::from(field.as_ref()), loc)),
						Some(_) | None => {}
					}
				}
				Some(JsQuery::Registry) | Some(JsQuery::Name) | None => {}
			}
		}
	}

	Ok(Output::JsItems {
		components,
		widgets,
		actions,
	})
}

fn parse_prop_type(node: Node, contents: &[u8], seed: Option<PropType>) -> PropType {
	let mut type_ = seed.unwrap_or_default();
	if node.kind() == "array" {
		// TODO: Is this correct?
		type_.insert(PropType::Optional);
		for child in node.named_children(&mut node.walk()) {
			type_ = parse_prop_type(child, contents, Some(type_));
		}
		return type_;
	}

	fn parse_identifier_prop(node: Node, contents: &[u8], mut type_: PropType) -> PropType {
		debug_assert_eq!(
			node.kind(),
			"identifier",
			"Expected `identifier` node, got {}",
			node.kind()
		);
		match &contents[node.byte_range()] {
			b"String" => type_.insert(PropType::String),
			b"Number" => type_.insert(PropType::Number),
			b"Boolean" => type_.insert(PropType::Boolean),
			b"Object" => type_.insert(PropType::Object),
			b"Function" => type_.insert(PropType::Function),
			b"Array" => type_.insert(PropType::Array),
			_ => {}
		}
		type_
	}

	if node.kind() == "identifier" {
		return parse_identifier_prop(node, contents, type_);
	}

	if node.kind() != "object" {
		return type_;
	}

	// { type: (String | [String, Boolean, ..]), optional?: true }
	for child in node.named_children(&mut node.walk()) {
		if child.kind() == "pair" {
			// (pair left right)
			let prop = child.named_child(0).unwrap();
			let mut range = prop.byte_range();
			if prop.kind() == "string" {
				range = range.shrink(1);
			}
			let value = prop.next_named_sibling().unwrap();
			match &contents[range] {
				b"type" => {
					type_ = parse_prop_type(value, contents, None);
				}
				b"optional" if value.kind() == "true" => {
					type_.insert(PropType::Optional);
				}
				// TODO: Handle 'shape'
				_ => {}
			}
		}
	}

	type_
}

pub type ComponentPrefixTrie = qp_trie::Trie<&'static [u8], ComponentName>;

#[derive(SmartDefault)]
pub struct ComponentIndex {
	#[default(_code = "DashMap::with_shard_amount(4)")]
	inner: DashMap<ComponentName, Component>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub by_template: DashMap<TemplateName, ComponentName>,
	pub by_prefix: RwLock<ComponentPrefixTrie>,
}

impl core::ops::Deref for ComponentIndex {
	type Target = DashMap<ComponentName, Component>;
	#[inline]
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl DerefMut for ComponentIndex {
	#[inline]
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.inner
	}
}

impl ComponentIndex {
	pub fn extend(&self, components: HashMap<ComponentName, Component>) {
		let mut by_prefix = self.by_prefix.try_write().expect(format_loc!("deadlock"));
		for (name, component) in components {
			by_prefix.insert(_R(name).as_bytes(), name);
			if let Some(ComponentTemplate::Name(template_name)) = component.template.as_ref() {
				self.by_template.insert(*template_name, name);
			}
			self.insert(name, component);
		}
	}
}

/// - `node`: A tree-sitter [Node] from a JS AST
fn registry_category_of_callee<'text>(mut callee: Node, contents: &'text [u8]) -> Option<&'text [u8]> {
	loop {
		// callee ?= registry.category($category)
		if callee.kind() == "call_expression"
			&& let Some(registry_category) = dig!(callee, member_expression)
			&& let Some(registry) = dig!(registry_category, identifier)
			&& b"registry" == &contents[registry.byte_range()]
			&& let Some(prop_category) = dig!(registry_category, property_identifier(1))
			&& b"category" == &contents[prop_category.byte_range()]
			&& let Some(category_node) = dig!(callee, arguments(1).string)
		{
			return Some(&contents[category_node.byte_range().shrink(1)]);
		}

		callee = callee.named_child(0)?;
	}
}

#[cfg(test)]
mod tests {
	use super::registry_category_of_callee;
	use crate::prelude::*;
	use pretty_assertions::assert_eq;

	#[test]
	fn test_registry_category_of_callee() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_javascript::LANGUAGE.into()).unwrap();
		let contents = br#"registry.category("fields")"#;
		let ast = parser.parse(contents, None).unwrap();
		let call = dig!(ast.root_node(), expression_statement.call_expression).unwrap();
		assert_eq!(registry_category_of_callee(call, contents), Some(b"fields".as_slice()));
	}

	#[test]
	fn test_registry_category_of_callee_nested() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_javascript::LANGUAGE.into()).unwrap();
		let contents = br#"registry.category("fields").add("foobar").add("barbaz")"#;
		let ast = parser.parse(contents, None).unwrap();
		let call = dig!(ast.root_node(), expression_statement.call_expression).expect(r#"$.add("barbaz")"#);
		assert_eq!(registry_category_of_callee(call, contents), Some(b"fields".as_slice()));
	}
}
