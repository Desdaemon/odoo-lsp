use std::ops::DerefMut;
use std::{collections::HashMap, path::PathBuf};

use async_lock::RwLock;
use dashmap::DashMap;
use lasso::{Key, Spur};
use smart_default::SmartDefault;
use tree_sitter::{Node, Parser, QueryCursor};
use ts_macros::query;

use crate::component::{ComponentTemplate, PropDescriptor, PropType};
use crate::index::{PathSymbol, _I};
use crate::utils::{offset_range_to_lsp_range, ts_range_to_lsp_range, ByteOffset, MinLoc, RangeExt};
use crate::{errloc, format_loc, ok, ImStr};

use super::{Component, ComponentName, Output, TemplateName, _R};

// Three cases:
// static props OR Foo.props: Component props
// static template OR Foo.template: Component template (XML name or inline decl)
// static components OR Foo.components: Component subcomponents
#[rustfmt::skip]
query! {
	#[lang = "tree_sitter_javascript"]
	JsQuery(Name, Prop, Parent, TemplateName, TemplateInline, Subcomponent, RegistryCategory, RegistryField);
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
  (member_expression
    (call_expression
      (member_expression (identifier) @_registry (property_identifier) @_category
        (#eq? @_registry "registry")
        (#eq? @_category "category"))
      (arguments . (string) @REGISTRY_CATEGORY .))
    (property_identifier) @_add (#eq? @_add "add"))
  (arguments . (string) @REGISTRY_FIELD))
}

pub(super) async fn add_root_js(root: Spur, pathbuf: PathBuf) -> anyhow::Result<Output> {
	let path = PathSymbol::strip_root(root, &pathbuf);

	#[cfg(target_family = "wasm")]
	let contents = std::fs::read(&pathbuf)?;

	#[cfg(not(target_family = "wasm"))]
	let contents = tokio::fs::read(&pathbuf).await?;

	let rope = ropey::Rope::from(String::from_utf8_lossy(&contents));
	let mut parser = Parser::new();
	ok!(parser.set_language(&tree_sitter_javascript::LANGUAGE.into()));
	let ast = parser.parse(&contents, None).ok_or_else(|| errloc!("AST not parsed"))?;
	let query = JsQuery::query();
	let mut cursor = QueryCursor::new();
	let mut components = HashMap::<_, Component>::default();
	let mut widgets = Vec::new();
	let mut actions = Vec::new();

	for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		let first = match_.captures.first().unwrap();
		debug_assert_eq!(first.index, JsQuery::Name as u32);

		let name = String::from_utf8_lossy(&contents[first.node.byte_range()]);
		let name = _I(&name);
		let component = components.entry(name.into()).or_default();
		if component.location.is_none() {
			component.location = Some(MinLoc {
				path,
				range: ts_range_to_lsp_range(first.node.range()),
			});
		}

		let mut category = None;

		for capture in match_.captures {
			use intmap::Entry;
			match JsQuery::from(capture.index) {
				Some(JsQuery::Prop) => {
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
								range: offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone()).unwrap(),
							},
						}),
					};
					if let Some(descriptor) = capture.node.next_named_sibling() {
						entry.type_ = parse_prop_type(descriptor, &contents, Some(entry.type_));
					}
				}
				Some(JsQuery::Parent) => {
					let parent = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
					let parent = _I(parent);
					component.ancestors.push(parent.into());
				}
				Some(JsQuery::TemplateName) => {
					let name = String::from_utf8_lossy(&contents[capture.node.byte_range().shrink(1)]);
					let name = _I(&name);
					component.template = Some(ComponentTemplate::Name(name.into()));
				}
				Some(JsQuery::TemplateInline) => {
					let range = capture.node.byte_range().shrink(1).map_unit(ByteOffset);
					component.template = Some(ComponentTemplate::Inline(
						offset_range_to_lsp_range(range, rope.clone()).unwrap(),
					));
				}
				Some(JsQuery::Subcomponent) => {
					let subcomponent = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
					let subcomponent = _I(&subcomponent);
					component.subcomponents.push(subcomponent.into());
				}
				// === registry ===
				Some(JsQuery::RegistryCategory) => {
					let range = capture.node.byte_range().shrink(1);
					category = Some(&contents[range]);
				}
				Some(JsQuery::RegistryField) => {
					let range = capture.node.byte_range().shrink(1);
					let field = String::from_utf8_lossy(&contents[range]);
					let loc = MinLoc {
						path,
						range: ts_range_to_lsp_range(capture.node.range()),
					};
					match category {
						Some(b"fields") => widgets.push((ImStr::from(field.as_ref()), loc)),
						Some(b"actions") => actions.push((ImStr::from(field.as_ref()), loc)),
						_ => {}
					}
				}
				Some(JsQuery::Name) | None => {}
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
