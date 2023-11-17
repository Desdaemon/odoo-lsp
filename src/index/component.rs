use std::ops::DerefMut;
use std::{collections::HashMap, path::PathBuf};

use dashmap::DashMap;
use miette::{diagnostic, Context, IntoDiagnostic};
use qp_trie::wrapper::BString;
use tokio::sync::RwLock;
use tree_sitter::{Node, Parser, QueryCursor};
use ts_macros::query_js;

use crate::component::{ComponentTemplate, PropDescriptor, PropType};
use crate::format_loc;
use crate::utils::{offset_range_to_lsp_range, ts_range_to_lsp_range, ByteOffset, MinLoc, RangeExt};

use super::{interner, Component, ComponentName, Output, TemplateName};

// Three cases:
// static props OR Foo.props: Component props
// static template OR Foo.template: Component template (XML name or inline decl)
// static components OR Foo.components: Component subcomponents
query_js! {
	ComponentQuery(NAME, PROP, PARENT, TEMPLATE_NAME, TEMPLATE_INLINE, SUBCOMPONENT);
r#"
((class_declaration
    (identifier) @NAME
    (class_body
      [ (field_definition . "static"
          property: (property_identifier) @_props
          value:
	      [ (array ((string) @PROP "," ?)*)
            (object
              [ (spread_element
                  (member_expression (identifier) @PARENT (property_identifier) @_props))
                (pair key: [(property_identifier) @PROP (string) @PROP]) ])
            (member_expression (identifier) @PARENT (property_identifier) @_props)])
        (field_definition . "static"
          property: (property_identifier) @_template
          value:
	      [ (string) @TEMPLATE_NAME
            (call_expression
              (identifier) @_xml
              (template_string) @TEMPLATE_INLINE)])
        (field_definition . "static"
          property: (property_identifier) @_components
          value:
	      (object
            [ (pair (property_identifier) @SUBCOMPONENT)
              (shorthand_property_identifier) @SUBCOMPONENT])) ]?))
  (#eq? @_props "props")
  (#eq? @_template "template")
  (#eq? @_xml "xml")
  (#eq? @_components "components")
  (#match? @NAME "^[A-Z]")
  (#match? @PARENT "^[A-Z]")
  (#match? @SUBCOMPONENT "^[A-Z]"))

((assignment_expression
    left: (member_expression (identifier) @NAME (property_identifier) @_props)
    right:
    [ (array ((string) @PROP "," ?)*)
      (object
        [ (spread_element
            (member_expression (identifier) @PARENT (property_identifier) @_props))
          (pair key: [(property_identifier) @PROP (string) @PROP]) ])
      (member_expression (identifier) @PARENT (property_identifier) @_props)
      (call_expression
        (member_expression (identifier) @_Object (property_identifier) @_assign)
        (arguments
          [ (member_expression (identifier) @PARENT (property_identifier) @_props)
            (object
              [ (spread_element
                  (member_expression (identifier) @PARENT (property_identifier) @_props))
                (pair key: [(property_identifier) @PROP (string) @PROP]) ]) ]))])
  (#eq? @_props "props")
  (#eq? @_Object "Object")
  (#eq? @_assign "assign")
  (#match? @NAME "^[A-Z]")
  (#match? @PARENT "^[A-Z]"))

((assignment_expression
    left: (member_expression (identifier) @NAME (property_identifier) @_template)
    right:
    [ (string) @TEMPLATE_NAME
      (call_expression (identifier) @_xml (template_string) @TEMPLATE_INLINE) ])
  (#eq? @_template "template")
  (#match? @NAME "^[A-Z]"))

((assignment_expression
    left: (member_expression (identifier) @NAME (property_identifier) @_components)
    right:
    (object
      [ (pair (property_identifier) @SUBCOMPONENT)
        (shorthand_property_identifier) @SUBCOMPONENT ]))
  (#eq? @_components "components")
  (#match? @SUBCOMPONENT "^[A-Z]"))
"#
}

pub(super) async fn add_root_js(path: PathBuf) -> miette::Result<Output> {
	let path_uri = interner().get_or_intern(path.to_string_lossy().as_ref());
	let contents = tokio::fs::read(&path)
		.await
		.into_diagnostic()
		.with_context(|| format_loc!("Could not read {path_uri}"))?;
	let rope = ropey::Rope::from(String::from_utf8_lossy(&contents));
	let mut parser = Parser::new();
	parser
		.set_language(tree_sitter_javascript::language())
		.into_diagnostic()?;
	let ast = parser
		.parse(&contents, None)
		.ok_or_else(|| diagnostic!("AST not parsed"))?;
	let query = ComponentQuery::query();
	let mut cursor = QueryCursor::new();
	let mut components = HashMap::<_, Component>::default();

	for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		let first = match_.captures.first().unwrap();
		debug_assert_eq!(first.index, ComponentQuery::NAME);

		let name = String::from_utf8_lossy(&contents[first.node.byte_range()]);
		let name = interner().get_or_intern(&name);
		let component = components.entry(name.into()).or_default();
		if component.location.is_none() {
			component.location = Some(MinLoc {
				path: path_uri,
				range: ts_range_to_lsp_range(first.node.range()),
			});
		}

		for capture in &match_.captures[1..] {
			use intmap::Entry;
			if capture.index == ComponentQuery::PROP {
				let mut range = capture.node.byte_range();
				if capture.node.kind() == "string" {
					range = range.shrink(1);
				}
				let prop = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
				let prop = interner().get_or_intern(prop).into_inner().get();
				let entry = match component.props.entry(prop as _) {
					Entry::Occupied(entry) => entry.into_mut(),
					Entry::Vacant(entry) => entry.insert(PropDescriptor {
						type_: Default::default(),
						location: MinLoc {
							path: path_uri,
							range: offset_range_to_lsp_range(range.map_unit(ByteOffset), rope.clone()).unwrap(),
						},
					}),
				};
				if let Some(descriptor) = capture.node.next_named_sibling() {
					entry.type_ = parse_prop_type(descriptor, &contents, Some(entry.type_));
				}
			} else if capture.index == ComponentQuery::PARENT {
				let parent = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
				let parent = interner().get_or_intern(parent);
				component.ancestors.push(parent.into());
			} else if capture.index == ComponentQuery::TEMPLATE_NAME {
				let name = String::from_utf8_lossy(&contents[capture.node.byte_range().shrink(1)]);
				let name = interner().get_or_intern(&name);
				component.template = Some(ComponentTemplate::Name(name.into()));
			} else if capture.index == ComponentQuery::TEMPLATE_INLINE {
				let range = capture.node.byte_range().shrink(1).map_unit(ByteOffset);
				component.template = Some(ComponentTemplate::Inline(
					offset_range_to_lsp_range(range, rope.clone()).unwrap(),
				));
			} else if capture.index == ComponentQuery::SUBCOMPONENT {
				let subcomponent = String::from_utf8_lossy(&contents[capture.node.byte_range()]);
				let subcomponent = interner().get_or_intern(&subcomponent);
				component.subcomponents.push(subcomponent.into());
			}
		}
	}

	Ok(Output::Components(components))
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

pub type ComponentPrefixTrie = qp_trie::Trie<BString, ComponentName>;

#[derive(Default)]
pub struct ComponentIndex {
	inner: DashMap<ComponentName, Component>,
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
		let interner = interner();
		for (name, component) in components {
			by_prefix.insert_str(interner.resolve(&name), name.clone());
			if let Some(ComponentTemplate::Name(template_name)) = component.template.as_ref() {
				self.by_template.insert(template_name.clone(), name.clone());
			}
			self.insert(name, component);
		}
	}
}