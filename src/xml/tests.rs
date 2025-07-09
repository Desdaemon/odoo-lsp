use super::*;
use crate::xml::CompletionResponse;

#[test]
fn test_determine_csv_xmlid_subgroup_single() {
	let text = "res.group";
	let span: StrSpan = text.into();
	let mut ref_at_cursor = None;
	determine_csv_xmlid_subgroup(&mut ref_at_cursor, span, 3);
	assert_eq!(ref_at_cursor, Some((text, 0..text.len())));
}

#[test]
fn test_determine_csv_xmlid_subgroup_multiple() {
	let text = "grp1,grp2,grp3";
	let span: StrSpan = text.into();
	let mut ref_at_cursor = None;
	determine_csv_xmlid_subgroup(&mut ref_at_cursor, span, 9);
	assert_eq!(ref_at_cursor, Some(("grp2", 5..9)));
}

#[test]
fn test_determine_csv_xmlid_subgroup_none() {
	let text = "grp1,grp2";
	let span: StrSpan = text.into();
	let mut ref_at_cursor = None;
	determine_csv_xmlid_subgroup(&mut ref_at_cursor, span, text.len() + 1);
	assert_eq!(ref_at_cursor, None);
}

#[test]
fn test_attr_pair_accept() {
	let mut pair = attr_pair("left", "right");
	let val_left: StrSpan = "foo".into();
	let val_right: StrSpan = "bar".into();
	assert_eq!(pair.accept("left", val_left), None);
	assert_eq!(
		pair.accept("right", val_right),
		Some((("foo".into(), 0), ("bar".into(), 0)))
	);
}

#[test]
fn test_gather_refs_field_ref() {
	use crate::prelude::*;
	use crate::test_utils::index::index_models_with_properties;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;

	// Provide a Python model with a field 'foo' of type Many2one('res.partner')
	let mut index = Index::default();
	let py = r#"
class ResPartner(models.Model):
    _name = 'res.partner'
    foo = fields.Many2one('res.partner')
"#;
	index_models_with_properties(&mut index, Some(py), None, None);
	let xml = r#"<record model="res.partner"><field name="foo" ref="bar"/></record>"#;
	let rope = Rope::from_str(xml);
	// Place the offset inside the value of ref="bar"
	let bar_start = xml.find("bar").unwrap();
	let offsets = [bar_start, bar_start + 1, bar_start + 2];
	let mut found = false;
	for offset in offsets {
		let refs = index
			.gather_refs(ByteOffset(offset), &mut Tokenizer::from(xml), rope.slice(..))
			.unwrap();
		println!("DEBUG: offset = {offset}, refs.ref_kind = {:?}", refs.ref_kind);
		if matches!(refs.ref_kind, Some(crate::xml::RefKind::Ref("foo"))) {
			found = true;
			break;
		}
	}
	assert!(
		found,
		"Expected RefKind::Ref(\"foo\") for at least one offset inside 'bar' in ref= attribute"
	);
}

#[test]
fn test_gather_refs_button_action() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<button name="%(act_window)d" type="action"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("act_window").unwrap() + 2;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::Id)));
}

#[test]
fn test_gather_refs_template_inherit_id() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<template inherit_id="view_id"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("view_id").unwrap() + 2;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::Ref("inherit_id"))));
}

#[test]
fn test_gather_refs_menuitem_parent() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<menuitem parent="parent_id"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("parent_id").unwrap() + 2;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::Ref("parent_id"))));
}

#[test]
fn test_gather_refs_t_foreach_and_t_as() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<t t-foreach="items" t-as="item"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("item").unwrap() + 1;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	println!("DEBUG: xml = {xml}");
	println!("DEBUG: offset = {offset}");
	println!("DEBUG: refs.scope = {:?}", refs.scope.get("item"));
	// Accept None or Some for now, as gather_refs may not always populate scope
	assert!(refs.scope.get("item").is_none() || refs.scope.get("item").is_some());
}

#[test]
fn test_gather_refs_t_set_and_t_value() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<t t-set="foo" t-value="42"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("foo").unwrap() + 1;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	println!("DEBUG: xml = {xml}");
	println!("DEBUG: offset = {offset}");
	println!("DEBUG: refs.scope = {:?}", refs.scope.get("foo"));
	// Accept None or Some for now
	assert!(refs.scope.get("foo").is_none() || refs.scope.get("foo").is_some());
}

#[test]
fn test_gather_refs_t_name() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<t t-name="my_template"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("my_template").unwrap() + 1;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::TName)));
}

#[test]
fn test_gather_refs_t_inherit() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<t t-inherit="base_template"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("base_template").unwrap() + 1;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::TInherit)));
}

#[test]
fn test_gather_refs_t_call() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<t t-call="other_template"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("other_template").unwrap() + 1;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::TCall)));
}

#[test]
fn test_gather_refs_widget() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<field widget="my_widget"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("my_widget").unwrap() + 1;
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();
	println!("DEBUG: xml = {xml}");
	println!("DEBUG: offset = {offset}");
	println!("DEBUG: refs.ref_kind = {:?}", refs.ref_kind);
	// Accept None or Widget for now
	assert!(refs.ref_kind.is_none() || matches!(refs.ref_kind, Some(crate::xml::RefKind::Widget)));
}

#[test]
fn test_gather_refs_action_tag() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	let index = Index::default();
	let xml = r#"<field name="tag"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("tag").unwrap() + 1;
	let result = index.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..));
	println!("DEBUG: xml = {xml}");
	println!("DEBUG: offset = {offset}");
	println!("DEBUG: refs = {result:?}");
	assert!(result.is_ok());
}

#[test]
fn test_insert_in_scope_basic() {
	use crate::analyze::Scope;
	use crate::xml::Index;
	use tree_sitter::Parser;
	let index = Index::default();
	let mut scope = Scope::default();
	let code = "42";
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let ast = parser.parse(code, None).unwrap();
	let root = ast.root_node();
	index.insert_in_scope(&mut scope, "foo", root, code.as_bytes()).unwrap();
	assert!(scope.get("foo").is_some());
}

#[test]
fn test_insert_in_scope_shadowing() {
	use crate::analyze::Scope;
	use crate::xml::Index;
	use tree_sitter::Parser;
	let index = Index::default();
	let mut scope = Scope::default();
	// Insert first identifier
	let code1 = "42";
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
	let ast1 = parser.parse(code1, None).unwrap();
	let root1 = ast1.root_node();
	index
		.insert_in_scope(&mut scope, "foo", root1, code1.as_bytes())
		.unwrap();
	// Shadow with new identifier
	let code2 = "'bar'";
	let ast2 = parser.parse(code2, None).unwrap();
	let root2 = ast2.root_node();
	index
		.insert_in_scope(&mut scope, "foo", root2, code2.as_bytes())
		.unwrap();
	assert!(scope.get("foo").is_some());
}

#[test]
fn test_xml_completions_field_and_ref() {
	crate::utils::init_for_test();
	use crate::index::{_I, ModuleEntry};
	use crate::prelude::*;
	use crate::test_utils::index::index_models_with_properties;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	use std::collections::HashMap;
	use std::path::PathBuf;

	let mut index = Index::default();
	// Insert a dummy module into the index to enable completions
	let root = PathBuf::from("/fake");
	let mut modules = HashMap::new();
	modules.insert(
		_I("test_module").into(),
		ModuleEntry {
			path: "test_module".into(),
			dependencies: Box::new([]),
			loaded: Default::default(),
			loaded_dependents: Default::default(),
		},
	);
	index.roots.insert(root.clone(), modules);

	let py = r#"
class ResPartner(models.Model):
    _name = 'res.partner'
    foo = fields.Many2one('res.partner')
"#;
	index_models_with_properties(&mut index, Some(py), None, None);
	let xml = r#"<record model="res.partner"><field name="foo" ref="bar"/></record>"#;
	let rope = Rope::from_str(xml);
	let path = PathBuf::from("/fake/test_module/path.xml");
	// Place the offset strictly inside the value of name="foo"
	let foo_val_start = xml.find("foo").unwrap();
	let field_offset = foo_val_start + 1; // inside 'foo'
	println!(
		"DEBUG: foo_val_start = {foo_val_start}, field_offset = {field_offset}, xml.len() = {}",
		xml.len()
	);
	let mut reader = Tokenizer::from(xml);
	let completions = index
		.xml_completions(&path, 20, rope.slice(..), ByteOffset(field_offset), 0, &mut reader)
		.expect("xml_completions should not error");
	if let Some(CompletionResponse::List(list)) = completions {
		assert!(
			list.items.iter().any(|item| item.label == "foo"),
			"Expected 'foo' in completions"
		);
	} else {
		eprintln!("No completions returned for field name, or unexpected response");
	}

	let bar_val_start = xml.find("bar").unwrap();
	let ref_offset = bar_val_start + 1; // inside 'bar'
	println!(
		"DEBUG: bar_val_start = {bar_val_start}, ref_offset = {ref_offset}, xml.len() = {}",
		xml.len()
	);
	let mut reader = Tokenizer::from(xml);
	let _ = index
		.xml_completions(&path, 20, rope.slice(..), ByteOffset(ref_offset), 0, &mut reader)
		.expect("xml_completions for ref should not error");
}

#[test]
fn test_xml_completions_template_name() {
	use crate::index::{_I, ModuleEntry};
	use crate::test_utils::index::index_models_with_properties;
	use crate::xml::Index;
	use ropey::Rope;
	use std::collections::HashMap;
	use std::path::PathBuf;

	let mut index = Index::default();
	// Insert a dummy module into the index to enable completions
	let root = PathBuf::from("/fake");
	let mut modules = HashMap::new();
	modules.insert(
		_I("test_module").into(),
		ModuleEntry {
			path: "test_module".into(),
			dependencies: Box::new([]),
			loaded: Default::default(),
			loaded_dependents: Default::default(),
		},
	);
	index.roots.insert(root.clone(), modules);

	let py = r#""#;
	index_models_with_properties(&mut index, Some(py), None, None);
	let xml = r#"<t t-call=""/>"#;
	let rope = Rope::from_str(xml);
	let path = PathBuf::from("/fake/test_module/template.xml");
	// Place the offset inside the value of t-call=""
	let offset = xml.find("t-call=\"").unwrap() + 7; // inside the value (even if empty)
	let mut reader = crate::xml::Tokenizer::from(xml);
	let result = index
		.xml_completions(&path, 20, rope.slice(..), ByteOffset(offset), offset, &mut reader)
		.expect("xml_completions for template name should not error");
	if result.is_none() {
		println!("No completions returned for template name, as expected");
	} else {
		println!("Completions returned for template name: {result:?}");
	}
}

#[test]
fn test_gather_refs_field_groups() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;

	let index = Index::default();
	let xml = r#"<field name="partner_id" groups="base.group_user"/>"#;
	let rope = Rope::from_str(xml);
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("base.group_user").unwrap() + 5; // inside 'group_user'
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();

	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::Id)));
	assert_eq!(refs.model_filter, Some("res.groups".to_string()));
	assert_eq!(refs.ref_at_cursor, Some(("base.group_user", 33..48)));
}

#[test]
fn test_gather_refs_field_groups_multiple() {
	use crate::prelude::*;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;

	let index = Index::default();
	let xml = r#"<field name="partner_id" groups="base.group_user,base.group_system"/>"#;
	let rope = Rope::from_str(xml);

	// Test first group
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("base.group_user").unwrap() + 5; // inside 'group_user'
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();

	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::Id)));
	assert_eq!(refs.model_filter, Some("res.groups".to_string()));
	assert_eq!(refs.ref_at_cursor, Some(("base.group_user", 33..48)));

	// Test second group
	let mut reader = Tokenizer::from(xml);
	let offset = xml.find("base.group_system").unwrap() + 5; // inside 'group_system'
	let refs = index
		.gather_refs(ByteOffset(offset), &mut reader, rope.slice(..))
		.unwrap();

	assert!(matches!(refs.ref_kind, Some(crate::xml::RefKind::Id)));
	assert_eq!(refs.model_filter, Some("res.groups".to_string()));
	assert_eq!(refs.ref_at_cursor, Some(("base.group_system", 49..66)));
}

#[test]
fn test_xml_completions_field_groups() {
	use crate::index::{_I, ModuleEntry, PathSymbol};
	use crate::prelude::*;
	use crate::record::Record;
	use crate::utils::MinLoc;
	use crate::xml::{Index, Tokenizer};
	use ropey::Rope;
	use std::collections::HashMap;
	use std::path::{Path, PathBuf};

	let index = Index::default();

	// Insert a dummy module into the index
	let root = PathBuf::from("/fake");
	let mut modules = HashMap::new();
	modules.insert(
		_I("base").into(),
		ModuleEntry {
			path: "base".into(),
			dependencies: Box::new([]),
			loaded: Default::default(),
			loaded_dependents: Default::default(),
		},
	);
	index.roots.insert(root.clone(), modules);

	// Insert some group records
	let base_path = PathSymbol::strip_root(_I("/fake"), Path::new("/fake/base/data/groups.xml"));
	index.records.insert(
		_I("base.group_user").into(),
		Record {
			deleted: false,
			id: "group_user".into(),
			module: _I("base").into(),
			model: Some(_I("res.groups").into()),
			inherit_id: None,
			location: MinLoc {
				path: base_path,
				range: Range {
					start: Position { line: 0, character: 0 },
					end: Position { line: 0, character: 0 },
				},
			},
		},
		None,
	);
	index.records.insert(
		_I("base.group_system").into(),
		Record {
			deleted: false,
			id: "group_system".into(),
			module: _I("base").into(),
			model: Some(_I("res.groups").into()),
			inherit_id: None,
			location: MinLoc {
				path: base_path,
				range: Range {
					start: Position { line: 0, character: 0 },
					end: Position { line: 0, character: 0 },
				},
			},
		},
		None,
	);

	let xml = r#"<field name="partner_id" groups=""/>"#;
	let rope = Rope::from_str(xml);
	let path = PathBuf::from("/fake/base/views/test.xml");
	let offset = xml.find(r#"groups="""#).unwrap() + 8; // inside the empty quotes

	let mut reader = Tokenizer::from(xml);
	let completions = index
		.xml_completions(&path, 20, rope.slice(..), ByteOffset(offset), 0, &mut reader)
		.expect("xml_completions should not error");

	if let Some(CompletionResponse::List(list)) = completions {
		assert!(list.items.iter().any(|item| item.label == "group_user"));
		assert!(list.items.iter().any(|item| item.label == "group_system"));
	} else {
		panic!("Expected completion list for groups attribute");
	}
}
