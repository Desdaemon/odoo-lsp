use super::*;

fn labels(text: &str, cursor: usize) -> Vec<String> {
	completions(text, cursor).into_iter().map(|it| it.label).collect()
}

#[test]
fn required_attributes_tracking() {
	// Directly declared, not under `<optional>`.
	assert_eq!(required_attributes("record").collect::<Vec<_>>(), vec!["model"]);
	// `<field>`'s `name` is mandatory; `type` sits under a `<choice>` so it is
	// not flagged as required (a value-bearing `<field name="x">v</field>` is
	// valid without it).
	let field_required_attrs: Vec<_> = required_attributes("field").collect();
	assert_eq!(field_required_attrs, vec!["name"]);
	assert!(!field_required_attrs.contains(&"type"));
	assert_eq!(required_attributes("xpath").collect::<Vec<_>>(), vec!["expr"]);
	// `id`/`forcecreate` on <record> are `<optional>`.
	assert!(!required_attributes("record").any(|it| it == "id"));
	// Unknown elements have no requirements.
	assert!(required_attributes("not-an-element").count() == 0);
}

#[test]
fn allowed_children_tracking() {
	// `<record>` has a closed content model: only `<field>` is permitted.
	assert_eq!(is_allowed_child("record", "field"), Some(true));
	assert_eq!(is_allowed_child("record", "bogustag"), Some(false));
	// `<odoo>` admits records but not stray fields at the top level.
	assert_eq!(is_allowed_child("odoo", "record"), Some(true));
	assert_eq!(is_allowed_child("odoo", "field"), Some(false));
	// `<form>` permits arbitrary markup (`<anyName>`), so nothing is flagged.
	assert_eq!(is_allowed_child("form", "whatever"), None);
	// Unknown parents can't be judged.
	assert_eq!(is_allowed_child("not-an-element", "field"), None);
}

#[test]
fn unknown_element_tracking() {
	assert!(is_unknown_element("bogustag"));
	// Known elements — including ones that only appear as children — are known.
	assert!(!is_unknown_element("field"));
	assert!(!is_unknown_element("button"));
	assert!(!is_unknown_element("record"));
}

#[test]
fn enum_values_tracking() {
	// `<field type=..>` is a closed enumeration (union across the grammar's
	// per-type groups), with no free-form escape hatch.
	let field_type = enum_values("field", "type").expect("field/type is a closed enum");
	assert!(field_type.iter().any(|v| v == "char"));
	assert!(field_type.iter().any(|v| v == "int"));
	assert!(!field_type.iter().any(|v| v == "bogus"));
	// `mode` mixes `<value>`s with `<text/>`, so it stays open.
	assert_eq!(enum_values("field", "mode"), None);
	// `name` has no enumerated values at all.
	assert_eq!(enum_values("field", "name"), None);
	// Same attribute name, different (open) element.
	assert_eq!(enum_values("value", "type"), None);
	// Unknown element/attribute.
	assert_eq!(enum_values("field", "not-an-attr"), None);
	assert_eq!(enum_values("not-an-element", "type"), None);
}

#[test]
fn roots_from_start() {
	let schema = &*SCHEMA;
	assert!(schema.roots.contains("odoo"));
	assert!(schema.roots.contains("data"));
	assert!(schema.roots.contains("openerp"));
}

#[test]
fn record_has_expected_shape() {
	let schema = &*SCHEMA;
	let record = schema.elements.get("record").expect("record element");
	assert!(record.children.contains("field"), "record should allow <field>");
	// attributes come from both direct declarations and the `env` ref.
	assert!(record.attrs.contains_key("model"));
	assert!(record.attrs.contains_key("id"));
	assert!(record.attrs.contains_key("context"), "env ref should be expanded");
	assert!(record.doc.is_some());
}

#[test]
fn complete_child_element() {
	let text = "<record>\n  <fi";
	let got = labels(text, text.len());
	assert!(got.contains(&"field".to_string()), "got: {got:?}");
	// Elements not valid under <record> should not leak in.
	assert!(!got.contains(&"menuitem".to_string()), "got: {got:?}");
}

#[test]
fn complete_closing_tag() {
	// `</` offers the innermost open element.
	let text = "<record>\n  </";
	assert_eq!(labels(text, text.len()), vec!["record".to_string()]);
	// A partial closing name is prefix-filtered against the open element.
	let text = "<record>\n  <field>\n  </fi";
	assert_eq!(labels(text, text.len()), vec!["field".to_string()]);
	// A non-matching prefix offers nothing.
	let text = "<record>\n  </zzz";
	assert!(labels(text, text.len()).is_empty());
	// Even elements the schema doesn't know can be closed structurally.
	let text = "<madeup>\n  </";
	assert_eq!(labels(text, text.len()), vec!["madeup".to_string()]);
	// Nothing open → nothing to close.
	let text = "</";
	assert!(labels(text, text.len()).is_empty());
}

#[test]
fn complete_root_element() {
	let text = "<od";
	let got = labels(text, text.len());
	assert!(got.contains(&"odoo".to_string()), "got: {got:?}");
}

#[test]
fn complete_attribute_name() {
	let text = "<record m";
	let got = labels(text, text.len());
	assert!(got.contains(&"model".to_string()), "got: {got:?}");
	assert!(!got.contains(&"id".to_string()), "prefix should filter: {got:?}");
}

#[test]
fn complete_attribute_value_enum() {
	let text = "<field type=\"";
	let got = labels(text, text.len());
	assert!(got.contains(&"char".to_string()), "got: {got:?}");
	assert!(got.contains(&"int".to_string()), "got: {got:?}");
}

#[test]
fn attribute_value_completions_are_unique() {
	// `class` (from `oe_class`) is reachable through several of the merged
	// `<field>` definitions; its enumerated values must not be offered twice.
	let text = "<field class=\"";
	let got = labels(text, text.len());
	let mut deduped = got.clone();
	deduped.sort();
	deduped.dedup();
	assert_eq!(got.len(), deduped.len(), "duplicate value completions: {got:?}");
	assert!(got.contains(&"oe_inline".to_string()), "got: {got:?}");
}

#[test]
fn no_completion_in_plain_text() {
	let text = "<record>\n  hello ";
	assert!(completions(text, text.len()).is_empty());
}

#[test]
fn hover_element_name() {
	let text = "<record model=\"res.partner\">";
	let cursor = text.find("record").unwrap() + 1;
	let (doc, _range) = hover(text, cursor).expect("element hover");
	assert!(doc.to_lowercase().contains("record"), "doc: {doc}");
}

#[test]
fn hover_attribute_name() {
	let text = "<record model=\"res.partner\">";
	let cursor = text.find("model").unwrap() + 1;
	let (doc, range) = hover(text, cursor).expect("attribute hover");
	assert_eq!(&text[range], "model");
	assert!(!doc.is_empty());
}
