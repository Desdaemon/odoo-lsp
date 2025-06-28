use super::*;

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
