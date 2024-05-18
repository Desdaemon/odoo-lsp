use miette::diagnostic;
use ropey::Rope;
use tower_lsp::lsp_types::{Position, Range};
use xmlparser::{ElementEnd, Token, Tokenizer};

use crate::index::{interner, PathSymbol, Symbol};
use crate::utils::{offset_to_position, ByteOffset, MinLoc};

#[derive(Default, Debug)]
pub struct Template {
	pub location: Option<MinLoc>,
	pub descendants: Vec<Template>,
}

/// t-name IFF t-inherit not specified or t-inherit-mode=extension,
/// otherwise t-inherit
pub type TemplateName = Symbol<Template>;

#[derive(Debug)]
pub struct NewTemplate {
	pub base: bool,
	pub name: TemplateName,
	pub template: Template,
}

pub fn gather_templates(
	path: PathSymbol,
	reader: &mut Tokenizer,
	document: Rope,
	templates: &mut Vec<NewTemplate>,
	legacy: bool,
) -> miette::Result<()> {
	let mut root_tag = None;
	let mut tag_start = 0;
	let mut stack = 0;
	let mut maybe_nested = false;

	let mut t_name = None;
	let mut t_inherit = None;
	let mut base = true;
	let wrapper = if legacy { "template" } else { "templates" };

	loop {
		match reader.next() {
			Some(Ok(Token::ElementStart { local, span, .. })) => {
				if matches!(root_tag, Some(tag) if tag == local.as_str()) {
					stack += 1;
					maybe_nested = true;
				} else if stack <= 0 {
					stack = 1;
					root_tag = Some(local.as_str());
					tag_start = span.start();
					maybe_nested = true;
				}
			}
			Some(Ok(Token::ElementEnd { end, span })) => {
				let mut templates_end = false;
				match end {
					ElementEnd::Close(_, tag_end) if matches!(root_tag, Some(tag) if tag == tag_end) => {
						stack -= 1;
					}
					ElementEnd::Close(_, templates) if templates == wrapper => {
						stack = 0;
						templates_end = true;
					}
					ElementEnd::Empty if maybe_nested => {
						stack -= 1;
					}
					_ => {}
				}
				maybe_nested = false;
				if stack <= 0 {
					_ = root_tag.take();
					let t_name = t_name.take();
					let t_inherit = t_inherit.take();
					let name_candidate = if base { t_name } else { t_inherit };
					let Some(name) = name_candidate else {
						if templates_end {
							break;
						} else {
							continue;
						}
					};
					let name = interner().get_or_intern(name).into();
					let start = offset_to_position(ByteOffset(tag_start), document.clone())
						.ok_or_else(|| diagnostic!("qweb_templates start <- tag_start"))?;
					let end = offset_to_position(ByteOffset(span.end()), document.clone())
						.ok_or_else(|| diagnostic!("qweb_templates end <- span.end()"))?;
					let range = Range { start, end };
					templates.push(NewTemplate {
						base,
						name,
						template: Template {
							location: Some(MinLoc { path, range }),
							descendants: vec![],
						},
					})
				}
				if templates_end {
					break;
				}
			}
			Some(Ok(Token::Attribute { local, value, .. })) if stack == 1 => match local.as_str() {
				"t-name" => {
					t_name = Some(value.as_str());
				}
				"t-inherit" => {
					t_inherit = Some(value.as_str());
					base = true;
				}
				"t-inherit-mode" => {
					base = value == "primary";
				}
				_ => {}
			},
			None => break,
			Some(Err(err)) => {
				let t_name = t_name.take();
				let t_inherit = t_inherit.take();
				let name_candidate = if base { t_name } else { t_inherit };
				let Some(name) = name_candidate else { break };
				let name = interner().get_or_intern(name).into();
				let start = offset_to_position(ByteOffset(tag_start), document.clone())
					.ok_or_else(|| diagnostic!("qweb_templates start <- tag_start"))?;
				let end = Position {
					line: err.pos().row,
					character: err.pos().col,
				};
				let range = Range { start, end };
				templates.push(NewTemplate {
					base,
					name,
					template: Template {
						location: Some(MinLoc { path, range }),
						descendants: vec![],
					},
				});
				break;
			}
			_ => {}
		}
	}
	Ok(())
}

#[cfg(test)]
mod tests {
	use crate::utils::init_for_test;

	use super::*;

	#[test]
	fn test_gather_templates() {
		let mut templates = vec![];
		let contents = r#"<templates>
			<div t-name="first">
				<div what="nested" />
				<div what="another nested">
					Boo!
				</div>
			</div>
			<span t-name="second">
				Bar!
			</span>
			<t t-name="doesnt_matter" t-inherit="first">
				<t t-if="True">
					Do nothing...
				</t>
			</t>
			<div t-name="primary_inherit" t-inherit="second" t-inherit-mode="extension">
				<div>Nested!</div>
			</div>
		</templates>"#;
		let mut reader = Tokenizer::from(contents);
		let rope = Rope::from_str(contents);
		_ = reader.next();

		gather_templates(PathSymbol::empty(), &mut reader, rope, &mut templates, false).unwrap();

		assert!(
			matches!(
				&templates[..],
				[
					NewTemplate { base: true, .. },
					NewTemplate { base: true, .. },
					NewTemplate { base: true, .. },
					NewTemplate { base: false, .. },
				]
			),
			"{templates:#?}"
		);
		assert_eq!(interner().resolve(&templates[0].name), "first");
		assert_eq!(interner().resolve(&templates[1].name), "second");
		assert_eq!(interner().resolve(&templates[2].name), "doesnt_matter");
		assert_eq!(interner().resolve(&templates[3].name), "second");
	}
	#[test]
	fn test_with_xml_decl() {
		init_for_test();

		let mut templates = vec![];
		let contents = r#"<?xml version="1.0" encoding="UTF-8"?>
<templates id="template" xml:space="preserve">

    <t t-name="Draggable" owl="1">
        <t t-slot="default"></t>
    </t>

</templates>

		"#;
		let mut reader = Tokenizer::from(contents);
		let rope = Rope::from_str(contents);
		_ = reader.next();
		_ = reader.next();

		gather_templates(PathSymbol::empty(), &mut reader, rope, &mut templates, false).unwrap();

		assert!(
			matches!(&templates[..], [NewTemplate { base: true, .. },]),
			"{templates:#?}"
		);
		assert_eq!(interner().resolve(&templates[0].name), "Draggable");
	}
}
