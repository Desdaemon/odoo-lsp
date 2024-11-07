use std::path::PathBuf;

use lasso::Spur;
use miette::diagnostic;
use tree_sitter::{Parser, QueryCursor};
use ts_macros::query;

use crate::index::PathSymbol;
use crate::utils::{ts_range_to_lsp_range, MinLoc, RangeExt};
use crate::{format_loc, ok, ImStr};

use super::Output;

#[rustfmt::skip]
query! {
    #[lang = "tree_sitter_javascript::language"]
    RegistryQuery(Category, Field);

// registry.category(CATEGORY).add(FIELD, ..)
(call_expression
	(member_expression
    	(call_expression
        	(member_expression (identifier) @_registry (property_identifier) @_category
        	    (#eq? @_registry "registry")
        	    (#eq? @_category "category"))
            (arguments . (string) @CATEGORY .))
        (property_identifier) @_add (#eq? @_add "add"))
	(arguments . (string) @FIELD))
}

pub(super) async fn add_root_js(root: Spur, path: PathBuf) -> miette::Result<Output> {
	let contents = ok!(tokio::fs::read(&path).await, "Could not read {:?}", path);
	let path = PathSymbol::strip_root(root, &path);
	let mut parser = Parser::new();
	ok!(parser.set_language(tree_sitter_javascript::language()));
	let ast = parser
		.parse(&contents, None)
		.ok_or_else(|| diagnostic!("AST not parsed"))?;
	let query = RegistryQuery::query();
	let mut cursor = QueryCursor::new();
	let mut widgets = Vec::new();
	let mut actions = Vec::new();

	for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		let mut category = None;
		for capture in match_.captures {
			// always a string
			let range = capture.node.byte_range().shrink(1);
			match RegistryQuery::from(capture.index) {
				Some(RegistryQuery::Category) => {
					category = Some(&contents[range]);
				}
				Some(RegistryQuery::Field) => {
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
				None => {}
			}
		}
	}

	Ok(Output::Registries { widgets, actions })
}
