use std::{path::PathBuf, sync::Arc};

use dashmap::DashMap;
use lasso::Spur;
use log::debug;
use miette::IntoDiagnostic;
use odoo_lsp::{index::interner, utils::RangeExt, ImStr};
use tree_sitter::{Parser, QueryCursor};
use ts_macros::query_js;

/// path -> \[defines]
pub type DefineIndex = DashMap<Spur, Vec<ImStr>>;

query_js! {
	OdooDefines(NAME, PRAGMA);
r#"
((program . (comment) @PRAGMA)
(#match? @PRAGMA "@odoo-module alias="))

((program (expression_statement
	(call_expression
		(member_expression (identifier) @_odoo (property_identifier) @_define)
		(arguments . (string) @NAME))))
(#eq? @_odoo "odoo")
(#eq? @_define "define"))
	"#
}

pub(super) async fn gather_defines(file: PathBuf, index: Arc<DefineIndex>) -> miette::Result<()> {
	let contents = tokio::fs::read(&file).await.into_diagnostic()?;

	let file = interner().get_or_intern(&file.to_string_lossy());

	let mut parser = Parser::new();
	parser
		.set_language(tree_sitter_javascript::language())
		.into_diagnostic()?;
	let ast = parser.parse(&contents, None).expect("AST not parsed");
	let query = OdooDefines::query();
	let mut cursor = QueryCursor::new();

	for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		debug!("{} captures in {}", match_.captures.len(), interner().resolve(&file));
		if let Some(name) = match_.nodes_for_capture_index(OdooDefines::NAME).next() {
			let range = name.byte_range().shrink(1);
			if !range.is_empty() {
				let name = String::from_utf8_lossy(&contents[range]);
				index.entry(file).or_default().push(name.as_ref().into());
			}
		} else if let Some(pragma) = match_.nodes_for_capture_index(OdooDefines::PRAGMA).next() {
			let text = &contents[pragma.byte_range()];
			// find alias=..
			if let Some(alias) = text.split(|c| *c == b' ').find(|token| token.starts_with(b"alias")) {
				if let Some(alias) = alias.strip_prefix(b"alias=") {
					let alias = String::from_utf8_lossy(alias);
					index.entry(file).or_default().push(alias.as_ref().into());
				}
			}
		}
	}

	Ok(())
}
