use std::{path::PathBuf, sync::Arc};

use dashmap::DashMap;
use lasso::Spur;
use odoo_lsp::{
	ImStr,
	index::{_I, _R},
	utils::RangeExt,
};
use tracing::debug;
use tree_sitter::{Parser, QueryCursor};
use ts_macros::query;

/// path -> \[defines]
pub type DefineIndex = DashMap<Spur, Vec<ImStr>>;

#[rustfmt::skip]
query! {
	#[lang = "tree_sitter_javascript"]
	OdooDefines(Pragma, Name);
((program . (comment) @PRAGMA)
  (#match? @PRAGMA "odoo-module alias="))

((program
  (expression_statement
	(call_expression
	  (member_expression
		(identifier) @_odoo (property_identifier) @_define)
	  (arguments . (string) @NAME))))
  (#eq? @_odoo "odoo")
  (#eq? @_define "define"))
}

pub(super) async fn gather_defines(file: PathBuf, index: Arc<DefineIndex>) -> anyhow::Result<()> {
	let contents = tokio::fs::read(&file).await?;

	let file = _I(file.to_string_lossy());

	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_javascript::LANGUAGE.into())?;
	let ast = parser.parse(&contents, None).expect("AST not parsed");
	let query = OdooDefines::query();
	let mut cursor = QueryCursor::new();

	for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		debug!("{} captures in {}", match_.captures.len(), _R(file));
		if let Some(name) = match_.nodes_for_capture_index(OdooDefines::Name as _).next() {
			let name = String::from_utf8_lossy(&contents[name.byte_range().shrink(1)]);
			index.entry(file).or_default().push(name.as_ref().into());
		} else if let Some(pragma) = match_.nodes_for_capture_index(OdooDefines::Pragma as _).next() {
			let text = &contents[pragma.byte_range()];
			// find alias=..
			if let Some(alias) = text.split(|c| *c == b' ').find(|token| token.starts_with(b"alias")) {
				if let Some(alias) = alias.strip_prefix(b"alias=") {
					let alias = alias
						// the class of characters allowed for a module name, i.e. [\w.]
						.split(|c| !matches!(*c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'.' | b'_'))
						.next()
						.unwrap_or(alias);
					let alias = String::from_utf8_lossy(alias);
					index.entry(file).or_default().push(alias.as_ref().into());
				}
			}
		}
	}

	Ok(())
}
