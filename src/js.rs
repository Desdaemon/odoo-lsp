use crate::backend::Backend;
use crate::Text;

use ropey::Rope;
use tower_lsp::lsp_types::Url;
use tree_sitter::Parser;

impl Backend {
	pub async fn on_change_js(&self, text: &Text, uri: &Url, rope: Rope, old_rope: Option<Rope>) -> miette::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(tree_sitter_javascript::language())
			.expect("bug: failed to init js parser");
		self.update_ast(text, uri, rope, old_rope, parser)?;
		Ok(())
	}
}
