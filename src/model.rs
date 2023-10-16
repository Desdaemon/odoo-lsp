use std::fmt::Display;
use std::ops::Deref;
use std::sync::Arc;

use dashmap::DashMap;
use lasso::Spur;
use log::debug;
use miette::{diagnostic, IntoDiagnostic};
use qp_trie::{wrapper::BString, Trie};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Range;
use tree_sitter::{Parser, QueryCursor};
use ts_macros::query;

use crate::index::{interner, Interner, Symbol, SymbolMap};
use crate::str::Text;
use crate::utils::{ByteOffset, ByteRange, Erase, MinLoc};
use crate::ImStr;

#[derive(Clone, Debug)]
pub struct Model {
	pub type_: ModelType,
	pub range: Range,
	pub byte_range: ByteRange,
}

#[derive(Clone, Debug)]
pub enum ModelType {
	Base(ImStr),
	Inherit(Vec<ImStr>),
}

#[derive(Default)]
pub struct ModelIndex {
	inner: DashMap<ModelName, ModelEntry>,
	pub by_prefix: Arc<RwLock<Trie<BString, ModelName>>>,
}

pub type ModelName = Symbol<ModelEntry>;

#[derive(Default)]
pub struct ModelEntry {
	pub base: Option<ModelLocation>,
	pub descendants: Vec<ModelLocation>,
	pub fields: Option<SymbolMap<FieldName, Field>>,
	pub docstring: Option<Text>,
}

pub enum FieldName {}

#[derive(Clone, Debug)]
pub enum FieldKind {
	Value,
	Relational(Spur),
}

#[derive(Clone, Debug)]
pub struct Field {
	pub kind: FieldKind,
	pub type_: Spur,
	pub location: MinLoc,
	pub help: Option<Text>,
}

#[derive(Clone)]
pub struct ModelLocation(pub MinLoc, pub std::ops::Range<ByteOffset>);

impl Display for ModelLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.0.range.start.line == self.0.range.end.line {
			write!(
				f,
				"{} [{}:{}..{}]",
				interner().resolve(&self.0.path),
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.character,
			)
		} else {
			write!(
				f,
				"{} [{}:{}..{}:{}]",
				interner().resolve(&self.0.path),
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.line,
				self.0.range.end.character,
			)
		}
	}
}

impl Deref for ModelIndex {
	type Target = DashMap<ModelName, ModelEntry>;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl ModelIndex {
	pub async fn append(&self, path: Spur, interner: &Interner, replace: bool, items: &[Model]) {
		let mut by_prefix = self.by_prefix.write().await;
		for item in items {
			match &item.type_ {
				ModelType::Base(base) => {
					let name = interner.get_or_intern(base).into();
					by_prefix.insert_str(base, name);
					let mut entry = self.entry(name).or_default();
					if entry.base.is_none() || replace {
						entry.base = Some(ModelLocation(
							MinLoc {
								path,
								range: item.range,
							},
							item.byte_range.clone(),
						));
					} else {
						debug!(
							"Conflicting bases:\nfirst={}\n  new={}",
							entry.base.as_ref().unwrap(),
							ModelLocation(
								MinLoc {
									path,
									range: item.range
								},
								item.byte_range.clone()
							)
						)
					}
				}
				ModelType::Inherit(inherits) => {
					if replace {
						for inherit in inherits {
							let Some(inherit) = interner.get(inherit) else { continue };
							if let Some(mut entry) = self.get_mut(&inherit.into()) {
								entry.descendants.retain(|loc| loc.0.path != path)
							}
						}
					}
					for inherit in inherits {
						let inherit = interner.get_or_intern(inherit).into();
						self.entry(inherit).or_default().descendants.push(ModelLocation(
							MinLoc {
								path,
								range: item.range,
							},
							item.byte_range.clone(),
						));
					}
				}
			}
		}
	}
}

query! {
	ModelHelp(DOCSTRING);
r#"
(class_definition
  (block .
    (expression_statement
      (string
        ((string_start) . (string_content) @DOCSTRING)))))"#
}

impl ModelEntry {
	pub async fn resolve_details(&mut self) -> miette::Result<()> {
		let Some(ModelLocation(loc, byte_range)) = &self.base else {
			return Ok(());
		};
		if self.docstring.is_none() {
			let contents = tokio::fs::read(interner().resolve(&loc.path)).await.into_diagnostic()?;
			let mut parser = Parser::new();
			parser.set_language(tree_sitter_python::language()).into_diagnostic()?;
			let ast = parser
				.parse(&contents, None)
				.ok_or_else(|| diagnostic!("AST not parsed"))?;
			let query = ModelHelp::query();
			let mut cursor = QueryCursor::new();
			cursor.set_byte_range(byte_range.erase());
			'docstring: for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
				if let Some(docstring) = match_.nodes_for_capture_index(0).next() {
					let contents = String::from_utf8_lossy(&contents[docstring.byte_range()]);
					self.docstring = Some(Text::try_from(contents.trim()).into_diagnostic()?);
					break 'docstring;
				}
			}
		}

		Ok(())
	}
}
