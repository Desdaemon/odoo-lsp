use std::fmt::Display;
use std::ops::Deref;
use std::sync::{Arc, OnceLock};

use dashmap::DashMap;
use intmap::IntMap;
use lasso::Spur;
use miette::{diagnostic, IntoDiagnostic};
use qp_trie::{wrapper::BString, Trie};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Range;
use tree_sitter::{Parser, Query, QueryCursor};

use crate::str::Text;
use crate::utils::{ByteOffset, Erase, MinLoc};
use crate::ImStr;

#[derive(Clone, Debug)]
pub struct Model {
	pub model: ModelId,
	pub range: Range,
	pub byte_range: std::ops::Range<ByteOffset>,
}

#[derive(Clone, Debug)]
pub enum ModelId {
	Base(ImStr),
	Inherit(Vec<ImStr>),
}

#[derive(Default, Clone)]
pub struct ModelIndex {
	inner: DashMap<ImStr, ModelEntry>,
	pub by_prefix: Arc<RwLock<Trie<BString, ImStr>>>,
}

#[derive(Clone, Default)]
pub struct ModelEntry {
	pub base: Option<ModelLocation>,
	pub descendants: Vec<ModelLocation>,
	pub fields: Option<IntMap<Field>>,
	pub docstring: Option<Text>,
}

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
				self.0.path, self.0.range.start.line, self.0.range.start.character, self.0.range.end.character,
			)
		} else {
			write!(
				f,
				"{} [{}:{}..{}:{}]",
				self.0.path,
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.line,
				self.0.range.end.character,
			)
		}
	}
}

impl Deref for ModelIndex {
	type Target = DashMap<ImStr, ModelEntry>;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl ModelIndex {
	pub async fn extend_models<I>(&self, path: ImStr, items: I)
	where
		I: IntoIterator<Item = Model>,
	{
		let mut by_prefix = self.by_prefix.write().await;
		for item in items {
			match item.model {
				ModelId::Base(base) => {
					by_prefix.insert_str(&base, base.clone());
					let mut entry = self.entry(base.clone()).or_default();
					entry.base = Some(ModelLocation(
						MinLoc {
							path: path.clone(),
							range: item.range,
						},
						item.byte_range,
					));
				}
				ModelId::Inherit(inherits) => {
					for inherit in inherits {
						self.entry(inherit).or_default().descendants.push(ModelLocation(
							MinLoc {
								path: path.clone(),
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

fn model_help() -> &'static Query {
	static QUERY: OnceLock<Query> = OnceLock::new();
	QUERY.get_or_init(|| Query::new(tree_sitter_python::language(), include_str!("queries/model_help.scm")).unwrap())
}

impl ModelEntry {
	pub async fn resolve_details(&mut self) -> miette::Result<()> {
		let Some(ModelLocation(loc, byte_range)) = &self.base else {
			return Ok(());
		};
		if self.docstring.is_none() {
			let contents = tokio::fs::read(loc.path.as_str()).await.into_diagnostic()?;
			let mut parser = Parser::new();
			parser.set_language(tree_sitter_python::language()).into_diagnostic()?;
			let ast = parser
				.parse(&contents, None)
				.ok_or_else(|| diagnostic!("AST not parsed"))?;
			let query = model_help();
			let mut cursor = QueryCursor::new();
			cursor.set_byte_range(byte_range.erase());
			'docstring: for match_ in cursor.matches(query, ast.root_node(), &contents[..]) {
				for docstring in match_.nodes_for_capture_index(0) {
					let contents = String::from_utf8_lossy(&contents[docstring.byte_range()]);
					self.docstring = Some(Text::try_from(contents.trim()).into_diagnostic()?);
					break 'docstring;
				}
			}
		}

		Ok(())
	}
}
