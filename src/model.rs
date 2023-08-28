use std::{fmt::Display, ops::Deref, sync::Arc};

use dashmap::DashMap;
use intmap::IntMap;
use lasso::{Key, Spur};
use qp_trie::{wrapper::BString, Trie};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Range;

use crate::{utils::MinLoc, ImStr};

#[derive(Clone, Debug)]
pub struct Model {
	pub model: ModelId,
	pub range: Range,
	pub fields: Vec<(Spur, Field)>,
}

#[derive(Clone, Debug)]
pub enum ModelId {
	Base(ImStr),
	Inherit { inherits: Vec<ImStr>, has_primary: bool },
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
	pub fields: IntMap<Field>,
}

#[derive(Clone, Debug)]
pub enum Field {
	Value,
	Relational(Spur),
}

#[derive(Clone)]
pub struct ModelLocation(pub MinLoc);

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
					entry.base = Some(ModelLocation(MinLoc {
						path: path.clone(),
						range: item.range,
					}));
					for (field, type_) in item.fields {
						entry.fields.insert(field.into_usize() as u64, type_);
					}
				}
				ModelId::Inherit { inherits, has_primary } => {
					if has_primary {
						let primary = &inherits[0];
						let mut entry = self.entry(primary.clone()).or_default();
						for (field, type_) in item.fields {
							entry.fields.insert(field.into_usize() as u64, type_);
						}
					}
					for inherit in inherits {
						self.entry(inherit).or_default().descendants.push(ModelLocation(MinLoc {
							path: path.clone(),
							range: item.range,
						}));
					}
				}
			}
		}
	}
}
