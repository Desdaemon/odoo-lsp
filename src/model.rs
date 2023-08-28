use std::{fmt::Display, ops::Deref, sync::Arc};

use dashmap::DashMap;
use intmap::IntMap;
use lasso::Spur;
use qp_trie::{wrapper::BString, Trie};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Range;

use crate::{
	utils::{ByteOffset, MinLoc},
	ImStr,
};

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
}

#[derive(Clone, Debug)]
pub enum Field {
	Value,
	Relational(Spur),
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
