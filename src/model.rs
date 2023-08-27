use std::{fmt::Display, ops::Deref, sync::Arc};

use dashmap::DashMap;
use qp_trie::{wrapper::BString, Trie};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Range;

use crate::utils::{ImStr, MinLoc};

#[derive(Clone, Debug)]
pub struct Model {
	pub model: ModelId,
	pub range: Range,
}

#[derive(Clone, Debug)]
pub enum ModelId {
	Base(ImStr),
	Inherit(Vec<ImStr>),
}

#[derive(Default, Clone)]
pub struct ModelIndex {
	inner: DashMap<ImStr, (Option<ModelLocation>, Vec<ModelLocation>)>,
	pub by_prefix: Arc<RwLock<Trie<BString, ImStr>>>,
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
	type Target = DashMap<ImStr, (Option<ModelLocation>, Vec<ModelLocation>)>;

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
					let mut location = Some(ModelLocation(MinLoc {
						path: path.clone(),
						range: item.range,
					}));
					core::mem::swap(&mut self.entry(base.clone()).or_default().0, &mut location);
					#[cfg(debug_assertions)]
					if let Some(leftover) = location {
						log::debug!("leftover base class {leftover}");
					}
				}
				ModelId::Inherit(inherits) => {
					for inherit in inherits.iter() {
						self.entry(inherit.clone()).or_default().1.push(ModelLocation(MinLoc {
							path: path.clone(),
							range: item.range,
						}))
					}
				}
			}
		}
	}
}
