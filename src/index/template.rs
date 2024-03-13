use std::collections::HashSet;

use derive_more::Deref;

use dashmap::DashMap;
use futures::executor::block_on;
use tokio::sync::RwLock;

use crate::{template::NewTemplate, utils::Usage};

use super::{interner, Template, TemplateName};

#[derive(Default, Deref)]
pub struct TemplateIndex {
	#[deref]
	inner: DashMap<TemplateName, Template>,
	pub by_prefix: RwLock<TemplatePrefixTrie>,
}

pub type TemplatePrefixTrie = qp_trie::Trie<&'static [u8], HashSet<TemplateName>>;

impl TemplateIndex {
	pub async fn append(&self, entries: Vec<NewTemplate>) {
		let mut prefix = self.by_prefix.write().await;
		for entry in entries {
			if entry.base {
				let Template {
					location,
					mut descendants,
				} = entry.template;
				let mut entry = self.entry(entry.name).or_default();
				// TODO: same t-name, t-inherit-mode=primary
				entry.location = entry.location.take().or(location);
				entry.descendants.append(&mut descendants);
			} else {
				self.entry(entry.name).or_default().descendants.push(entry.template);
			}
			let raw = interner().resolve(&entry.name);
			prefix
				.entry(raw.as_bytes())
				.or_insert_with(Default::default)
				.insert(entry.name);
		}
	}
	pub(super) fn statistics(&self) -> serde_json::Value {
		let Self { inner, by_prefix } = self;
		serde_json::json! {{
			"entries": inner.usage(),
			"by_prefix": block_on(by_prefix.read()).usage(),
		}}
	}
}
