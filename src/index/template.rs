use derive_more::Deref;

use async_lock::RwLock;
use dashmap::DashMap;
use smart_default::SmartDefault;

use crate::template::NewTemplate;

use super::{Template, TemplateName, _R};

#[derive(SmartDefault, Deref)]
pub struct TemplateIndex {
	#[deref]
	#[default(_code = "DashMap::with_shard_amount(4)")]
	inner: DashMap<TemplateName, Template>,
	pub by_prefix: RwLock<TemplatePrefixTrie>,
}

pub type TemplatePrefixTrie = qp_trie::Trie<&'static [u8], TemplateName>;

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
			let raw = _R(entry.name);
			prefix.insert(raw.as_bytes(), entry.name);
		}
	}
}
