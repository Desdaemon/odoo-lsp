use derive_more::Deref;

use dashmap::DashMap;
use smart_default::SmartDefault;
use std::sync::RwLock;

use crate::{format_loc, template::NewTemplate};

use super::{_R, Template, TemplateName};

#[derive(SmartDefault, Deref)]
pub struct TemplateIndex {
	#[deref]
	#[default(_code = "DashMap::with_shard_amount(4)")]
	inner: DashMap<TemplateName, Template>,
	pub by_prefix: RwLock<TemplatePrefixTrie>,
}

pub type TemplatePrefixTrie = qp_trie::Trie<&'static [u8], TemplateName>;

impl TemplateIndex {
	pub fn append(&self, entries: Vec<NewTemplate>) {
		let mut prefix = self
			.by_prefix
			.write()
			.expect(format_loc!("cannot acquire write lock now"));
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
