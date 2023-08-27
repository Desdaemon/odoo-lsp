use std::{ops::Deref, sync::Arc};

use dashmap::{mapref::one::Ref, DashMap, DashSet};
use tokio::sync::RwLock;

use crate::{record::Record, utils::PrefixTrie};

#[derive(Default)]
pub struct RecordIndex {
	inner: DashMap<String, Record>,
	by_model: DashMap<String, DashSet<String>>,
	by_inherit_id: DashMap<String, DashSet<String>>,
	pub by_prefix: Arc<RwLock<PrefixTrie>>,
}

impl Deref for RecordIndex {
	type Target = DashMap<String, Record>;
	#[inline]
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl RecordIndex {
	pub async fn insert(&self, qualified_id: String, record: Record, prefix: Option<&mut PrefixTrie>) {
		if let Some(model) = &record.model {
			self.by_model
				.entry(model.to_string())
				.or_default()
				.insert(qualified_id.to_string());
		}
		if let Some(inherit_id) = &record.inherit_id {
			let inherit_id = match inherit_id {
				(Some(module), xml_id) => format!("{module}.{xml_id}"),
				(None, xml_id) => format!("{}.{xml_id}", record.module),
			};
			self.by_inherit_id
				.entry(inherit_id)
				.or_default()
				.insert(qualified_id.to_string());
		}
		if let Some(prefix) = prefix {
			prefix.insert_str(&qualified_id, qualified_id.to_string());
		} else {
			self.by_prefix
				.write()
				.await
				.insert_str(&qualified_id, qualified_id.to_string());
		}
		self.inner.insert(qualified_id, record);
	}
	pub async fn extend_records(&self, prefix: Option<&mut PrefixTrie>, records: impl IntoIterator<Item = Record>) {
		if let Some(prefix) = prefix {
			for record in records {
				self.insert(record.qualified_id(), record, Some(prefix)).await;
			}
		} else {
			let mut prefix = self.by_prefix.write().await;
			for record in records {
				self.insert(record.qualified_id(), record, Some(&mut prefix)).await;
			}
		}
	}
	pub fn by_model(&self, model: &str) -> impl Iterator<Item = Ref<String, Record>> {
		self.by_model
			.get(model)
			.into_iter()
			.flat_map(|ids| self.resolve_references(ids))
	}
	pub fn by_inherit_id(&self, inherit_id: &str) -> impl Iterator<Item = Ref<String, Record>> {
		self.by_inherit_id
			.get(inherit_id)
			.into_iter()
			.flat_map(|ids| self.resolve_references(ids))
	}
	fn resolve_references(&self, ids: Ref<String, DashSet<String>>) -> impl IntoIterator<Item = Ref<String, Record>> {
		ids.value()
			.iter()
			.flat_map(|id| self.get(id.key()).into_iter())
			.collect::<Vec<_>>()
	}
}
