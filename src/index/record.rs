use std::{ops::Deref, sync::Arc};

use dashmap::{mapref::one::Ref, DashMap, DashSet};
use qp_trie::wrapper::BString;
use tokio::sync::RwLock;

use crate::{
	record::Record,
	utils::{ImStr, PrefixTrie},
};

#[derive(Default)]
pub struct RecordIndex {
	inner: DashMap<ImStr, Record>,
	by_model: DashMap<ImStr, DashSet<ImStr>>,
	by_inherit_id: DashMap<ImStr, DashSet<ImStr>>,
	pub by_prefix: Arc<RwLock<PrefixTrie>>,
}

impl Deref for RecordIndex {
	type Target = DashMap<ImStr, Record>;
	#[inline]
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl RecordIndex {
	pub async fn insert(&self, qualified_id: ImStr, record: Record, prefix: Option<&mut PrefixTrie>) {
		if let Some(model) = &record.model {
			self.by_model
				.entry(model.clone())
				.or_default()
				.insert(qualified_id.clone());
		}
		if let Some(inherit_id) = &record.inherit_id {
			let inherit_id = match inherit_id {
				(Some(module), xml_id) => format!("{module}.{xml_id}"),
				(None, xml_id) => format!("{}.{xml_id}", record.module),
			};
			self.by_inherit_id
				.entry(inherit_id.into())
				.or_default()
				.insert(qualified_id.clone());
		}
		if let Some(prefix) = prefix {
			prefix
				.entry(BString::from(record.id.as_str()))
				.or_insert_with(DashSet::default)
				.insert(qualified_id.clone());
		} else {
			self.by_prefix
				.write()
				.await
				.entry(BString::from(record.id.as_str()))
				.or_insert_with(DashSet::default)
				.insert(qualified_id.clone());
		}
		self.inner.insert(qualified_id.into(), record);
	}
	pub async fn extend_records(&self, prefix: Option<&mut PrefixTrie>, records: impl IntoIterator<Item = Record>) {
		if let Some(prefix) = prefix {
			for record in records {
				self.insert(record.qualified_id().into(), record, Some(prefix)).await;
			}
		} else {
			let mut prefix = self.by_prefix.write().await;
			for record in records {
				self.insert(record.qualified_id().into(), record, Some(&mut prefix))
					.await;
			}
		}
	}
	pub fn by_model(&self, model: &str) -> impl Iterator<Item = Ref<ImStr, Record>> {
		self.by_model
			.get(model)
			.into_iter()
			.flat_map(|ids| self.resolve_references(ids))
	}
	pub fn by_inherit_id(&self, inherit_id: &str) -> impl Iterator<Item = Ref<ImStr, Record>> {
		self.by_inherit_id
			.get(inherit_id)
			.into_iter()
			.flat_map(|ids| self.resolve_references(ids))
	}
	fn resolve_references(&self, ids: Ref<ImStr, DashSet<ImStr>>) -> impl IntoIterator<Item = Ref<ImStr, Record>> {
		ids.value()
			.iter()
			.flat_map(|id| self.get(id.key()).into_iter())
			.collect::<Vec<_>>()
	}
}
