use std::{
	hash::Hash,
	marker::PhantomData,
	ops::{Deref, DerefMut},
	sync::Arc,
};

use dashmap::{mapref::one::Ref, DashMap};
use intmap::IntMap;
use lasso::{Key, Spur, ThreadedRodeo};
use qp_trie::wrapper::BString;
use tokio::sync::RwLock;

use crate::{record::Record, ImStr};

use super::Symbol;

#[derive(Default)]
pub struct RecordIndex {
	inner: DashMap<RecordId, Record>,
	by_model: DashMap<ImStr, SymbolSet<Record>>,
	by_inherit_id: DashMap<RecordId, SymbolSet<Record>>,
	pub by_prefix: Arc<RwLock<RecordPrefixTrie>>,
}

pub type RecordId = Symbol<Record>;
pub type RecordPrefixTrie = qp_trie::Trie<BString, SymbolSet<Record>>;

impl Deref for RecordIndex {
	type Target = DashMap<RecordId, Record>;
	#[inline]
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl RecordIndex {
	pub async fn insert(
		&self,
		qualified_id: RecordId,
		record: Record,
		prefix: Option<&mut RecordPrefixTrie>,
		interner: &ThreadedRodeo,
	) {
		if let Some(model) = &record.model {
			self.by_model.entry(model.clone()).or_default().insert(qualified_id);
		}
		if let Some(inherit_id) = &record.inherit_id {
			let inherit_id = match inherit_id {
				(Some(module), xml_id) => format!("{module}.{xml_id}"),
				(None, xml_id) => format!("{}.{xml_id}", interner.resolve(&record.module)),
			};
			let inherit_id = interner.get_or_intern(inherit_id);
			self.by_inherit_id
				.entry(inherit_id.into())
				.or_default()
				.insert(qualified_id);
		}
		if let Some(prefix) = prefix {
			prefix
				.entry(BString::from(record.id.as_str()))
				.or_insert_with(SymbolMap::default)
				.insert(qualified_id);
		} else {
			self.by_prefix
				.write()
				.await
				.entry(BString::from(record.id.as_str()))
				.or_insert_with(SymbolSet::default)
				.insert(qualified_id);
		}
		self.inner.insert(qualified_id, record);
	}
	pub async fn extend_records(
		&self,
		prefix: Option<&mut RecordPrefixTrie>,
		records: impl IntoIterator<Item = Record>,
		interner: &ThreadedRodeo,
	) {
		if let Some(prefix) = prefix {
			for record in records {
				let id = interner.get_or_intern(record.qualified_id(interner));
				self.insert(id.into(), record, Some(prefix), interner).await;
			}
		} else {
			let mut prefix = self.by_prefix.write().await;
			for record in records {
				let id = interner.get_or_intern(record.qualified_id(interner));
				self.insert(id.into(), record, Some(&mut prefix), interner).await;
			}
		}
	}
	pub fn by_model(&self, model: &str) -> impl Iterator<Item = Ref<RecordId, Record>> {
		self.by_model
			.get(model)
			.into_iter()
			.flat_map(|ids| self.resolve_references(ids))
	}
	pub fn by_inherit_id(&self, inherit_id: &RecordId) -> impl Iterator<Item = Ref<RecordId, Record>> {
		self.by_inherit_id
			.get(inherit_id)
			.into_iter()
			.flat_map(|ids| self.resolve_references(ids))
	}
	fn resolve_references<K>(&self, ids: Ref<K, SymbolSet<Record>>) -> impl IntoIterator<Item = Ref<RecordId, Record>>
	where
		K: PartialEq + Eq + Hash,
	{
		ids.value()
			.keys()
			.flat_map(|id| self.get(&id).into_iter())
			.collect::<Vec<_>>()
	}
}

pub struct SymbolMap<K, T = ()>(IntMap<T>, PhantomData<K>);
pub type SymbolSet<K> = SymbolMap<K, ()>;
impl<K, T> Deref for SymbolMap<K, T> {
	type Target = IntMap<T>;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}
impl<K, T> DerefMut for SymbolMap<K, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl<K, T> Default for SymbolMap<K, T> {
	fn default() -> Self {
		Self(Default::default(), PhantomData)
	}
}

impl<K, T> SymbolMap<K, T> {
	#[inline]
	pub fn get(&self, key: &Symbol<K>) -> Option<&T> {
		self.0.get(key.into_usize() as u64)
	}
	pub fn keys(&self) -> impl Iterator<Item = Symbol<K>> + '_ {
		self.0
			.iter()
			.map(|(key, _)| Spur::try_from_usize(*key as usize).unwrap().into())
	}
}

impl<K> SymbolSet<K> {
	#[inline]
	pub fn insert(&mut self, key: Symbol<K>) -> bool {
		self.0.insert_checked(key.into_usize() as u64, ())
	}
	// pub fn iter(&self) -> impl Iterator<Item = Symbol<K>> + '_ {
	// 	self.0
	// 		.iter()
	// 		.map(|(key, _)| Spur::try_from_usize(*key as usize).unwrap().into())
	// }
}
