use crate::{utils::Usage, ImStr};
use std::{collections::HashSet, fmt::Debug, hash::Hash, marker::PhantomData};

use dashmap::{mapref::one::Ref, DashMap};
use derive_more::{Deref, DerefMut};
use futures::executor::block_on;
use intmap::IntMap;
use lasso::{Key, Spur, ThreadedRodeo};
use smart_default::SmartDefault;
use tokio::sync::RwLock;

use crate::{model::ModelName, record::Record};

use super::Symbol;

#[derive(SmartDefault, Deref)]
pub struct RecordIndex {
	#[deref]
	#[default(_code = "DashMap::with_shard_amount(4)")]
	inner: DashMap<RecordId, Record>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	by_model: DashMap<ModelName, HashSet<RecordId>>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	by_inherit_id: DashMap<RecordId, HashSet<RecordId>>,
	/// unqualified XML ID -> RecordID
	pub by_prefix: RwLock<RecordPrefixTrie>,
}

pub type RecordId = Symbol<Record>;
pub type RecordPrefixTrie = qp_trie::Trie<ImStr, HashSet<RecordId>>;

impl RecordIndex {
	pub async fn insert(&self, qualified_id: RecordId, record: Record, prefix: Option<&mut RecordPrefixTrie>) {
		if let Some(model) = &record.model {
			self.by_model.entry(*model).or_default().insert(qualified_id);
		}
		if let Some(inherit_id) = &record.inherit_id {
			self.by_inherit_id.entry(*inherit_id).or_default().insert(qualified_id);
		}
		if let Some(prefix) = prefix {
			prefix
				.entry(record.id.clone())
				.or_insert_with(Default::default)
				.insert(qualified_id);
		} else {
			self.by_prefix
				.write()
				.await
				.entry(record.id.clone())
				.or_insert_with(Default::default)
				.insert(qualified_id);
		}
		self.inner.insert(qualified_id, record);
	}
	pub async fn append(
		&self,
		prefix: Option<&mut RecordPrefixTrie>,
		records: impl IntoIterator<Item = Record>,
		interner: &ThreadedRodeo,
	) {
		if let Some(prefix) = prefix {
			for record in records {
				let id = interner.get_or_intern(record.qualified_id(interner));
				self.insert(id.into(), record, Some(prefix)).await;
			}
		} else {
			let mut prefix = self.by_prefix.write().await;
			for record in records {
				let id = interner.get_or_intern(record.qualified_id(interner));
				self.insert(id.into(), record, Some(&mut prefix)).await;
			}
		}
	}
	pub fn by_model(&self, model: &ModelName) -> impl Iterator<Item = Ref<RecordId, Record>> {
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
	fn resolve_references<K>(
		&self,
		ids: Ref<K, HashSet<Symbol<Record>>>,
	) -> impl IntoIterator<Item = Ref<RecordId, Record>>
	where
		K: PartialEq + Eq + Hash,
	{
		ids.value()
			.iter()
			.flat_map(|id| self.get(id).into_iter())
			.collect::<Vec<_>>()
	}
}

#[derive(Deref, DerefMut)]
pub struct SymbolMap<K, T = ()>(
	#[deref]
	#[deref_mut]
	IntMap<T>,
	PhantomData<K>,
);

impl<K: Debug, T: Debug> Debug for SymbolMap<K, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_map().entries(self.iter()).finish()
	}
}

#[derive(Deref, DerefMut)]
pub struct SymbolSet<K>(pub(crate) SymbolMap<K, ()>);

impl<K, T> Default for SymbolMap<K, T> {
	#[inline]
	fn default() -> Self {
		Self(Default::default(), PhantomData)
	}
}

impl<K> Default for SymbolSet<K> {
	#[inline]
	fn default() -> Self {
		Self(Default::default())
	}
}

impl<K, T> SymbolMap<K, T> {
	#[inline]
	pub fn get(&self, key: &Symbol<K>) -> Option<&T> {
		self.0.get(key.into_usize() as u64)
	}
	#[inline]
	pub fn get_mut(&mut self, key: &Symbol<K>) -> Option<&mut T> {
		self.0.get_mut(key.into_usize() as u64)
	}
	pub fn keys(&self) -> impl Iterator<Item = Symbol<K>> + '_ {
		self.0
			.iter()
			.map(|(key, _)| Spur::try_from_usize(*key as usize).unwrap().into())
	}
	pub fn iter(&self) -> IterMap<impl Iterator<Item = (&u64, &T)>, K, T> {
		IterMap(self.0.iter(), PhantomData)
	}
}

impl<K> SymbolSet<K> {
	#[inline]
	pub fn insert(&mut self, key: Symbol<K>) -> bool {
		self.0 .0.insert_checked(key.into_usize() as u64, ())
	}
	#[inline]
	pub fn contains_key(&self, key: Symbol<K>) -> bool {
		self.0 .0.contains_key(key.into_usize() as _)
	}
	pub fn iter(&self) -> IterSet<impl Iterator<Item = (&u64, &())>, K> {
		IterSet(self.0 .0.iter(), PhantomData)
	}
	pub fn extend<I>(&mut self, items: I)
	where
		I: IntoIterator<Item = Symbol<K>>,
	{
		(self.0 .0).extend(items.into_iter().map(|key| (key.into_usize() as u64, ())));
	}
}

pub struct IterMap<'a, I, K, T>(I, PhantomData<(&'a K, &'a T)>);

impl<'iter, K, T, I> Iterator for IterMap<'iter, I, K, T>
where
	I: Iterator<Item = (&'iter u64, &'iter T)>,
{
	type Item = (Symbol<T>, &'iter T);

	fn next(&mut self) -> Option<Self::Item> {
		let (next_key, next_value) = self.0.next()?;
		Some((Symbol::from(Spur::try_from_usize(*next_key as _).unwrap()), next_value))
	}
}

pub struct IterSet<'a, I, T>(I, PhantomData<&'a T>);

impl<'iter, T, I> Iterator for IterSet<'iter, I, T>
where
	I: Iterator<Item = (&'iter u64, &'iter ())>,
{
	type Item = Symbol<T>;

	fn next(&mut self) -> Option<Self::Item> {
		let (next, ()) = self.0.next()?;
		Some(Symbol::from(Spur::try_from_usize(*next as _).unwrap()))
	}
}

impl RecordIndex {
	pub(super) fn statistics(&self) -> serde_json::Value {
		let Self {
			inner,
			by_model,
			by_inherit_id,
			by_prefix,
		} = self;
		serde_json::json! {{
			"entries": inner.usage(),
			"by_model": by_model.usage(),
			"by_inherit_id": by_inherit_id.usage(),
			"by_prefix": block_on(by_prefix.read()).usage(),
		}}
	}
}
