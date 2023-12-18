use crate::utils::Usage;
use std::{
	hash::Hash,
	marker::PhantomData,
	ops::{Deref, DerefMut},
};

use dashmap::{mapref::one::Ref, DashMap};
use futures::executor::block_on;
use intmap::IntMap;
use lasso::{Key, Spur, ThreadedRodeo};
use qp_trie::wrapper::BString;
use tokio::sync::RwLock;

use crate::{model::ModelName, record::Record};

use super::{interner, Symbol};

#[derive(Default)]
pub struct RecordIndex {
	inner: DashMap<RecordId, Record>,
	by_model: DashMap<ModelName, SymbolSet<Record>>,
	by_inherit_id: DashMap<RecordId, SymbolSet<Record>>,
	pub by_prefix: RwLock<RecordPrefixTrie>,
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
	pub async fn insert(&self, qualified_id: RecordId, record: Record, prefix: Option<&mut RecordPrefixTrie>) {
		let interner = interner();
		if let Some(model) = &record.model {
			self.by_model.entry(*model).or_default().insert(qualified_id);
		}
		if let Some(inherit_id) = &record.inherit_id {
			let inherit_id = match inherit_id {
				(Some(module), xml_id) => format!("{}.{xml_id}", interner.resolve(module)),
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
	#[inline]
	pub fn get_mut(&mut self, key: &Symbol<K>) -> Option<&mut T> {
		self.0.get_mut(key.into_usize() as u64)
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
	#[inline]
	pub fn contains_key(&self, key: Symbol<K>) -> bool {
		self.0.contains_key(key.into_usize() as _)
	}
	pub fn iter(&self) -> Iter<impl Iterator<Item = (&u64, &())>, K> {
		Iter(self.0.iter(), PhantomData)
	}
	pub fn extend<I>(&mut self, items: I)
	where
		I: IntoIterator<Item = Symbol<K>>,
	{
		self.0
			.extend(items.into_iter().map(|key| (key.into_usize() as u64, ())));
	}
}

pub struct Iter<'a, I, T>(I, PhantomData<&'a T>);

impl<'iter, T, I> Iterator for Iter<'iter, I, T>
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
