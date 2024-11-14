use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use tokio::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

pub struct RwMap<K, V> {
	shards: Box<[tokio::sync::RwLock<HashMap<K, V>>]>,
}

#[repr(transparent)]
pub struct Ref<'a, V, K>(RefImpl<'a, V, K>);

impl<K, V> Deref for Ref<'_, V, K> {
	type Target = V;
	#[inline]
	fn deref(&self) -> &Self::Target {
		self.0.borrow_value()
	}
}

#[repr(transparent)]
pub struct RefMut<'a, V, K>(RefMutImpl<'a, V, K>);

impl<K, V> Deref for RefMut<'_, V, K> {
	type Target = V;
	#[inline]
	fn deref(&self) -> &Self::Target {
		self.0.borrow_value()
	}
}

pub struct SyncRef<'a, T: ?Sized>(NonNull<T>, PhantomData<&'a mut T>);

impl<T: ?Sized> Deref for SyncRef<'_, T> {
	type Target = T;
	#[inline]
	fn deref(&self) -> &Self::Target {
		unsafe { self.0.as_ref() }
	}
}

impl<T: ?Sized> DerefMut for SyncRef<'_, T> {
	#[inline]
	fn deref_mut(&mut self) -> &mut Self::Target {
		unsafe { self.0.as_mut() }
	}
}

impl<K, V> RefMut<'_, V, K> {
	#[inline]
	pub fn as_mut(&mut self) -> SyncRef<V> {
		self.0
			.with_value_mut(|value| SyncRef(unsafe { NonNull::new_unchecked(*value as _) }, PhantomData))
	}
}

#[ouroboros::self_referencing]
struct RefImpl<'a, V, K> {
	guard: RwLockReadGuard<'a, HashMap<K, V>>,
	#[borrows(guard)]
	value: &'this V,
}

#[ouroboros::self_referencing]
struct RefMutImpl<'a, V, K> {
	guard: RwLockWriteGuard<'a, HashMap<K, V>>,
	#[borrows(mut guard)]
	value: &'this mut V,
}

impl<K, V> Default for RwMap<K, V>
where
	K: Hash + Eq,
{
	#[inline]
	fn default() -> Self {
		Self::new()
	}
}

impl<K, V> RwMap<K, V>
where
	K: Hash + Eq,
{
	pub fn new() -> Self {
		Self {
			shards: (0..4).map(|_| Default::default()).collect(),
		}
	}
	#[inline]
	fn get_shard_of<T: Hash + ?Sized>(&self, key: &T) -> &RwLock<HashMap<K, V>> {
		let mut hasher = DefaultHasher::default();
		key.hash(&mut hasher);
		let idx = hasher.finish() as usize % self.shards.len();
		unsafe { self.shards.get_unchecked(idx) }
	}
	pub async fn get<Q>(&self, key: &Q) -> Option<Ref<V, K>>
	where
		K: Borrow<Q>,
		Q: Hash + Eq + ?Sized,
	{
		let guard = self.get_shard_of(key).read().await;
		match guard.contains_key(key) {
			false => None,
			true => Some(Ref(RefImplBuilder {
				guard,
				// SAFETY: Only readers are active, which is guaranteed by read().
				value_builder: |guard| unsafe { guard.get(key).unwrap_unchecked() },
			}
			.build())),
		}
	}
	/// Returns an exclusive reference to the key and keeps it alive.
	/// The resulting reference does not implement [`DerefMut`]; therefore,
	/// you need to call [`as_mut`][RefMut::as_mut] yourself if you want
	/// a [mutable reference][SyncRef]. Note that this reference cannot be sent between threads/tasks.
	pub async fn get_mut<Q>(&self, key: &Q) -> Option<RefMut<V, K>>
	where
		K: Borrow<Q>,
		Q: Hash + Eq + ?Sized,
	{
		let guard = self.get_shard_of(key).write().await;
		match guard.contains_key(key) {
			false => None,
			true => Some(RefMut(
				RefMutImplBuilder {
					guard,
					// SAFETY: We are the only writer, which is guaranteed by write().
					value_builder: |guard| unsafe { guard.get_mut(key).unwrap_unchecked() },
				}
				.build(),
			)),
		}
	}
	pub async fn remove<Q>(&self, key: &Q) -> Option<V>
	where
		K: Borrow<Q>,
		Q: Hash + Eq + ?Sized,
	{
		let shard = self.get_shard_of(key);
		shard.write().await.remove(key)
	}
	pub async fn insert(&self, key: K, value: V) -> Option<V> {
		let shard = self.get_shard_of(key.borrow());
		shard.write().await.insert(key, value)
	}
}
