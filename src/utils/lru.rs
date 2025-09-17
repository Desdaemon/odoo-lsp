use std::collections::{HashMap, VecDeque};

pub struct LruCache<K, V> {
	map: HashMap<K, V>,
	order: VecDeque<K>,
	capacity: usize,
}

impl<K, V> Default for LruCache<K, V>
where
	K: Clone + Eq + std::hash::Hash,
{
	#[inline]
	fn default() -> Self {
		Self::new(16)
	}
}

impl<K: Clone + Eq + std::hash::Hash, V> LruCache<K, V> {
	pub fn new(capacity: usize) -> Self {
		Self {
			map: Default::default(),
			order: Default::default(),
			capacity,
		}
	}

	pub fn get(&mut self, key: &K) -> Option<&V> {
		let item = self.map.get(key);
		if item.is_some() {
			// let mut order = self.order.write();
			// move key to the back (most recent)
			self.order.retain(|k| k != key);
			self.order.push_back(key.clone());
		}
		item
	}

	pub fn put(&mut self, key: K, value: V) {
		if self.map.contains_key(&key) {
			// update existing, move key
			self.order.retain(|k| k != &key);
		} else if self.map.len() == self.capacity {
			// evict least recent
			if let Some(old) = self.order.pop_front() {
				self.map.remove(&old);
			}
		}
		self.map.insert(key.clone(), value);
		self.order.push_back(key);
	}
}
