//! Estimations for memory usage.
//!
//! Each struct's usage can be divided into two camps: overhead and heap allocations.
//!
//! Overhead comes from the struct's physical size on the stack, and subsequently
//! any of its descendant fields.
//!
//! For example, a String's usage would be its layout size (24 bytes) plus however
//! large its capacity is.
//!
//! Note that stack and heap doesn't have to be absolute: for example, a HashMap's
//! stack size doesn't contribute much to overall usage, so it reports its stack size
//! as the size it would have occupied as a `[(K, V); Capacity]`

use std::{
	alloc::Layout,
	borrow::Borrow,
	cell::UnsafeCell,
	collections::{HashMap, HashSet},
	hash::Hash,
	marker::PhantomData,
	ops::Add,
	path::PathBuf,
	sync::{atomic::AtomicUsize, Arc},
};

use crate::{
	component::{Component, ComponentTemplate, PropDescriptor},
	index::{PathSymbol, Symbol, SymbolMap, SymbolSet},
	model::{Field, FieldKind, ModelEntry, ModelLocation},
	record::Record,
	template::Template,
	ImStr,
};
use dashmap::DashMap;
use ropey::Rope;

use super::MinLoc;

pub struct UsageInfo<T>(pub usize, pub usize, PhantomData<T>);

impl<T> UsageInfo<T> {
	#[inline]
	pub const fn new(heap: usize) -> Self {
		Self(heap, core::mem::size_of::<Self>(), PhantomData)
	}
	#[inline]
	pub const fn new_with_stack(heap: usize, stack: usize) -> Self {
		Self(heap, stack, PhantomData)
	}
}

impl<T> Add for UsageInfo<T> {
	type Output = UsageInfo<T>;

	#[inline]
	fn add(self, rhs: Self) -> Self::Output {
		let UsageInfo(lhs, llen, _) = self;
		let UsageInfo(rhs, rlen, _) = rhs;
		UsageInfo(lhs + rhs, llen + rlen, PhantomData)
	}
}

impl<T> serde::Serialize for UsageInfo<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		use serde::ser::SerializeStruct;
		let Self(bytes, stack, _) = self;
		let fields = if *stack != 0 { 3 } else { 2 };
		let mut st = serializer.serialize_struct("UsageInfo", fields)?;
		st.serialize_field("stack", &stack)?;
		st.serialize_field("bytes", &bytes)?;
		if fields > 2 {
			st.serialize_field("bytes/stack", &(*bytes as f64 / *stack as f64))?;
		}
		st.end()
	}
}

pub trait Usage
where
	Self: Sized,
{
	fn usage(&self) -> UsageInfo<Self>;
}

impl<K, V> Usage for DashMap<K, V>
where
	K: Eq + Hash + Usage,
	V: Usage,
{
	fn usage(&self) -> UsageInfo<Self> {
		#[allow(dead_code)]
		struct RwLockLayout<T> {
			raw: AtomicUsize,
			cell: UnsafeCell<T>,
		}
		let stack = Layout::array::<(K, V)>(self.len()).unwrap().size();
		let mut usage = Layout::array::<RwLockLayout<HashMap<K, V>>>(self.shards().len())
			.unwrap()
			.size();
		usage += stack;
		usage += self
			.iter()
			.map(|entry| entry.key().usage().0 + entry.value().usage().0)
			.sum::<usize>();
		UsageInfo::new_with_stack(usage, stack)
	}
}

impl Usage for String {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo(self.capacity(), 0, PhantomData)
	}
}

impl Usage for Rope {
	fn usage(&self) -> UsageInfo<Self> {
		match self.slice(..).as_str() {
			Some(str) => UsageInfo::new(str.len()),
			None => UsageInfo::new(self.capacity()),
		}
	}
}

impl<T: Usage> Usage for Box<[T]> {
	fn usage(&self) -> UsageInfo<Self> {
		let per_entry = self.iter().map(|entry| entry.usage().0).sum::<usize>();
		let stack = core::alloc::Layout::array::<T>(self.len()).unwrap().size();
		UsageInfo::new_with_stack(stack + per_entry, stack)
	}
}

impl<T> Usage for core::ops::Range<T> {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl Usage for tree_sitter::Tree {
	fn usage(&self) -> UsageInfo<Self> {
		// TODO: Properly measure usage when needed
		UsageInfo::new(0)
	}
}

impl Usage for ImStr {
	fn usage(&self) -> UsageInfo<Self> {
		match &self.0 {
			crate::str::Repr::Arc(value) => UsageInfo::new(core::mem::size_of_val(&**value) + value.len()),
			crate::str::Repr::Inline(..) => UsageInfo::new(0),
		}
	}
}

impl<K, V: Usage> Usage for SymbolMap<K, V> {
	fn usage(&self) -> UsageInfo<Self> {
		let stack = Layout::array::<(u64, V)>(self.len()).unwrap().size();
		let per_entry = self
			.iter()
			.map(|(key, value)| key.usage().0 + value.usage().0)
			.sum::<usize>();
		UsageInfo::new_with_stack(stack + per_entry, stack)
	}
}

impl<K: Usage, V: Usage> Usage for HashMap<K, V> {
	fn usage(&self) -> UsageInfo<Self> {
		let stack = Layout::array::<(K, V)>(self.len()).unwrap().size();
		let per_entry = self
			.iter()
			.map(|(key, value)| key.usage().0 + value.usage().0)
			.sum::<usize>();
		UsageInfo::new_with_stack(stack + per_entry, stack)
	}
}

impl<K: Usage> Usage for HashSet<K> {
	fn usage(&self) -> UsageInfo<Self> {
		let stack = Layout::array::<(K, ())>(self.len()).unwrap().size();
		let per_entry = self.iter().map(|key| key.usage().0).sum::<usize>();
		UsageInfo::new_with_stack(stack + per_entry, stack)
	}
}

impl<K> Usage for SymbolSet<K> {
	fn usage(&self) -> UsageInfo<Self> {
		let stack = Layout::array::<(u64, ())>(self.len()).unwrap().size();
		UsageInfo::new_with_stack(self.0.usage().0, stack)
	}
}

impl Usage for () {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl<T> Usage for Symbol<T> {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl Usage for Record {
	fn usage(&self) -> UsageInfo<Self> {
		let Self {
			deleted: _,
			id,
			module,
			model,
			inherit_id,
			location,
		} = self;
		let mut usage = 0;
		usage += id.usage().0;
		usage += module.usage().0;
		usage += model.usage().0;
		usage += inherit_id.usage().0;
		usage += location.usage().0;
		UsageInfo::new(usage)
	}
}

impl<K: Borrow<[u8]> + Usage, V: Usage> Usage for qp_trie::Trie<K, V> {
	fn usage(&self) -> UsageInfo<Self> {
		// TODO: Improve this approximation, or find a more reliable source
		#[allow(dead_code)]
		enum Tree<K, V> {
			Leaf(K, V),
			// choice, index, branches
			Branch(u64, u32, Vec<Tree<K, V>>),
		}
		let mut usage_by_entries = 0;
		let mut len = 0;
		for (key, value) in self.iter() {
			len += 1;
			usage_by_entries += key.usage().0 + value.usage().0;
		}

		const BRANCHING_FACTOR: usize = 16;
		let branches = len / BRANCHING_FACTOR;
		let usage_per_branch = Layout::array::<Tree<K, V>>(BRANCHING_FACTOR).unwrap().size();
		let leaves = len / 2;
		let usage_per_leaf = core::mem::size_of::<(K, V)>();

		let usage = usage_per_branch * branches + usage_per_leaf * leaves + usage_by_entries;
		let stack = Layout::array::<Tree<K, V>>(len).unwrap().size();
		UsageInfo::new_with_stack(stack + usage, stack)
	}
}

// e.g. borrowed from interner()
impl Usage for &'static [u8] {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl<T: Usage> Usage for Option<T> {
	fn usage(&self) -> UsageInfo<Self> {
		self.as_ref()
			.map(|self_| UsageInfo::new(self_.usage().0))
			.unwrap_or(UsageInfo::new(0))
	}
}

impl<A: Usage, B: Usage> Usage for (A, B) {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(self.0.usage().0 + self.1.usage().0)
	}
}

impl Usage for MinLoc {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(self.path.usage().0)
	}
}

impl Usage for PathSymbol {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl<T: Usage> Usage for Vec<T> {
	fn usage(&self) -> UsageInfo<Self> {
		let stack = Layout::array::<T>(self.len()).unwrap().size();
		let heap = self.iter().map(|val| val.usage().0).sum::<usize>();
		UsageInfo::new_with_stack(stack + heap, stack)
	}
}

impl Usage for Template {
	fn usage(&self) -> UsageInfo<Self> {
		let Self { location, descendants } = self;
		let mut usage = location.usage().0;
		usage += descendants.usage().0;
		UsageInfo::new(usage)
	}
}

impl Usage for ModelEntry {
	fn usage(&self) -> UsageInfo<Self> {
		let Self {
			base,
			descendants,
			ancestors,
			fields,
			fields_set,
			docstring,
		} = self;
		let mut usage = 0;
		usage += base.usage().0;
		usage += descendants.usage().0;
		usage += ancestors.usage().0;
		usage += fields.usage().0;
		usage += fields_set.usage().0;
		usage += docstring.usage().0;
		UsageInfo::new(usage)
	}
}

impl Usage for ModelLocation {
	fn usage(&self) -> UsageInfo<Self> {
		let Self(a) = self;
		let mut usage = 0;
		usage += a.usage().0;
		UsageInfo::new(usage)
	}
}

impl Usage for Field {
	fn usage(&self) -> UsageInfo<Self> {
		let Self {
			kind,
			type_: _,
			location,
			help,
		} = self;

		let mut usage = 0;
		usage += match kind {
			FieldKind::Value => 0,
			FieldKind::Relational(_) => 0,
			FieldKind::Related(str) => str.usage().0,
		};
		usage += location.usage().0;
		usage += help.usage().0;
		UsageInfo::new(usage)
	}
}

impl Usage for Component {
	fn usage(&self) -> UsageInfo<Self> {
		let Self {
			location,
			subcomponents,
			props,
			ancestors,
			template,
			extends,
		} = self;
		let mut usage = 0;
		usage += location.usage().0;
		usage += subcomponents.usage().0;
		usage += props.usage().0;
		usage += ancestors.usage().0;
		usage += template.usage().0;
		usage += extends.usage().0;
		UsageInfo::new(usage)
	}
}

impl Usage for PropDescriptor {
	fn usage(&self) -> UsageInfo<Self> {
		let Self { location, type_: _ } = self;
		let mut usage = 0;
		usage += location.usage().0;
		UsageInfo::new(usage)
	}
}

impl Usage for ComponentTemplate {
	fn usage(&self) -> UsageInfo<Self> {
		match self {
			ComponentTemplate::Name(name) => UsageInfo::new(name.usage().0),
			ComponentTemplate::Inline(inline) => UsageInfo::new(inline.usage().0),
		}
	}
}

impl Usage for tower_lsp::lsp_types::Range {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl Usage for tower_lsp::lsp_types::Diagnostic {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(0)
	}
}

impl<T: Usage> Usage for Arc<T> {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(self.as_ref().usage().0)
	}
}

impl Usage for PathBuf {
	fn usage(&self) -> UsageInfo<Self> {
		UsageInfo::new(self.capacity())
	}
}
