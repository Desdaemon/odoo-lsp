use std::{
	alloc::Layout,
	borrow::{Borrow, Cow},
	hash::Hash,
	ops::Add,
};

use crate::{
	component::{Component, ComponentTemplate, PropDescriptor},
	index::{Symbol, SymbolMap},
	model::{Field, FieldKind, ModelEntry, ModelLocation},
	record::Record,
	template::Template,
	ImStr,
};
use dashmap::DashMap;
use qp_trie::wrapper::BString;
use ropey::Rope;

use super::MinLoc;

pub struct UsageInfo(pub usize, pub usize);

impl Add for UsageInfo {
	type Output = UsageInfo;

	#[inline]
	fn add(self, rhs: Self) -> Self::Output {
		let UsageInfo(l0, l1) = self;
		let UsageInfo(r0, r1) = rhs;
		UsageInfo(l0 + r0, l1 + r1)
	}
}

impl serde::Serialize for UsageInfo {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		use serde::ser::SerializeStruct;
		let fields = if self.0 == 0 { 2 } else { 3 };
		let mut st = serializer.serialize_struct("UsageInfo", fields)?;
		st.serialize_field("len", &self.0)?;
		st.serialize_field("bytes", &self.1)?;
		if fields > 2 {
			st.serialize_field("avg", &(self.1 as f64 / self.0 as f64).round())?;
		}
		st.end()
	}
}

pub trait Usage {
	fn usage(&self) -> UsageInfo;
}

impl<K, V> Usage for DashMap<K, V>
where
	K: Eq + Hash + Usage,
	V: Usage,
{
	fn usage(&self) -> UsageInfo {
		let len = self.len();
		let usage = Layout::new::<Self>().size() + Layout::new::<(K, V)>().size() * self.capacity();
		let usage = usage
			+ self
				.iter()
				.map(|entry| entry.key().usage().1 + entry.value().usage().1)
				.sum::<usize>();
		UsageInfo(len, usage)
	}
}

impl Usage for String {
	fn usage(&self) -> UsageInfo {
		UsageInfo(
			self.len(),
			core::mem::size_of::<Self>() + self.capacity() * core::mem::size_of::<u8>(),
		)
	}
}

impl Usage for Rope {
	fn usage(&self) -> UsageInfo {
		match Cow::from(self) {
			Cow::Borrowed(str) => UsageInfo(0, str.as_bytes().len()),
			Cow::Owned(str) => str.usage(),
		}
	}
}

impl<T: Usage> Usage for Box<[T]> {
	fn usage(&self) -> UsageInfo {
		let per_entry = self.iter().map(|entry| entry.usage().1).sum::<usize>();
		UsageInfo(self.len(), Layout::array::<T>(self.len()).unwrap().size() + per_entry)
	}
}

impl<T> Usage for core::ops::Range<T> {
	fn usage(&self) -> UsageInfo {
		UsageInfo(0, Layout::new::<Self>().size())
	}
}

impl Usage for tree_sitter::Tree {
	fn usage(&self) -> UsageInfo {
		UsageInfo(0, 0)
	}
}

impl Usage for ImStr {
	fn usage(&self) -> UsageInfo {
		UsageInfo(self.len(), self.as_bytes().len())
	}
}

impl<K, V: Usage> Usage for SymbolMap<K, V> {
	fn usage(&self) -> UsageInfo {
		let len = self.len();
		let usage = Layout::new::<(u64, V)>().size() * self.capacity();
		let usage = usage + self.iter().map(|(_, value)| value.usage().1).sum::<usize>();
		UsageInfo(len, usage)
	}
}

impl Usage for () {
	fn usage(&self) -> UsageInfo {
		UsageInfo(0, 0)
	}
}

impl<T> Usage for Symbol<T> {
	fn usage(&self) -> UsageInfo {
		UsageInfo(0, core::mem::size_of::<u64>())
	}
}

impl Usage for Record {
	fn usage(&self) -> UsageInfo {
		let mut usage = Layout::new::<Self>().size();
		let Self {
			deleted: _,
			id,
			module,
			model,
			inherit_id,
			location,
		} = self;
		usage += id.usage().1;
		usage += module.usage().1;
		usage += model.usage().1;
		usage += inherit_id.usage().1;
		usage += location.usage().1;
		UsageInfo(0, usage)
	}
}

impl<K: Borrow<[u8]> + Usage, V: Usage> Usage for qp_trie::Trie<K, V> {
	fn usage(&self) -> UsageInfo {
		let len = self.count();
		// TODO: Wildly inaccurate, only useful for an approximate.
		let usage = Layout::new::<Self>().size() + len.next_power_of_two() * Layout::new::<(K, V)>().size();
		let usage = usage
			+ self
				.iter()
				.map(|(key, val)| key.usage().1 + val.usage().1)
				.sum::<usize>();
		UsageInfo(len, usage)
	}
}

impl Usage for BString {
	fn usage(&self) -> UsageInfo {
		UsageInfo(0, Borrow::<[u8]>::borrow(self).len())
	}
}

impl<T: Usage> Usage for Option<T> {
	fn usage(&self) -> UsageInfo {
		self.as_ref().map(|self_| self_.usage()).unwrap_or(UsageInfo(0, 0))
	}
}

impl<A: Usage, B: Usage> Usage for (A, B) {
	fn usage(&self) -> UsageInfo {
		let mut usage = Layout::new::<(A, B)>().size();
		usage += self.0.usage().1;
		usage += self.1.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for MinLoc {
	fn usage(&self) -> UsageInfo {
		let Self { path, range } = self;
		let usage = Layout::new::<Self>().size() + core::mem::size_of_val(&path);
		UsageInfo(0, usage + core::mem::size_of_val(&range))
	}
}

impl<T: Usage> Usage for Vec<T> {
	fn usage(&self) -> UsageInfo {
		let mut usage = Layout::new::<Self>().size();
		usage += Layout::array::<T>(self.capacity()).unwrap().size();
		usage += usage + self.iter().map(|val| val.usage().1).sum::<usize>();
		UsageInfo(self.len(), usage)
	}
}

impl Usage for Template {
	fn usage(&self) -> UsageInfo {
		let Self { location, descendants } = self;
		let mut usage = Layout::new::<Template>().size();
		usage += location.usage().1;
		usage += descendants.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for ModelEntry {
	fn usage(&self) -> UsageInfo {
		let Self {
			base,
			descendants,
			ancestors,
			fields,
			fields_set,
			docstring,
		} = self;
		let mut usage = Layout::new::<Self>().size();
		usage += base.usage().1;
		usage += descendants.usage().1;
		usage += ancestors.usage().1;
		usage += fields.usage().1;
		usage += fields_set.usage().1;
		usage += docstring.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for ModelLocation {
	fn usage(&self) -> UsageInfo {
		let Self(a, b) = self;
		let mut usage = Layout::new::<Self>().size();
		usage += a.usage().1;
		usage += b.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for Field {
	fn usage(&self) -> UsageInfo {
		let Self {
			kind,
			type_: _,
			location,
			help,
		} = self;

		let mut usage = Layout::new::<Self>().size();
		usage += match kind {
			FieldKind::Value => 0,
			FieldKind::Relational(_) => 0,
			FieldKind::Related(str) => str.usage().1,
		};
		usage += location.usage().1;
		usage += help.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for Component {
	fn usage(&self) -> UsageInfo {
		let Self {
			location,
			subcomponents,
			props,
			ancestors,
			template,
		} = self;
		let mut usage = Layout::new::<Self>().size();
		usage += location.usage().1;
		usage += subcomponents.usage().1;
		usage += props.usage().1;
		usage += ancestors.usage().1;
		usage += template.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for PropDescriptor {
	fn usage(&self) -> UsageInfo {
		let Self { location, type_: _ } = self;
		let mut usage = Layout::new::<Self>().size();
		usage += location.usage().1;
		UsageInfo(0, usage)
	}
}

impl Usage for ComponentTemplate {
	fn usage(&self) -> UsageInfo {
		UsageInfo(0, core::mem::size_of::<Self>())
	}
}
