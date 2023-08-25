use std::{
	fmt::Display,
	ops::{Deref, DerefMut},
};

use dashmap::DashMap;
use log::warn;
use tower_lsp::lsp_types::{Location, Range, Url};

#[derive(Clone, Debug)]
pub struct Model {
	pub model: String,
	pub range: Range,
	pub inherit: bool,
}

#[derive(Default, Clone)]
pub struct ModelIndex(DashMap<String, (Option<ModelLocation>, Vec<ModelLocation>)>);

#[derive(Clone)]
pub struct ModelLocation(pub Location);

impl Display for ModelLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.0.range.start.line == self.0.range.end.line {
			write!(
				f,
				"{} [{}:{}..{}]",
				self.0.uri.path(),
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.character,
			)
		} else {
			write!(
				f,
				"{} [{}:{}..{}:{}]",
				self.0.uri.path(),
				self.0.range.start.line,
				self.0.range.start.character,
				self.0.range.end.line,
				self.0.range.end.character,
			)
		}
	}
}

impl Deref for ModelIndex {
	type Target = DashMap<String, (Option<ModelLocation>, Vec<ModelLocation>)>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl DerefMut for ModelIndex {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl ModelIndex {
	pub fn extend_models<I>(&self, uri: &Url, items: I)
	where
		I: IntoIterator<Item = Model>,
	{
		for item in items {
			let mut entry = self.entry(item.model).or_default();
			if item.inherit {
				entry.1.push(ModelLocation(Location {
					uri: uri.clone(),
					range: item.range,
				}));
			} else if let Some(base) = &entry.0 {
				warn!("{} already defined at {base}", entry.key());
			} else {
				entry.0 = Some(ModelLocation(Location {
					uri: uri.clone(),
					range: item.range,
				}))
			}
		}
	}
}
