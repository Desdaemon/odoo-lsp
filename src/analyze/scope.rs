use std::{borrow::Borrow, collections::HashMap, fmt::Debug, iter::FusedIterator};

use tracing::instrument;

use super::Type;
use crate::ImStr;

/// Information about an imported symbol
#[derive(Clone, Debug)]
pub struct ImportInfo {
	/// The fully qualified module path (e.g., "odoo.models")
	pub module_path: ImStr,
	/// The name of the symbol being imported (e.g., "Model")
	pub imported_name: ImStr,
	/// The alias if imported as a different name (e.g., "from X import Y as Z")
	pub alias: Option<ImStr>,
}

/// Key for storing imports: (module_path, imported_name) for proper disambiguation
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportKey(pub ImStr, pub ImStr);

/// The current environment, populated from the AST statement by statement.
#[derive(Default, Clone)]
pub struct Scope {
	pub variables: HashMap<String, Type>,
	/// Tracks imported symbols by (module_path, imported_name) key
	pub imports: HashMap<ImportKey, ImportInfo>,
	/// Index from alias or imported_name to ImportKey for quick lookup
	pub import_aliases: HashMap<String, ImportKey>,
	pub parent: Option<Box<Scope>>,
	/// TODO: Allow super(_, \<self>)
	pub super_: Option<ImStr>,
}

impl Debug for Scope {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Scope").field(&"..").finish()
	}
}

impl Scope {
	pub fn new(parent: Option<Scope>) -> Self {
		Self {
			parent: parent.map(Box::new),
			..Default::default()
		}
	}
	pub fn get(&self, key: impl Borrow<str>) -> Option<&Type> {
		fn impl_<'me>(self_: &'me Scope, key: &str) -> Option<&'me Type> {
			self_
				.variables
				.get(key)
				.or_else(|| self_.parent.as_ref().and_then(|parent| parent.get(key)))
		}
		impl_(self, key.borrow())
	}
	/// Get an imported symbol by its alias or name, searching parent scopes if needed
	pub fn get_import(&self, key: impl Borrow<str>) -> Option<&ImportInfo> {
		fn impl_<'me>(self_: &'me Scope, key: &str) -> Option<&'me ImportInfo> {
			// First try to find by alias/name
			if let Some(import_key) = self_.import_aliases.get(key)
				&& let Some(import) = self_.imports.get(import_key)
			{
				return Some(import);
			}
			// Fall back to parent scope
			self_.parent.as_ref().and_then(|parent| parent.get_import(key))
		}
		impl_(self, key.borrow())
	}
	#[instrument(level = "trace", ret, skip(self))]
	pub fn insert(&mut self, key: String, value: Type) {
		self.variables.insert(key, value);
	}
	/// Insert an imported symbol with composite key (module_path, imported_name)
	#[instrument(level = "trace", ret, skip(self))]
	pub fn insert_import(&mut self, import: ImportInfo) {
		let key = ImportKey(import.module_path.clone(), import.imported_name.clone());
		// Index by alias if present, otherwise by the imported name
		let alias_key = import
			.alias
			.as_ref()
			.map(|a| a.as_str().to_string())
			.unwrap_or_else(|| import.imported_name.as_str().to_string());
		self.import_aliases.insert(alias_key, key.clone());
		self.imports.insert(key, import);
	}
	pub fn enter(&mut self, inherit_super: bool) {
		*self = Scope::new(Some(core::mem::take(self)));
		if inherit_super {
			self.super_ = self.parent.as_ref().unwrap().super_.clone();
		}
	}
	pub fn exit(&mut self) {
		if let Some(parent) = self.parent.take() {
			*self = *parent;
		}
	}
	/// Iterates over the current scope and its parents.
	/// If the same variable is defined multiple times in the tree,
	/// each definition is returned from innermost to outermost.
	pub fn iter(&self) -> impl Iterator<Item = (&str, &Type)> {
		Iter {
			variables: self.variables.iter(),
			parent: self.parent.as_deref(),
		}
	}
}

struct Iter<'a> {
	variables: std::collections::hash_map::Iter<'a, String, Type>,
	parent: Option<&'a Scope>,
}

impl FusedIterator for Iter<'_> {}

impl<'a> Iterator for Iter<'a> {
	type Item = (&'a str, &'a Type);
	fn next(&mut self) -> Option<Self::Item> {
		if let Some((key, value)) = self.variables.next() {
			return Some((key.as_str(), value));
		}

		let parent = self.parent.take()?;
		self.parent = parent.parent.as_deref();
		self.variables = parent.variables.iter();
		let (key, value) = self.variables.next()?;
		Some((key.as_str(), value))
	}
}
