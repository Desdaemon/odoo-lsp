use std::sync::atomic::AtomicBool;

use crate::{ImStr, index::ModuleName};

#[derive(Debug)]
pub struct ModuleEntry {
	/// Path relative to root
	pub path: ImStr,
	pub dependencies: Box<[ModuleName]>,
	pub loaded: AtomicBool,
	pub loaded_dependents: AtomicBool,
}
