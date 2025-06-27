use std::sync::atomic::AtomicBool;

use crate::{index::Symbol, ImStr};

#[derive(Debug)]
pub struct ModuleEntry {
    /// Path relative to root
    pub path: ImStr,
    pub dependencies: Box<[Symbol<ModuleEntry>]>,
    pub loaded: AtomicBool,
}