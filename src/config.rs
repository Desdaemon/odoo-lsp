use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
	pub module: Option<ModuleConfig>,
	pub symbols: Option<SymbolsConfig>,
	pub references: Option<ReferencesConfig>,
	pub completions: Option<CompletionsConfig>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ModuleConfig {
	pub roots: Option<Vec<String>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SymbolsConfig {
	pub limit: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ReferencesConfig {
	pub limit: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CompletionsConfig {
	pub limit: Option<u32>,
}
