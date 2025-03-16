use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
	pub module: Option<ModuleConfig>,
	pub symbols: Option<SymbolsConfig>,
	pub references: Option<ReferencesConfig>,
	pub completions: Option<CompletionsConfig>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct ModuleConfig {
	pub roots: Option<Vec<String>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct SymbolsConfig {
	pub limit: Option<usize>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct ReferencesConfig {
	pub limit: Option<usize>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct CompletionsConfig {
	pub limit: Option<usize>,
}
