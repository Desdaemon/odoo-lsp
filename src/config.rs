use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct Config {
	pub module: Option<ModuleConfig>,
	pub symbols: Option<SymbolsConfig>,
	pub references: Option<ReferencesConfig>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ModuleConfig {
	pub roots: Option<Vec<String>>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct SymbolsConfig {
	pub limit: Option<u32>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ReferencesConfig {
	pub limit: Option<u32>,
}
