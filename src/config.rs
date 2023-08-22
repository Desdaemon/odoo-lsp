use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct Config {
	pub module: Option<ModuleConfig>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ModuleConfig {
	pub roots: Option<Vec<String>>,
}
