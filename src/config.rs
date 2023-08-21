use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
pub struct Config {
	pub module: ModuleConfig,
	pub trace: TraceConfig,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ModuleConfig {
	pub roots: Vec<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct TraceConfig {
	pub server: TraceVerbosity,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub enum TraceVerbosity {
	Off,
	Messages,
	Verbose,
}
