//! Configuration keys available to `.odoo_lsp`.

use serde::{Deserialize, Serialize};

/// Configuration is changed via [`on_change_config`][crate::backend::Backend::on_change_config].
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
	pub module: Option<ModuleConfig>,
	pub symbols: Option<SymbolsConfig>,
	pub references: Option<ReferencesConfig>,
	pub completions: Option<CompletionsConfig>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub xml: Option<XmlConfig>,
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

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct XmlConfig {
	/// Whether the built-in RELAX NG schema assistance (element/attribute
	/// completions and hovers) is enabled. Defaults to `true`. Disable this to
	/// rely solely on an external XML schema provider, e.g. the RedHat XML
	/// extension in VSCode.
	#[serde(rename = "schemaCompletions")]
	pub schema_completions: Option<bool>,
}
