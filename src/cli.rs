use std::{env::current_dir, io::stdout, process::exit, sync::Arc};

use anyhow::Context;
use globwalk::FileType;
use odoo_lsp::config::{CompletionsConfig, Config, ModuleConfig, ReferencesConfig, SymbolsConfig};
use odoo_lsp::index::{Index, _R};
use odoo_lsp::utils::strict_canonicalize;
use odoo_lsp::{errloc, format_loc, loc};
use self_update::{backends::github, Status};
use serde_json::Value;
use tracing::{debug, warn};

mod tsconfig;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
const GIT_VERSION: &str = git_version::git_version!(args = ["--tags", "--candidates=0"], fallback = "");

#[derive(Default)]
pub struct Args<'a> {
	pub addons_path: Vec<&'a str>,
	pub output: Option<&'a str>,
	pub threads: Option<usize>,
	pub log_format: LogFormat,
	pub command: Command,
}

#[derive(Default)]
pub enum Command {
	#[default]
	Run,
	Init {
		tsconfig: bool,
	},
	TsConfig,
	SelfUpdate {
		nightly: bool,
	},
}

#[derive(Default, Clone, Copy)]
pub enum LogFormat {
	#[default]
	Compact,
	Json,
}

const HELP: &str = "Language server for Odoo Python/JS/XML

USAGE:
	odoo-lsp
		Runs the language server
	odoo-lsp init --addons-path ..
		Generate a config file for the server
		Files named `.odoo_lsp` and `.odoo_lsp.json` are recognized
	odoo-lsp tsconfig --addons-path ..
		Generate a tsconfig.json file for TypeScript support
	odoo-lsp self-update [--nightly]
		Update to the latest version available.

OPTIONS:
	-h, --help
		Display this help message
	-o, --out
		Specify the path to store the output.
	--addons-path PATH
		Specifies the roots of the addons.
		Can be specified multiple times, or as a comma-separated list of paths.
	--tsconfig
		[init] Also generates a tsconfig.json file
	-v, --version
		Prints the current version and exit.
	--nightly
		[self-update] Selects the latest nightly available.
	-j, --threads NUMBER
		Specifies the number of threads to use. Defaults to four threads, or half
		of the core count if it is less than four, minimum of one.
	--log-format json
		Emits machine-readable JSON logs, useful for LSP clients and middleware
";

pub fn parse_args<'r>(mut args: &[&'r str]) -> Args<'r> {
	let mut out = Args::default();
	loop {
		match args {
			["tsconfig", rest @ ..] => {
				args = rest;
				out.command = Command::TsConfig;
			}
			["init", rest @ ..] => {
				args = rest;
				out.command = Command::Init { tsconfig: false };
			}
			["self-update", rest @ ..] => {
				args = rest;
				out.command = Command::SelfUpdate { nightly: false };
			}
			["-h" | "--help", ..] => {
				eprintln!("{HELP}");
				exit(0);
			}
			["-v" | "--version", ..] => {
				eprintln!("odoo-lsp v{VERSION} git:{GIT_VERSION}");
				exit(0);
			}
			["--addons-path", path, rest @ ..] => {
				args = rest;
				out.addons_path.extend(path.split(','));
			}
			["-o" | "--out", path, rest @ ..] => {
				args = rest;
				out.output = Some(path);
			}
			["--tsconfig", rest @ ..] => {
				args = rest;
				if let Command::Init { tsconfig } = &mut out.command {
					*tsconfig = true;
				}
			}
			["--nightly", rest @ ..] => {
				args = rest;
				if let Command::SelfUpdate { nightly } = &mut out.command {
					*nightly = true;
				}
			}
			["-j" | "--threads", threads, rest @ ..] => {
				args = rest;
				match threads.parse::<usize>() {
					Ok(0) => {
						eprintln!("Invalid thread count specified, ignoring.");
					}
					Ok(threads) => out.threads = Some(threads),
					Err(err) => {
						eprintln!("Cannot parse threads: {err}");
						exit(1);
					}
				}
			}
			["--log-format", "json", rest @ ..] => {
				args = rest;
				out.log_format = LogFormat::Json;
			}
			[] => break,
			_ => {
				eprintln!("{HELP}");
				exit(1);
			}
		}
	}

	out
}

/// Returns true if a CLI handler has been invoked.
pub async fn run(args: Args<'_>) -> bool {
	match args.command {
		Command::Run => return false,
		Command::TsConfig => {
			_ = tsconfig(&args.addons_path, args.output)
				.await
				.inspect_err(|err| eprintln!("{} tsconfig failed: {err}", loc!()));
		}
		Command::Init { tsconfig } => {
			_ = init(&args.addons_path, args.output).inspect_err(|err| eprintln!("{} init failed: {err}", loc!()));
			if tsconfig {
				_ = self::tsconfig(&args.addons_path, None)
					.await
					.inspect_err(|err| eprintln!("{} tsconfig failed: {err}", loc!()));
			}
		}
		Command::SelfUpdate { nightly } => {
			_ = tokio::task::spawn_blocking(move || self_update(nightly))
				.await
				.unwrap()
				.inspect_err(|err| eprintln!("{} self-update failed: {err}", loc!()));
		}
	}
	true
}

async fn tsconfig(addons_path: &[&str], output: Option<&str>) -> anyhow::Result<()> {
	let addons_path = addons_path
		.iter()
		.map(|path| strict_canonicalize(path).with_context(|| format_loc!("{} could not be canonicalized", path)))
		.collect::<anyhow::Result<Vec<_>>>()?;

	let index = Index::default();

	for addons in &addons_path {
		// let path = addons.to_string_lossy();
		index.add_root(addons, None, true).await?;
	}

	let mut ts_paths = serde_json::Map::new();
	let pwd = current_dir()?;

	let mut includes = vec![];
	let mut type_roots = vec![];

	let defines = Arc::new(tsconfig::DefineIndex::default());

	let mut outputs = tokio::task::JoinSet::new();

	for entry in &index.roots {
		let root =
			pathdiff::diff_paths(entry.key(), &pwd).ok_or_else(|| errloc!("Cannot diff {:?} to pwd", entry.key()))?;

		includes.push(Value::String(root.join("**/static/src").to_string_lossy().into_owned()));
		type_roots.push(Value::String(
			root.join("web/tooling/types").to_string_lossy().into_owned(),
		));
		for (module, path) in entry.value().iter() {
			let module = _R(*module);
			ts_paths
				.entry(format!("@{module}/*"))
				.or_insert_with(|| Value::Array(vec![]))
				.as_array_mut()
				.unwrap()
				.push(
					root.join(format!("{path}/static/src/*"))
						.to_string_lossy()
						.into_owned()
						.into(),
				);
		}
		let scripts = globwalk::glob_builder(entry.key().join("**/*.js").to_string_lossy())
			.file_type(FileType::FILE | FileType::SYMLINK)
			.follow_links(true)
			.build()?;
		for js in scripts {
			let Ok(js) = js else { continue };
			let path = js.path().to_path_buf();
			let path =
				pathdiff::diff_paths(&path, &pwd).ok_or_else(|| errloc!("Cannot diff {} to pwd", path.display()))?;
			let defines = Arc::clone(&defines);
			outputs.spawn(tsconfig::gather_defines(path, defines));
		}
	}

	while let Some(res) = outputs.join_next().await {
		match res {
			Ok(Ok(())) => {}
			Ok(Err(err)) => {
				warn!("{err}");
			}
			Err(err) => {
				debug!("join error: {err}")
			}
		}
	}

	for entry in defines.iter() {
		let location = Value::String(_R(*entry.key()).to_string());
		for define in entry.value() {
			ts_paths
				.entry(define.to_string())
				.or_insert_with(|| Value::Array(vec![]))
				.as_array_mut()
				.unwrap()
				.push(location.clone())
		}
	}

	let tsconfig = serde_json::json! {{
		"include": includes,
		"compilerOptions": {
			"baseUrl": ".",
			"target": "es2019",
			"checkJs": true,
			"allowJs": true,
			"noEmit": true,
			"typeRoots": type_roots,
			"paths": ts_paths,
		},
	}};

	let output = output.unwrap_or("tsconfig.json");
	if output == "-" {
		serde_json::to_writer_pretty(stdout(), &tsconfig)?;
	} else {
		let file = std::fs::OpenOptions::new()
			.write(true)
			.truncate(true)
			.create(true)
			.open(output)?;
		serde_json::to_writer_pretty(file, &tsconfig)?;
		eprintln!("TypeScript config file generated at {output}");
	}
	Ok(())
}

fn init(addons_path: &[&str], output: Option<&str>) -> anyhow::Result<()> {
	let config = Config {
		module: Some(ModuleConfig {
			roots: Some(addons_path.iter().map(ToString::to_string).collect()),
		}),
		symbols: Some(SymbolsConfig { limit: Some(80) }),
		references: Some(ReferencesConfig { limit: Some(80) }),
		completions: Some(CompletionsConfig { limit: Some(200) }),
	};
	let output = output.unwrap_or(".odoo_lsp");
	if output == "-" {
		serde_json::to_writer_pretty(stdout(), &config)?;
	} else {
		let file = std::fs::OpenOptions::new()
			.write(true)
			.truncate(true)
			.create(true)
			.open(output)?;
		serde_json::to_writer_pretty(file, &config)?;
		eprintln!("Config file generated at {output}");
	}
	Ok(())
}

fn self_update(nightly: bool) -> anyhow::Result<()> {
	const _: () = {
		if option_env!("CI").is_some() {
			assert!(!GIT_VERSION.is_empty(), "Git tag must be present when running in CI");
		}
	};

	let mut task = github::Update::configure();
	let mut task = task
		.repo_owner("Desdaemon")
		.repo_name("odoo-lsp")
		.bin_name("odoo-lsp")
		.show_download_progress(true)
		.current_version(VERSION);

	if nightly {
		let mut releases = github::ReleaseList::configure()
			.repo_owner("Desdaemon")
			.repo_name("odoo-lsp")
			.build()?
			.fetch()?;
		releases.sort_unstable_by(|a, z| z.date.cmp(&a.date));
		if GIT_VERSION.starts_with("nightly") {
			releases.retain(|rel| rel.version.as_str() > GIT_VERSION);
		}
		if let Some(latest_nightly) = releases.iter().find(|rel| rel.name.starts_with("nightly")) {
			eprintln!("Latest nightly is {}", latest_nightly.version);
			task = task.target_version_tag(&latest_nightly.version);
		} else {
			eprintln!("No nightly versions found; defaulting to stable releases.");
		}
	}

	let status = task.build()?.update()?;
	match status {
		Status::UpToDate(current) => eprintln!("odoo-lsp is already at the latest version: {current}"),
		Status::Updated(new) => eprintln!("Updated odoo-lsp to {new}"),
	}
	Ok(())
}
