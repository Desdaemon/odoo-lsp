use std::{env::current_dir, fs::canonicalize, io::stdout, path::Path, process::exit, sync::Arc};

use globwalk::FileType;
use tracing::{debug, warn};
use miette::{diagnostic, IntoDiagnostic};
use odoo_lsp::config::{CompletionsConfig, Config, ModuleConfig, ReferencesConfig, SymbolsConfig};
use odoo_lsp::index::{interner, Index};
use self_update::{backends::github, Status};
use serde_json::Value;

mod tsconfig;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
const GIT_VERSION: &str = git_version::git_version!(args = ["--tags", "--candidates=0"], fallback = "");

#[derive(Default)]
pub struct Args<'a> {
	pub addons_path: Vec<&'a str>,
	pub output: Option<&'a str>,
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
";

pub fn parse_args<'r>(mut args: &'r [&'r str]) -> Args<'r> {
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
			[] => break,
			_ => {
				eprintln!("{HELP}");
				exit(1);
			}
		}
	}

	out
}

pub async fn tsconfig(addons_path: &[&str], output: Option<&str>) -> miette::Result<()> {
	let addons_path = addons_path
		.iter()
		.map(|path| canonicalize(path).into_diagnostic())
		.collect::<miette::Result<Vec<_>>>()?;

	let index = Index::default();

	for addons in &addons_path {
		let path = addons.to_string_lossy();
		index.add_root(&path, None, true).await?;
	}

	let mut ts_paths = serde_json::Map::new();
	let pwd = current_dir().into_diagnostic()?;

	let mut includes = vec![];
	let mut type_roots = vec![];
	let interner = interner();

	let defines = Arc::new(tsconfig::DefineIndex::default());

	let mut outputs = tokio::task::JoinSet::new();

	for entry in &index.roots {
		let root = pathdiff::diff_paths(Path::new(entry.key().as_str()), &pwd)
			.ok_or_else(|| diagnostic!("Cannot diff {} to pwd", entry.key()))?;

		includes.push(Value::String(root.join("**/static/src").to_string_lossy().into_owned()));
		type_roots.push(Value::String(
			root.join("web/tooling/types").to_string_lossy().into_owned(),
		));
		for (module, path) in entry.value().iter() {
			let module = interner.resolve(&module);
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
		let scripts = globwalk::glob_builder(format!("{}/**/*.js", entry.key()))
			.file_type(FileType::FILE | FileType::SYMLINK)
			.follow_links(true)
			.build()
			.into_diagnostic()?;
		for js in scripts {
			let Ok(js) = js else { continue };
			let path = js.path().to_path_buf();
			let path = pathdiff::diff_paths(&path, &pwd)
				.ok_or_else(|| diagnostic!("Cannot diff {} to pwd", path.display()))?;
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
		let location = Value::String(interner.resolve(entry.key()).to_string());
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
		"exclude": [
			"/**/*.po",
			"/**/*.py",
			"/**/*.pyc",
			"/**/*.xml",
			"/**/*.png",
			"/**/*.md",
			"/**/*.dat",
			"/**/*.scss",
			"/**/*.jpg",
			"/**/*.svg",
			"/**/*.pot",
			"/**/*.csv",
			"/**/*.mo",
			"/**/*.txt",
			"/**/*.less",
			"/**/*.bcmap",
			"/**/*.properties",
			"/**/*.html",
			"/**/*.ttf",
			"/**/*.rst",
			"/**/*.css",
			"/**/*.pack",
			"/**/*.idx",
			"/**/*.h",
			"/**/*.map",
			"/**/*.gif",
			"/**/*.sample",
			"/**/*.doctree",
			"/**/*.so",
			"/**/*.pdf",
			"/**/*.xslt",
			"/**/*.conf",
			"/**/*.woff",
			"/**/*.xsd",
			"/**/*.eot",
			"/**/*.jst",
			"/**/*.flow",
			"/**/*.sh",
			"/**/*.yml",
			"/**/*.pfb",
			"/**/*.jpeg",
			"/**/*.crt",
			"/**/*.template",
			"/**/*.pxd",
			"/**/*.dylib",
			"/**/*.pem",
			"/**/*.rng",
			"/**/*.xsl",
			"/**/*.xls",
			"/**/*.cfg",
			"/**/*.pyi",
			"/**/*.pth",
			"/**/*.markdown",
			"/**/*.key",
			"/**/*.ico"
		]
	}};

	let output = output.unwrap_or("tsconfig.json");
	if output == "-" {
		serde_json::to_writer_pretty(stdout(), &tsconfig).into_diagnostic()
	} else {
		let file = std::fs::OpenOptions::new()
			.write(true)
			.truncate(true)
			.create(true)
			.open(output)
			.into_diagnostic()?;
		serde_json::to_writer_pretty(file, &tsconfig).into_diagnostic()?;
		eprintln!("TypeScript config file generated at {output}");
		Ok(())
	}
}

pub fn init(addons_path: &[&str], output: Option<&str>) -> miette::Result<()> {
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
		serde_json::to_writer_pretty(stdout(), &config).into_diagnostic()
	} else {
		let file = std::fs::OpenOptions::new()
			.write(true)
			.truncate(true)
			.create(true)
			.open(output)
			.into_diagnostic()?;
		serde_json::to_writer_pretty(file, &config).into_diagnostic()?;
		eprintln!("Config file generated at {output}");
		Ok(())
	}
}

pub fn self_update(nightly: bool) -> miette::Result<()> {
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
			.build()
			.into_diagnostic()?
			.fetch()
			.into_diagnostic()?;
		releases.sort_unstable_by(|a, z| z.date.cmp(&a.date));
		if GIT_VERSION.starts_with("nightly") {
			releases.retain(|rel| rel.name.as_str() > GIT_VERSION);
		}
		if let Some(latest_nightly) = releases.iter().find(|rel| rel.name.starts_with("nightly")) {
			eprintln!("Latest nightly is {}", latest_nightly.version);
			task = task.target_version_tag(&latest_nightly.version);
		} else {
			eprintln!("No nightly versions found; defaulting to stable releases.");
		}
	}

	let status = task.build().into_diagnostic()?.update().into_diagnostic()?;
	match status {
		Status::UpToDate(current) => eprintln!("odoo-lsp is already at the latest version: {current}"),
		Status::Updated(new) => eprintln!("Updated odoo-lsp to {new}"),
	}
	Ok(())
}
