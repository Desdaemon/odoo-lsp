use std::{env::current_dir, fs::canonicalize, io::stdout, path::Path, process::exit};

use miette::{diagnostic, IntoDiagnostic};
use odoo_lsp::index::{interner, Index};
use serde_json::Value;

pub enum Command<'a> {
	Run,
	TsConfig {
		addons_path: Vec<&'a str>,
		output: Option<&'a Path>,
	},
}

const HELP: &str = "Language server for Odoo Python/JS/XML

USAGE:
	odoo-lsp
	odoo-lsp tsconfig --addons-path ..

OPTIONS:
	-h, --help
		Display this help message
	-o, --out
		Specify the path to store the output.
	--addons-path PATH
		[tsconfig only] Specifies the roots of the addons.
		Can be specified multiple times, or as a comma-separated list of paths.
";

pub fn parse_args<'r>(mut args: &'r [&'r str]) -> Command<'r> {
	let mut out = Command::Run;
	loop {
		match args {
			["tsconfig", rest @ ..] => {
				args = rest;
				out = Command::TsConfig {
					addons_path: Vec::new(),
					output: None,
				};
			}
			["-h" | "--help", ..] => {
				eprintln!("{HELP}");
				exit(0);
			}
			["--addons-path", path, rest @ ..] => {
				args = rest;
				if let Command::TsConfig { addons_path, .. } = &mut out {
					addons_path.extend(path.split(","));
				} else {
					eprintln!("--addons-path is only supported by tsconfig.");
					exit(1);
				}
			}
			["-o" | "--out", path, rest @ ..] => {
				args = rest;
				if let Command::TsConfig { output, .. } = &mut out {
					*output = Some(Path::new(path));
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

pub async fn tsconfig(addons_path: &[&str], output: Option<&Path>) -> miette::Result<()> {
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
	for entry in &index.roots {
		let root = pathdiff::diff_paths(Path::new(entry.key().as_str()), &pwd)
			.ok_or_else(|| diagnostic!("Cannot diff {} to pwd", entry.key()))?;

		includes.push(Value::String(root.join("**/static/src").to_string_lossy().into_owned()));
		type_roots.push(Value::String(
			root.join("web/tooling/types").to_string_lossy().into_owned(),
		));
		for module in entry.value().keys() {
			let module = interner.resolve(&module);
			ts_paths
				.entry(format!("@{module}/*"))
				.or_insert_with(|| Value::Array(vec![]))
				.as_array_mut()
				.unwrap()
				.push(
					root.join(format!("{module}/static/src/*"))
						.to_string_lossy()
						.into_owned()
						.into(),
				)
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

	if let Some(output) = output {
		let file = std::fs::OpenOptions::new()
			.write(true)
			.truncate(true)
			.open(output)
			.into_diagnostic()?;
		serde_json::to_writer_pretty(file, &tsconfig).into_diagnostic()
	} else {
		serde_json::to_writer_pretty(stdout(), &tsconfig).into_diagnostic()
	}
}
