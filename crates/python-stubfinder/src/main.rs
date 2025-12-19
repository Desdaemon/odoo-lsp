use std::env::args;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio, exit};

use serde::Deserialize;

fn main() {
	let Some(mut module) = args().nth(1) else {
		eprintln!("At least one Python module path is required!");
		exit(1);
	};

	let mut venv = std::env::var("VIRTUAL_ENV").ok().map(PathBuf::from);
	if venv.is_none() {
		let path = std::env::current_dir().unwrap().join(".venv");
		if path.exists() {
			venv = Some(path);
		}
	}

	let Some(venv) = venv else {
		eprintln!("No VIRTUAL_ENV defined and no .venv found at the working directory!");
		exit(1);
	};

	let python = venv.join("bin/python");
	let (stream, mut sink) = std::io::pipe().unwrap();
	let child = Command::new(python)
		.arg("-")
		.stdin(stream)
		.stdout(Stdio::piped())
		.spawn()
		.unwrap();
	_ = sink.write_all(
		br#"
import sysconfig, site, json
print(json.dumps({
'stdlib':sysconfig.get_path('stdlib'),
'site':site.getsitepackages(),
}))"#,
	);
	drop(sink);

	#[derive(Deserialize)]
	struct Output {
		stdlib: String,
		site: Vec<String>,
	}

	let result = child.wait_with_output().unwrap();
	let result: Output = serde_json::from_slice(&result.stdout).unwrap();

	let mut alternatives = vec![result.stdlib];
	alternatives.extend(result.site);
	module = module.split('.').collect::<Vec<_>>().join("/");

	for alternative in alternatives {
		let path = Path::new(&alternative).join(&module).with_added_extension("pyi");
		if path.exists() {
			print_and_quit(&module, &path);
		}

		let path = Path::new(&alternative).join(&module).join("__init__.pyi");
		if path.exists() {
			print_submodules(&path);
			print_and_quit(&module, &path);
		}

		let path = Path::new(&alternative).join(&module).with_added_extension("py");
		if path.exists() {
			print_and_quit(&module, &path);
		}

		let path = Path::new(&alternative).join(&module).join("__init__.py");
		if path.exists() {
			print_submodules(&path);
			print_and_quit(&module, &path);
		}
	}

	println!("Module {module} could not be resolved");
	exit(1);
}

fn print_submodules(path: &Path) {
	println!("# Visible child modules:");
	let root = path.parent().unwrap();
	for file in root.read_dir().unwrap() {
		let Ok(file) = file else { continue };
		if file.path() != path {
			println!("#  {}", file.path().display());
		}
	}
}

fn print_and_quit(module: &str, path: &Path) -> ! {
	println!("# {module} defined in {}:", path.display());
	std::io::copy(&mut File::open(path).unwrap(), &mut std::io::stdout()).unwrap();
	exit(0);
}
