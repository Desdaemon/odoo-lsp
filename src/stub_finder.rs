//! Stub file finder - locates Python type stub files (.pyi) and source files (.py)
//! Similar to python-stubfinder, finds modules in stdlib, site-packages, and user code

use serde::Deserialize;
use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;
use tracing::debug;

/// Python search paths: stdlib and site-packages
#[derive(Debug, Clone)]
pub struct PythonSearchPaths {
	pub stdlib: PathBuf,
	pub site_packages: Vec<PathBuf>,
}

/// Cache for Python search paths to avoid repeated subprocess calls
static PYTHON_SEARCH_PATHS_CACHE: OnceLock<Option<PythonSearchPaths>> = OnceLock::new();

/// Finds Python module paths by querying the Python environment (cached)
pub fn get_python_search_paths() -> Option<PythonSearchPaths> {
	PYTHON_SEARCH_PATHS_CACHE
		.get_or_init(compute_python_search_paths)
		.clone()
}

/// Internal function that actually computes Python search paths
fn compute_python_search_paths() -> Option<PythonSearchPaths> {
	// Try to find Python executable
	let python = find_python_executable()?;

	debug!("Using Python: {}", python.display());

	// Query Python for stdlib and site-packages paths
	let output = Command::new(&python)
		.arg("-c")
		.arg(
			r#"
import sysconfig, site, json
print(json.dumps({
	'stdlib': sysconfig.get_path('stdlib'),
	'site': site.getsitepackages(),
}))
"#,
		)
		.output()
		.ok()?;

	if !output.status.success() {
		debug!("Python query failed: {}", String::from_utf8_lossy(&output.stderr));
		return None;
	}

	#[derive(Deserialize)]
	struct Output {
		stdlib: String,
		site: Vec<String>,
	}

	let result: Output = serde_json::from_slice(&output.stdout).ok()?;

	Some(PythonSearchPaths {
		stdlib: PathBuf::from(result.stdlib),
		site_packages: result.site.into_iter().map(PathBuf::from).collect(),
	})
}

/// Finds the Python executable from VIRTUAL_ENV, .venv, or system PATH
fn find_python_executable() -> Option<PathBuf> {
	// Try VIRTUAL_ENV
	if let Ok(venv) = std::env::var("VIRTUAL_ENV") {
		let python = PathBuf::from(&venv).join("bin/python");
		if python.exists() {
			return Some(python);
		}
	}

	// Try .venv in current directory
	let venv_path = std::env::current_dir().ok()?.join(".venv");
	if venv_path.exists() {
		let python = venv_path.join("bin/python");
		if python.exists() {
			return Some(python);
		}
	}

	// Try system python
	if which::which("python3").is_ok() {
		return which::which("python3").ok();
	}

	if which::which("python").is_ok() {
		return which::which("python").ok();
	}

	None
}

/// Finds a Python module file, checking stubs first
///
/// Search order (stub-first):
/// 1. `module.pyi` (stub file)
/// 2. `module/__init__.pyi` (package stub)
/// 3. `module.py` (source file)
/// 4. `module/__init__.py` (package source)
pub fn find_module_file(module_path: &str, search_paths: &PythonSearchPaths) -> Option<PathBuf> {
	// Convert module path to file path (e.g., "os.path" -> "os/path")
	let file_path = module_path.replace('.', "/");

	// Search in all paths (stdlib first, then site-packages)
	let mut all_paths = vec![search_paths.stdlib.clone()];
	all_paths.extend(search_paths.site_packages.clone());

	for search_dir in all_paths {
		// Try .pyi (stub file)
		let stub_file = search_dir.join(&file_path).with_extension("pyi");
		if stub_file.exists() {
			debug!("Found stub: {}", stub_file.display());
			return Some(stub_file);
		}

		// Try __init__.pyi (package stub)
		let init_stub = search_dir.join(&file_path).join("__init__.pyi");
		if init_stub.exists() {
			debug!("Found package stub: {}", init_stub.display());
			return Some(init_stub);
		}

		// Try .py (source file)
		let source_file = search_dir.join(&file_path).with_extension("py");
		if source_file.exists() {
			debug!("Found source: {}", source_file.display());
			return Some(source_file);
		}

		// Try __init__.py (package source)
		let init_py = search_dir.join(&file_path).join("__init__.py");
		if init_py.exists() {
			debug!("Found package source: {}", init_py.display());
			return Some(init_py);
		}
	}

	debug!("Module not found: {}", module_path);
	None
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_find_stdlib_modules() {
		// This test requires a Python environment
		if let Some(paths) = get_python_search_paths() {
			// typing is always available in stdlib
			assert!(find_module_file("typing", &paths).is_some());

			// json is always available in stdlib
			assert!(find_module_file("json", &paths).is_some());

			// collections is always available
			assert!(find_module_file("collections", &paths).is_some());
		}
	}
}
