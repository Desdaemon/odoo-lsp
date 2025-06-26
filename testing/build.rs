use std::fs;
use std::path::Path;

fn visit_folder(dir: &Path) {
	if let Ok(entries) = fs::read_dir(dir) {
		for entry in entries.flatten() {
			let path = entry.path();
			println!("cargo:rerun-if-changed={}", path.display());
			if path.is_dir() {
				visit_folder(&path);
			}
		}
	}
}

fn main() {
	let fixtures_dir = Path::new("fixtures");
	println!("cargo:rerun-if-changed={}", fixtures_dir.display());
	visit_folder(fixtures_dir);
}
