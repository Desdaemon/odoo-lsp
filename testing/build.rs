use std::fs;
use std::path::Path;

fn main() {
    let fixtures_dir = Path::new("fixtures");

    if let Ok(entries) = fs::read_dir(fixtures_dir) {
        for entry in entries.flatten() {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }
}
