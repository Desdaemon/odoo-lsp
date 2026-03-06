//! Utilities for testing and benchmarking.

pub mod fs {
	#[cfg(not(test))]
	pub use std::fs::read_to_string;

	#[cfg(test)]
	pub static TEST_FS: std::sync::LazyLock<
		std::sync::RwLock<std::collections::HashMap<std::path::PathBuf, &'static [u8]>>,
	> = std::sync::LazyLock::new(Default::default);

	/// Mocked [`std::fs::read_to_string`] reading from [`TEST_FS`]
	#[cfg(test)]
	pub fn read_to_string<P>(path: P) -> std::io::Result<String>
	where
		P: AsRef<std::path::Path>,
	{
		fn inner(path: &std::path::Path) -> std::io::Result<String> {
			let fs = TEST_FS.read().unwrap();
			let bytes = fs.get(path).ok_or_else(|| {
				std::io::Error::new(
					std::io::ErrorKind::NotFound,
					format!("{} not found in test_utils::TEST_FS", path.display()),
				)
			})?;
			Ok(String::from_utf8(bytes.to_vec()).expect("non utf-8"))
		}
		inner(path.as_ref())
	}
}

pub mod index {
	#[cfg(test)]
	use crate::{index::Index, test_utils::fs::TEST_FS};

	/// Helper to index all models from Python, JS, and XML source and populate their properties using the virtual test FS.
	#[cfg(test)]
	pub fn index_models_with_properties(
		index: &mut Index,
		py: Option<&'static str>,
		js: Option<&'static str>,
		xml: Option<&'static str>,
	) {
		index_models_with_properties_and_id(index, py, js, xml);
	}

	/// Helper to index all models from Python, JS, and XML source with a specific test ID.
	#[cfg(test)]
	pub fn index_models_with_properties_and_id(
		index: &mut Index,
		py: Option<&'static str>,
		js: Option<&'static str>,
		xml: Option<&'static str>,
	) {
		// Generate a unique test ID to avoid conflicts between parallel tests
		use std::sync::atomic::{AtomicU64, Ordering};
		static TEST_ID: AtomicU64 = AtomicU64::new(0);
		let test_id = TEST_ID.fetch_add(1, Ordering::SeqCst);
		use crate::model::ModelEntry;
		use crate::prelude::*;
		use std::path::Path;

		let root = format!("/test_{test_id}");
		let py_path = format!("/test_{test_id}/dummy.py");
		let js_path = format!("/test_{test_id}/dummy.js");
		let xml_path = format!("/test_{test_id}/dummy.xml");
		if let Ok(mut fs) = TEST_FS.write() {
			if let Some(py) = py {
				fs.insert(Path::new(&py_path).to_path_buf(), py.as_bytes());
			}
			if let Some(js) = js {
				fs.insert(Path::new(&js_path).to_path_buf(), js.as_bytes());
			}
			if let Some(xml) = xml {
				fs.insert(Path::new(&xml_path).to_path_buf(), xml.as_bytes());
			}
		} else {
			panic!("can't modify test_utils::fs::TEST_FS");
		}
		// Index the model from the file
		let py_bytes = py.expect("Python source required").as_bytes();
		let models = crate::index::index_models(py_bytes).unwrap();
		for model in models {
			if let crate::model::ModelType::Base { name, .. } = &model.type_ {
				let entry = ModelEntry {
					base: Some(crate::model::ModelLocation(
						crate::utils::MinLoc {
							path: crate::index::PathSymbol::strip_root(_I(&root), Path::new(&py_path)),
							range: Range::default(),
						},
						ByteOffset(0)..ByteOffset(0),
					)),
					..Default::default()
				};
				index.models.insert(_I(name.as_str()).into(), entry);
				let _ = index.models.populate_properties(_I(name.as_str()).into(), &[]);
			}
		}
		// Optionally index JS and XML if needed by your backend (add logic here)
		// e.g. index_js, index_xml, or append to index.components, index.templates, etc.
	}
}

#[cfg(test)]
pub mod cases {
	use crate::index::{PathSymbol, index_models};
	use crate::model::ModelIndex;
	use crate::test_utils;
	use std::path::Path;

	pub mod foo {
		use crate::index::_I;

		use super::*;
		pub const FOO_PY: &[u8] = br#"
class Foo(Model):
	_name = 'foo'

	bar = Char()

class Bar(Model):
	_name = 'bar'
	_inherit = 'foo'

	baz = Char()

	def test(self):
		return self.env['foo']

class Quux(Model):
	_name = 'quux'
	_inherit = 'bar'

	def test(self):
		return super().test()
"#;
		pub fn prepare_foo_index() -> ModelIndex {
			const ROOT: &str = "/addons";
			const FOO_PY_PATH: &str = "/addons/foo.py";
			if let Ok(mut fs) = test_utils::fs::TEST_FS.write() {
				fs.insert(FOO_PY_PATH.into(), FOO_PY);
			} else {
				panic!("can't modify FS");
			}

			let index = ModelIndex::default();
			let foo_models = index_models(FOO_PY).unwrap();

			let root = _I(ROOT);
			let path = PathSymbol::strip_root(root, Path::new(FOO_PY_PATH));
			index.append(path, false, &foo_models[..]);
			index
		}
	}
}
