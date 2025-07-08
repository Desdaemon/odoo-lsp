//! Utilities for testing and benchmarking.

pub mod fs {
	#[cfg(not(test))]
	pub use std::fs::read;

	#[cfg(test)]
	pub static TEST_FS: std::sync::LazyLock<
		std::sync::RwLock<std::collections::HashMap<std::path::PathBuf, &'static [u8]>>,
	> = std::sync::LazyLock::new(Default::default);

	/// Mocked [`std::fs::read`] reading from [`TEST_FS`]
	#[cfg(test)]
	pub fn read<P>(path: P) -> std::io::Result<Vec<u8>>
	where
		P: AsRef<std::path::Path>,
	{
		let fs = TEST_FS.read().unwrap();
		let bytes = fs.get(path.as_ref()).ok_or_else(|| {
			std::io::Error::new(
				std::io::ErrorKind::NotFound,
				format!("{} not found in test_utils::TEST_FS", path.as_ref().to_string_lossy()),
			)
		})?;
		Ok(bytes.to_vec())
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
		use crate::model::ModelEntry;
		use crate::prelude::*;
		use std::path::Path;

		let root = "/test";
		let py_path = "/test/dummy.py";
		let js_path = "/test/dummy.js";
		let xml_path = "/test/dummy.xml";
		if let Ok(mut fs) = TEST_FS.write() {
			if let Some(py) = py {
				fs.insert(Path::new(py_path).to_path_buf(), py.as_bytes());
			}
			if let Some(js) = js {
				fs.insert(Path::new(js_path).to_path_buf(), js.as_bytes());
			}
			if let Some(xml) = xml {
				fs.insert(Path::new(xml_path).to_path_buf(), xml.as_bytes());
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
							path: crate::index::PathSymbol::strip_root(_I(root), Path::new(py_path)),
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
