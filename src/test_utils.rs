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

#[cfg(test)]
pub mod cases {
	use crate::index::{index_models, interner, PathSymbol};
	use crate::model::ModelIndex;
	use crate::test_utils;
	use std::path::Path;

	pub mod foo {
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
"#;
		pub fn prepare_foo_index() -> ModelIndex {
			let rt = tokio::runtime::Builder::new_current_thread()
				.enable_all()
				.build()
				.unwrap();

			const ROOT: &str = "/addons";
			const FOO_PY_PATH: &str = "/addons/foo.py";
			if let Ok(mut fs) = test_utils::fs::TEST_FS.write() {
				fs.insert(FOO_PY_PATH.into(), FOO_PY);
			} else {
				panic!("can't modify FS");
			}

			let index = ModelIndex::default();
			let foo_models = index_models(FOO_PY).unwrap();

			let root = interner().get_or_intern_static(ROOT);
			let path = PathSymbol::strip_root(root, Path::new(FOO_PY_PATH));
			rt.block_on(index.append(path, interner(), false, &foo_models[..]));
			index
		}
	}
}
