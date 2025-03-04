
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
