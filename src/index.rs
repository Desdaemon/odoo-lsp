use std::path::Path;
use std::sync::atomic::AtomicBool;

use dashmap::{DashMap, DashSet};
use faststr::FastStr;
use globwalk::FileType;
use miette::{Context, IntoDiagnostic};
use ropey::Rope;
use xmlparser::{Token, Tokenizer};

use crate::{record::Record, utils::CharOffset};

#[derive(Debug, Default)]
pub struct ModuleIndex {
	pub in_progress: AtomicBool,
	pub roots: DashSet<String>,
	pub modules: DashSet<String>,
	pub records: DashMap<String, Record>,
}

impl ModuleIndex {
	pub fn mark_n_sweep(&self) {
		self.records.retain(|_, record| !record.deleted)
	}
	pub async fn add_root(&self, root: &str) -> miette::Result<()> {
		if !self.roots.insert(dbg!(root).to_string()) {
			return Ok(());
		}
		let modules = globwalk::glob_builder(format!("{root}/**/__manifest__.py"))
			.file_type(FileType::FILE | FileType::SYMLINK)
			.build()
			.into_diagnostic()
			.with_context(|| format!("Could not glob into {root}"))?;

		let mut handles: Vec<tokio::task::JoinHandle<miette::Result<_>>> = vec![];
		for module in modules {
			let module = module.into_diagnostic()?;
			let module_dir = dbg!(&module)
				.path()
				.parent()
				.map(Path::to_path_buf)
				.ok_or_else(|| miette::diagnostic!("Unexpected empty path"))?;
			let module_name = module_dir.file_name().unwrap().to_string_lossy().to_string();
			let module_name = FastStr::from(module_name);
			if !self.modules.insert(module_name.to_string()) {
				eprintln!("Duplicate module {}", module_name);
				continue;
			}
			let xmls = globwalk::glob_builder(dbg!(format!("{}/**/*.xml", module_dir.display())))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format!("Could not glob into {:?}", module.path()))?;
			for xml in xmls {
				let xml = xml.into_diagnostic()?;
				let path = dbg!(&xml).path().to_path_buf();
				let path_uri = path.to_string_lossy().to_string();
				let module_name = module_name.clone();
				handles.push(tokio::spawn(async move {
					let file = tokio::fs::read(&path)
						.await
						.into_diagnostic()
						.with_context(|| format!("Could not read {}", path.display()))?;
					let file = std::str::from_utf8(&file)
						.into_diagnostic()
						.with_context(|| "non-utf8 file")?;
					let mut reader = Tokenizer::from(file);
					let mut out = vec![];
					loop {
						match reader.next() {
							Some(Ok(Token::ElementStart { local, span, .. })) if local.as_str() == "record" => {
								let record = Record::from_reader(
									CharOffset(span.start()),
									module_name.clone(),
									&path_uri,
									&mut reader,
									&Rope::from_str(file),
								)?;
								out.push(record);
							}
							None | Some(Err(_)) => break,
							_ => {}
						}
					}

					miette::Result::Ok(out)
				}));
			}
		}
		for result in futures::future::join_all(handles).await {
			let records = result.into_diagnostic()??;
			for record in records {
				self.records.insert(record.id.to_string(), record);
			}
		}

		Ok(())
	}
	pub fn remove_root(&self, root: &str) {
		self.roots.remove(root);
		for mut entry in self.records.iter_mut() {
			if root.contains(entry.module.as_str()) {
				entry.deleted = true;
			}
		}
	}
}
