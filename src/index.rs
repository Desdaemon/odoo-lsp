use std::path::Path;
use std::time::Duration;

use dashmap::{DashMap, DashSet};
use faststr::FastStr;
use globwalk::FileType;
use miette::{Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::notification::Progress;
use tower_lsp::lsp_types::{
	ProgressParams, ProgressParamsValue, ProgressToken, WorkDoneProgress, WorkDoneProgressReport,
};
use xmlparser::{Token, Tokenizer};

use crate::{record::Record, utils::CharOffset};

#[derive(Debug, Default)]
pub struct ModuleIndex {
	pub roots: DashSet<String>,
	pub modules: DashSet<String>,
	pub records: DashMap<String, Record>,
}

impl ModuleIndex {
	pub fn mark_n_sweep(&self) {
		self.records.retain(|_, record| !record.deleted)
	}
	pub async fn add_root(
		&self,
		root: &str,
		progress: Option<(&tower_lsp::Client, ProgressToken)>,
	) -> miette::Result<Option<(usize, usize, Duration)>> {
		if !self.roots.insert(dbg!(root).to_string()) {
			return Ok(None);
		}

		let t0 = tokio::time::Instant::now();
		let modules = globwalk::glob_builder(format!("{root}/**/__manifest__.py"))
			.file_type(FileType::FILE | FileType::SYMLINK)
			.build()
			.into_diagnostic()
			.with_context(|| format!("Could not glob into {root}"))?;

		let mut handles: Vec<tokio::task::JoinHandle<miette::Result<_>>> = vec![];
		let mut module_count = 0;
		for module in modules {
			module_count += 1;
			let module = module.into_diagnostic()?;
			let module_dir = &module
				.path()
				.parent()
				.map(Path::to_path_buf)
				.ok_or_else(|| miette::diagnostic!("Unexpected empty path"))?;
			let module_name = module_dir.file_name().unwrap().to_string_lossy().to_string();
			if let Some((client, token)) = &progress {
				_ = client
					.send_notification::<Progress>(ProgressParams {
						token: token.clone(),
						value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(WorkDoneProgressReport {
							message: Some(format!("Indexing {module_name}")),
							..Default::default()
						})),
					})
					.await;
			}
			let module_name = FastStr::from(module_name);
			if !self.modules.insert(module_name.to_string()) {
				eprintln!("Duplicate module {}", module_name);
				continue;
			}
			let xmls = globwalk::glob_builder(format!("{}/**/*.xml", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format!("Could not glob into {:?}", module.path()))?;
			for xml in xmls {
				let xml = xml.into_diagnostic()?;
				let path = xml.path().to_path_buf();
				let path_uri = path.to_string_lossy().to_string();
				let module_name = module_name.clone();
				handles.push(tokio::spawn(async move {
					let file = tokio::fs::read(&path)
						.await
						.into_diagnostic()
						.with_context(|| format!("Could not read {path_uri}"))?;
					let file = std::str::from_utf8(&file)
						.into_diagnostic()
						.with_context(|| format!("non-utf8 file: {path_uri}"))?;
					let mut reader = Tokenizer::from(file);
					let mut records = vec![];
					let rope = Rope::from_str(file);
					loop {
						match reader.next() {
							Some(Ok(Token::ElementStart { local, span, .. })) => {
								if local.as_str() == "record" {
									let record = Record::from_reader(
										CharOffset(span.start()),
										module_name.clone(),
										&path_uri,
										&mut reader,
										rope.clone(),
									)?;
									records.extend(record);
								} else if local.as_str() == "template" {
									let template = Record::template(
										CharOffset(span.start()),
										module_name.clone(),
										&path_uri,
										&mut reader,
										rope.clone(),
									)?;
									records.extend(template);
								}
							}
							None => break,
							Some(Err(err)) => {
								eprintln!("error parsing {}:\n{err}", path.display());
								break;
							}
							_ => {}
						}
					}

					miette::Result::Ok(records)
				}));
			}
		}
		let mut record_count = 0;
		// let mut template_count = 0;
		for result in futures::future::join_all(handles).await {
			let records = result.into_diagnostic()?;
			let records = match records {
				Ok(records) => records,
				Err(err) => {
					eprintln!("{err}");
					continue;
				}
			};
			record_count += records.len();
			for record in records {
				let key = record.qualified_id();
				if let Some((_, discarded)) = self.records.remove(&key) {
					eprintln!(
						"{key}:\n{} -> {}",
						discarded.location.uri.path(),
						record.location.uri.path()
					)
				}
				self.records.insert(key, record);
			}
			// template_count += templates.len();
			// for template in templates {}
		}

		Ok(Some((module_count, record_count, t0.elapsed())))
	}
	pub fn remove_root(&self, root: &str) {
		self.roots.remove(root);
		for mut entry in self.records.iter_mut() {
			if root.contains(entry.module.as_str()) {
				entry.deleted = true;
			}
		}
	}
	pub fn module_of_path(&self, path: &Path) -> Option<dashmap::setref::one::Ref<String>> {
		let mut path = Some(path);
		while let Some(path_) = &path {
			if let Some(module) = self.modules.get(path_.file_name()?.to_string_lossy().as_ref()) {
				return Some(module);
			}
			path = path_.parent();
		}
		None
	}
}
