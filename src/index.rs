use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::time::Duration;

use dashmap::DashSet;
use globwalk::FileType;
use log::{debug, warn};
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::notification::Progress;
use tower_lsp::lsp_types::*;
use tree_sitter::{Query, QueryCursor};
use xmlparser::{Token, Tokenizer};

use crate::format_loc;
use crate::model::{Model, ModelIndex};
use crate::record::Record;
use crate::utils::{offset_range_to_lsp_range, ByteOffset, CharOffset, ImStr, RangeExt};

mod record;

#[derive(Default)]
pub struct ModuleIndex {
	pub roots: DashSet<ImStr>,
	pub modules: DashSet<ImStr>,
	pub records: record::RecordIndex,
	pub models: ModelIndex,
}

enum Output {
	Records(Vec<Record>),
	Models { path: ImStr, models: Vec<Model> },
}

pub struct AddRootResults {
	pub module_count: usize,
	pub record_count: usize,
	pub model_count: usize,
	pub elapsed: Duration,
}

impl ModuleIndex {
	pub fn mark_n_sweep(&self) {
		self.records.retain(|_, record| !record.deleted)
	}
	pub async fn add_root(
		&self,
		root: &str,
		progress: Option<(&tower_lsp::Client, ProgressToken)>,
	) -> miette::Result<Option<AddRootResults>> {
		if !self.roots.insert(root.into()) {
			return Ok(None);
		}

		let t0 = tokio::time::Instant::now();
		let modules = globwalk::glob_builder(format!("{root}/**/__manifest__.py"))
			.file_type(FileType::FILE | FileType::SYMLINK)
			.build()
			.into_diagnostic()
			.with_context(|| format!("Could not glob into {root}"))?;

		let mut module_count = 0;
		let mut outputs = tokio::task::JoinSet::new();
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
				// let module_name = module_name.clone();
				_ = client
					.send_notification::<Progress>(ProgressParams {
						token: token.clone(),
						value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(WorkDoneProgressReport {
							message: Some(module_name.clone()),
							..Default::default()
						})),
					})
					.await;
			}
			let module_name = ImStr::from(module_name);
			if !self.modules.insert(module_name.clone()) {
				debug!("duplicate module {module_name}");
				continue;
			}
			let xmls = globwalk::glob_builder(format!("{}/**/*.xml", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format_loc!("Could not glob into {:?}", module.path()))?;
			for xml in xmls {
				let xml = match xml {
					Ok(entry) => entry,
					Err(err) => {
						debug!("{err}");
						continue;
					}
				};
				let module_name = module_name.clone();
				outputs.spawn(add_root_xml(xml.path().to_path_buf(), module_name));
			}
			let pys = globwalk::glob_builder(format!("{}/**/*.py", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format_loc!("Could not glob into {:?}", module.path()))?;
			for py in pys {
				let py = match py {
					Ok(entry) => entry,
					Err(err) => {
						debug!("{err}");
						continue;
					}
				};
				let module_name = module_name.clone();
				let path = py.path().to_path_buf();
				outputs.spawn(add_root_py(path, module_name));
			}
		}

		let mut record_count = 0;
		let mut model_count = 0;
		while let Some(outputs) = outputs.join_next().await {
			let Ok(outputs) = outputs else {
				debug!("join error");
				continue;
			};
			let outputs = match outputs {
				Ok(records) => records,
				Err(err) => {
					warn!("{err}");
					continue;
				}
			};
			match outputs {
				Output::Records(records) => {
					record_count += records.len();
					let mut prefix = self.records.by_prefix.write().await;
					self.records.extend_records(Some(&mut prefix), records).await;
				}
				Output::Models { path, models } => {
					model_count += models.len();
					let uri = match format!("file://{path}").parse() {
						Ok(uri) => uri,
						Err(err) => {
							debug!("{err}");
							continue;
						}
					};
					self.models.extend_models(&uri, models).await;
				}
			}
		}
		Ok(Some(AddRootResults {
			module_count,
			record_count,
			model_count,
			elapsed: t0.elapsed(),
		}))
	}
	pub fn remove_root(&self, root: &str) {
		self.roots.remove(root);
		for mut entry in self.records.iter_mut() {
			if root.contains(entry.module.as_str()) {
				entry.deleted = true;
			}
		}
	}
	pub fn module_of_path(&self, path: &Path) -> Option<dashmap::setref::one::Ref<ImStr>> {
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

async fn add_root_xml(path: PathBuf, module_name: ImStr) -> miette::Result<Output> {
	let path_uri = path.to_string_lossy();
	let file = tokio::fs::read(&path)
		.await
		.into_diagnostic()
		.with_context(|| format_loc!("Could not read {path_uri}"))?;
	let file = String::from_utf8_lossy(&file);
	let mut reader = Tokenizer::from(file.as_ref());
	let mut records = vec![];
	let rope = Rope::from_str(&file);
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
				debug!("error parsing {}:\n{err}", path.display());
				break;
			}
			_ => {}
		}
	}

	Ok(Output::Records(records))
}

fn model_query() -> &'static Query {
	static MODEL: OnceLock<Query> = OnceLock::new();
	MODEL.get_or_init(|| {
		tree_sitter::Query::new(tree_sitter_python::language(), include_str!("queries/model.scm"))
			.expect("failed to parse query")
	})
}

async fn add_root_py(path: PathBuf, _: ImStr) -> miette::Result<Output> {
	let file = tokio::fs::read(&path)
		.await
		.into_diagnostic()
		.with_context(|| format_loc!("Could not read {}", path.display()))?;

	let mut parser = tree_sitter::Parser::new();
	parser.set_language(tree_sitter_python::language()).into_diagnostic()?;

	let ast = parser.parse(&file, None).ok_or_else(|| diagnostic!("AST not parsed"))?;
	let query = model_query();
	let mut cursor = QueryCursor::new();

	let mut out = vec![];
	let rope = Rope::from_str(&String::from_utf8_lossy(&file));
	let eq_slice = |range: std::ops::Range<usize>, slice: &[u8]| &file[range] == slice;
	for match_ in cursor.matches(query, ast.root_node(), file.as_slice()) {
		let mut captures = match_.captures;
		let tail = match captures {
			[models, _, tail @ ..] if eq_slice(models.node.byte_range(), b"models") => tail,
			[_model, tail @ ..] => {
				debug_assert!(matches!(&file[_model.node.byte_range()], b"Model" | b"TransientModel"));
				tail
			}
			unk => Err(diagnostic!(
				"Bug: Unknown pattern {:?}",
				unk.iter()
					.map(|capture| String::from_utf8_lossy(&file[capture.node.byte_range()]))
					.collect::<Vec<_>>()
			))?,
		};
		captures = tail;
		let mut model = None;
		while !captures.is_empty() {
			match captures {
				[_name, name, tail @ ..] if eq_slice(_name.node.byte_range(), b"_name") => {
					captures = tail;
					let name = name.node.byte_range().contract(1);
					if name.is_empty() {
						break;
					}
					model = Some(Model {
						model: String::from_utf8_lossy(&file[name.clone()]).to_string(),
						range: offset_range_to_lsp_range(name.map_unit(ByteOffset), rope.clone())
							.ok_or_else(|| diagnostic!("name range"))?,
						inherit: false,
					});
				}
				[_inherit, inherit, tail @ ..] if eq_slice(_inherit.node.byte_range(), b"_inherit") => {
					captures = tail;
					let inherit = inherit.node.byte_range().contract(1);
					if inherit.is_empty() {
						continue;
					}
					if model.is_none() {
						model = Some(Model {
							model: String::from_utf8_lossy(&file[inherit.clone()]).to_string(),
							range: offset_range_to_lsp_range(inherit.map_unit(ByteOffset), rope.clone())
								.ok_or_else(|| diagnostic!("inherit range"))?,
							inherit: true,
						});
					}
				}
				unk => Err(diagnostic!("Bug: Leftover captures: {unk:?}"))?,
			}
		}
		out.extend(model);
	}

	Ok(Output::Models {
		path: path.to_string_lossy().to_string().into(),
		models: out,
	})
}
