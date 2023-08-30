use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use dashmap::DashSet;
use globwalk::FileType;
use lasso::{Spur, ThreadedRodeo};
use log::{debug, warn};
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::notification::Progress;
use tower_lsp::lsp_types::*;
use tree_sitter::{Query, QueryCursor};
use xmlparser::{Token, Tokenizer};

use crate::model::{Model, ModelId, ModelIndex};
use crate::record::Record;
use crate::utils::{offset_range_to_lsp_range, ByteOffset, CharOffset, RangeExt};
use crate::{format_loc, ImStr};

mod record;
pub use record::{RecordId, SymbolMap, SymbolSet};

#[derive(Default)]
pub struct ModuleIndex {
	pub roots: DashSet<ImStr>,
	pub modules: DashSet<ModuleName>,
	pub records: record::RecordIndex,
	pub models: ModelIndex,
	pub interner: Arc<ThreadedRodeo>,
}

/// Type-safe wrapper around [Spur].
#[derive(Debug)]
pub struct Symbol<T> {
	inner: Spur,
	_kind: PhantomData<T>,
}

pub type ModuleName = Symbol<Module>;
#[derive(Debug)]
pub enum Module {}

impl<T> From<Spur> for Symbol<T> {
	#[inline]
	fn from(inner: Spur) -> Self {
		Symbol {
			inner,
			_kind: PhantomData,
		}
	}
}

impl<T> Clone for Symbol<T> {
	#[inline]
	fn clone(&self) -> Self {
		Symbol {
			inner: self.inner.clone(),
			_kind: PhantomData,
		}
	}
}

impl<T> Copy for Symbol<T> {}
impl<T> Eq for Symbol<T> {}
impl<T> PartialEq for Symbol<T> {
	#[inline]
	fn eq(&self, other: &Self) -> bool {
		self.inner.eq(&other.inner)
	}
}

impl<T> Hash for Symbol<T> {
	#[inline]
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.inner.hash(state)
	}
}

impl<T> Deref for Symbol<T> {
	type Target = Spur;
	fn deref(&self) -> &Self::Target {
		&self.inner
	}
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
			let module_key = self.interner.get_or_intern(&module_name);
			if !self.modules.insert(module_key.into()) {
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
				outputs.spawn(add_root_xml(xml.path().to_path_buf(), module_key.into()));
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
				let path = py.path().to_path_buf();
				outputs.spawn(add_root_py(path));
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
					self.records
						.extend_records(Some(&mut prefix), records, &self.interner)
						.await;
				}
				Output::Models { path, models } => {
					model_count += models.len();
					self.models.extend_models(path, models).await;
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
			let module = self.interner.resolve(&entry.module);
			if root.contains(module) {
				entry.deleted = true;
			}
		}
	}
	pub fn module_of_path(&self, path: &Path) -> Option<dashmap::setref::one::Ref<ModuleName>> {
		let mut path = Some(path);
		while let Some(path_) = &path {
			let module = path_.file_name()?.to_string_lossy();
			// By this point, if this module hadn't been interned,
			// it certainly is not a valid module name.
			if let Some(module) = self.interner.get(module.as_ref()) {
				if let Some(name) = self.modules.get(&ModuleName::from(module)) {
					return Some(name);
				}
			}
			path = path_.parent();
		}
		None
	}
}

async fn add_root_xml(path: PathBuf, module_name: ModuleName) -> miette::Result<Output> {
	let path_uri = ImStr::from(path.to_string_lossy().as_ref());
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
						module_name,
						path_uri.clone(),
						&mut reader,
						rope.clone(),
					)?;
					records.extend(record);
				} else if local.as_str() == "template" {
					let template = Record::template(
						CharOffset(span.start()),
						module_name,
						path_uri.clone(),
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

async fn add_root_py(path: PathBuf) -> miette::Result<Output> {
	let contents = tokio::fs::read(&path)
		.await
		.into_diagnostic()
		.with_context(|| format_loc!("Could not read {}", path.display()))?;

	let mut parser = tree_sitter::Parser::new();
	parser.set_language(tree_sitter_python::language()).into_diagnostic()?;

	let ast = parser
		.parse(&contents, None)
		.ok_or_else(|| diagnostic!("AST not parsed"))?;
	let query = model_query();
	let mut cursor = QueryCursor::new();

	let mut out = vec![];
	let rope = Rope::from_str(&String::from_utf8_lossy(&contents));
	'match_: for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		let mut inherits = vec![];
		// let mut fields = vec![];
		let mut range = None;
		let mut maybe_base = None;
		// let mut current_field = None;
		for capture in match_.captures {
			if capture.index == 3 && maybe_base.is_none() {
				// @name
				let name = capture.node.byte_range().contract(1);
				if name.is_empty() {
					continue 'match_;
				}
				maybe_base = Some(String::from_utf8_lossy(&contents[name.clone()]));
			} else if capture.index == 5 {
				// @inherit
				let inherit = capture.node.byte_range().contract(1);
				if !inherit.is_empty() {
					inherits.push(ImStr::from(String::from_utf8_lossy(&contents[inherit]).as_ref()));
				}
			// } else if capture.index == 7 {
			// 	// @field
			// 	current_field = Some(capture.node.byte_range());
			// } else if capture.index == 10 {
			// 	// @relation
			// 	if let Some(current_field) = current_field.take() {
			// 		let field = String::from_utf8_lossy(&contents[current_field]);
			// 		let field = interner.get_or_intern(&field);
			// 		let field_relation = capture.node.byte_range().contract(1);
			// 		let field_relation = String::from_utf8_lossy(&contents[field_relation]);
			// 		let field_relation = interner.get_or_intern(field_relation.as_ref());
			// 		fields.push((field, Field::Relational(field_relation)));
			// 	}
			// } else if capture.index == 12 {
			// 	// @_Type
			// 	if let Some(current_field) = current_field.take() {
			// 		let field = String::from_utf8_lossy(&contents[current_field]);
			// 		let field = interner.get_or_intern(&field);
			// 		fields.push((field, Field::Value));
			// 	}
			} else if capture.index == 6 {
				// @model
				if range.is_none() {
					range = Some(capture.node.byte_range());
				}
			}
		}
		let Some(range) = range else { continue };
		let range = range.map_unit(ByteOffset);
		let lsp_range =
			offset_range_to_lsp_range(range.clone(), rope.clone()).ok_or_else(|| diagnostic!("model out of bounds"))?;
		let mut has_primary = inherits.len() == 1;
		if !inherits.is_empty() {
			// Rearranges the primary inherit to the first index
			if let Some(maybe_base) = &maybe_base {
				if let Some(position) = inherits.iter().position(|inherit| inherit == maybe_base) {
					inherits.swap(position, 0);
					has_primary = true;
				} else {
					has_primary = false;
				}
			}
		}
		match (inherits.as_slice(), maybe_base) {
			([_, ..], None) => out.push(Model {
				model: ModelId::Inherit(inherits),
				range: lsp_range,
				byte_range: range,
			}),
			([], None) => {}
			(_, Some(base)) => {
				if has_primary {
					out.push(Model {
						model: ModelId::Inherit(inherits),
						range: lsp_range,
						byte_range: range,
					});
				} else {
					out.push(Model {
						model: ModelId::Base(base.as_ref().into()),
						range: lsp_range,
						byte_range: range,
					})
				}
			}
		}
	}

	Ok(Output::Models {
		path: path.to_string_lossy().to_string().into(),
		models: out,
	})
}
