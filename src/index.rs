use std::path::{Path, PathBuf};
use std::time::Duration;

use dashmap::DashSet;
use globwalk::FileType;
use lasso::{Spur, ThreadedRodeo};
use log::{debug, warn};
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::notification::Progress;
use tower_lsp::lsp_types::*;
use tree_sitter::QueryCursor;
use ts_macros::query;
use xmlparser::{Token, Tokenizer};

use crate::model::{Model, ModelIndex, ModelType};
use crate::record::Record;
use crate::utils::{offset_range_to_lsp_range, ByteOffset, CharOffset, RangeExt};
use crate::{format_loc, ImStr};

mod record;
pub use record::{RecordId, SymbolMap, SymbolSet};
mod symbol;
pub use symbol::{interner, Symbol};

pub type Interner = ThreadedRodeo;

#[derive(Default)]
pub struct Index {
	pub roots: DashSet<ImStr>,
	pub modules: DashSet<ModuleName>,
	pub records: record::RecordIndex,
	pub models: ModelIndex,
}

pub type ModuleName = Symbol<Module>;
#[derive(Debug)]
pub enum Module {}

enum Output {
	Xml { records: Vec<Record> },
	Models { path: Spur, models: Vec<Model> },
}

pub struct AddRootResults {
	pub module_count: usize,
	pub record_count: usize,
	pub model_count: usize,
	pub elapsed: Duration,
}

impl Index {
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
		let interner = interner();
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
							message: Some(module_name.clone()),
							..Default::default()
						})),
					})
					.await;
			}
			let module_key = interner.get_or_intern(&module_name);
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
				Output::Xml { records } => {
					record_count += records.len();
					let mut prefix = self.records.by_prefix.write().await;
					self.records.extend_records(Some(&mut prefix), records, interner).await;
				}
				Output::Models { path, models } => {
					model_count += models.len();
					self.models.extend_models(path, interner, models).await;
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
			let module = interner().resolve(&entry.module);
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
			if let Some(module) = interner().get(module.as_ref()) {
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
	let path_uri = interner().get_or_intern(path.to_string_lossy().as_ref());
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
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(record);
				} else if local.as_str() == "template" {
					let template = Record::template(
						CharOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(template);
				} else if local.as_str() == "templates" {
					// QWeb templates
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

	Ok(Output::Xml { records })
}

query! {
	ModelQuery(NAME, INHERIT, MODEL);
r#"
((class_definition
	(argument_list [
		(identifier) @_Model
		(attribute (identifier) @_models (identifier) @_Model)
	])
	(block [	
		(expression_statement (assignment (identifier) @_name (string) @NAME))
		(expression_statement
			(assignment
				(identifier) @_inherit [
					(string) @INHERIT
					(list ((string) @INHERIT ","?)*)
				])
		)]*
	)) @MODEL
 (#eq? @_models "models")
 (#match? @_Model "^(Transient|Abstract)?Model$")
 (#eq? @_name "_name")
 (#eq? @_inherit "_inherit"))"#
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
	let query = ModelQuery::query();
	let mut cursor = QueryCursor::new();

	let mut out = vec![];
	let rope = Rope::from_str(&String::from_utf8_lossy(&contents));
	'match_: for match_ in cursor.matches(query, ast.root_node(), contents.as_slice()) {
		let mut inherits = vec![];
		let mut range = None;
		let mut maybe_base = None;
		for capture in match_.captures {
			if capture.index == ModelQuery::NAME && maybe_base.is_none() {
				let name = capture.node.byte_range().contract(1);
				if name.is_empty() {
					continue 'match_;
				}
				maybe_base = Some(String::from_utf8_lossy(&contents[name.clone()]));
			} else if capture.index == ModelQuery::INHERIT {
				let inherit = capture.node.byte_range().contract(1);
				if !inherit.is_empty() {
					inherits.push(ImStr::from(String::from_utf8_lossy(&contents[inherit]).as_ref()));
				}
			} else if capture.index == ModelQuery::MODEL && range.is_none() {
   					range = Some(capture.node.byte_range());
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
				type_: ModelType::Inherit(inherits),
				range: lsp_range,
				byte_range: range,
			}),
			([], None) => {}
			(_, Some(base)) => {
				if has_primary {
					out.push(Model {
						type_: ModelType::Inherit(inherits),
						range: lsp_range,
						byte_range: range,
					});
				} else {
					out.push(Model {
						type_: ModelType::Base(base.as_ref().into()),
						range: lsp_range,
						byte_range: range,
					})
				}
			}
		}
	}

	let path = interner().get_or_intern(path.to_string_lossy().as_ref());
	Ok(Output::Models { path, models: out })
}

#[cfg(test)]
mod tests {
	use crate::index::ModelQuery;
	use pretty_assertions::assert_eq;
	use tree_sitter::{Parser, QueryCursor};

	#[test]
	fn test_model_query() {
		let mut parser = Parser::new();
		parser.set_language(tree_sitter_python::language()).unwrap();
		let contents = br#"
class Foo(models.AbstractModel):
	_name = 'foo'
	_inherit = ['foo', 'bar']
	@api.depends('foo')
	def foo(self):
		pass
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = ModelQuery::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[&[
			"models",
			"AbstractModel",
			"_name",
			"'foo'",
			"_inherit",
			"'foo'",
			"'bar'",
		]];
		let actual = cursor
			.matches(query, ast.root_node(), &contents[..])
			.map(|match_| {
				match_
					.captures
					.iter()
					.skip(1)
					.map(|capture| String::from_utf8_lossy(&contents[capture.node.byte_range()]))
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();
		assert_eq!(expected, actual);
	}
}
