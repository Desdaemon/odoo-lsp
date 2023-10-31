use std::collections::HashMap;
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
use crate::utils::{ts_range_to_lsp_range, ByteOffset, CharOffset, RangeExt};
use crate::{format_loc, ImStr};

mod record;
pub use record::{RecordId, SymbolMap, SymbolSet};
mod symbol;
pub use symbol::{interner, Symbol};
mod template;
pub use crate::template::{Template, TemplateName};

use crate::template::{gather_templates, NewTemplate};

pub type Interner = ThreadedRodeo;

#[derive(Default)]
pub struct Index {
	pub roots: DashSet<ImStr>,
	pub modules: DashSet<ModuleName>,
	pub records: record::RecordIndex,
	pub templates: template::TemplateIndex,
	pub models: ModelIndex,
}

pub type ModuleName = Symbol<Module>;
#[derive(Debug)]
pub enum Module {}

enum Output {
	Xml {
		records: Vec<Record>,
		templates: Vec<NewTemplate>,
	},
	Models {
		path: Spur,
		models: Vec<Model>,
	},
}

pub struct AddRootResults {
	pub module_count: usize,
	pub record_count: usize,
	pub model_count: usize,
	pub template_count: usize,
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
		let mut template_count = 0;
		while let Some(outputs) = outputs.join_next().await {
			let outputs = match outputs {
				Ok(Ok(out)) => out,
				Ok(Err(err)) => {
					warn!("{}", err);
					continue;
				}
				Err(err) => {
					debug!("{err}");
					continue;
				}
			};
			match outputs {
				Output::Xml { records, templates } => {
					record_count += records.len();
					self.records.append(None, records, interner).await;
					template_count += templates.len();
					self.templates.append(templates).await;
				}
				Output::Models { path, models } => {
					model_count += models.len();
					self.models.append(path, interner, false, &models).await;
				}
			}
		}
		Ok(Some(AddRootResults {
			module_count,
			record_count,
			model_count,
			template_count,
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
	let mut templates = vec![];
	let rope = Rope::from_str(&file);
	loop {
		match reader.next() {
			Some(Ok(Token::ElementStart { local, span, .. })) => match local.as_str() {
				"record" => {
					let record = Record::from_reader(
						CharOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(record);
				}
				"template" => {
					let template = Record::template(
						CharOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(template);
				}
				"menuitem" => {
					let menuitem = Record::menuitem(
						CharOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(menuitem);
				}
				"templates" => {
					gather_templates(path_uri, &mut reader, rope.clone(), &mut templates)?;
				}
				_ => {}
			},
			None => break,
			Some(Err(err)) => {
				debug!("error parsing {}:\n{err}", path.display());
				break;
			}
			_ => {}
		}
	}

	Ok(Output::Xml { records, templates })
}

query! {
	ModelQuery(MODEL, NAME);
r#"
((class_definition
	(argument_list [
		(identifier) @_Model
		(attribute (identifier) @_models (identifier) @_Model)
	])
	(block
		(expression_statement
			(assignment (identifier) @NAME))
	)) @MODEL
 (#eq? @_models "models")
 (#match? @_Model "^(Transient|Abstract)?Model$")
 (#match? @NAME "^_(name|inherit)$"))"#
}

async fn add_root_py(path: PathBuf) -> miette::Result<Output> {
	let contents = tokio::fs::read(&path)
		.await
		.into_diagnostic()
		.with_context(|| format_loc!("Could not read {}", path.display()))?;

	let path = interner().get_or_intern(path.to_string_lossy().as_ref());
	let models = index_models(&contents)?;
	Ok(Output::Models { path, models })
}

pub fn index_models(contents: &[u8]) -> miette::Result<Vec<Model>> {
	let mut parser = tree_sitter::Parser::new();
	parser.set_language(tree_sitter_python::language()).into_diagnostic()?;

	let ast = parser
		.parse(contents, None)
		.ok_or_else(|| diagnostic!("AST not parsed"))?;
	let query = ModelQuery::query();
	let mut cursor = QueryCursor::new();

	// let rope = Rope::from_str(&String::from_utf8_lossy(contents));
	let mut models = HashMap::new();
	struct NewModel<'a> {
		range: tree_sitter::Range,
		byte_range: core::ops::Range<usize>,
		name: Option<&'a [u8]>,
		inherits: Vec<&'a [u8]>,
	}
	for match_ in cursor.matches(query, ast.root_node(), contents) {
		let model_node = match_
			.nodes_for_capture_index(ModelQuery::MODEL)
			.next()
			.ok_or_else(|| diagnostic!("(index_models) model_node"))?;
		let capture = match_
			.nodes_for_capture_index(ModelQuery::NAME)
			.next()
			.ok_or_else(|| diagnostic!("(index_models) name"))?;
		let model = models.entry(model_node.byte_range()).or_insert_with(|| NewModel {
			name: None,
			range: model_node.range(),
			byte_range: model_node.byte_range(),
			inherits: vec![],
		});
		match &contents[capture.byte_range()] {
			b"_name" => {
				let Some(name_decl) = capture.next_named_sibling() else {
					continue;
				};
				if name_decl.kind() == "string" {
					let name = name_decl.byte_range().contract(1);
					if name.is_empty() {
						continue;
					}
					model.name = Some(&contents[name]);
				}
			}
			b"_inherit" => {
				let Some(inherit_decl) = capture.next_named_sibling() else {
					continue;
				};
				match inherit_decl.kind() {
					"string" => {
						let inherit = inherit_decl.byte_range().contract(1);
						if !inherit.is_empty() {
							model.inherits.push(&contents[inherit]);
						}
					}
					"list" => {
						for maybe_inherit_decl in inherit_decl.named_children(&mut inherit_decl.walk()) {
							if maybe_inherit_decl.kind() == "string" {
								let inherit = maybe_inherit_decl.byte_range().contract(1);
								if !inherit.is_empty() {
									model.inherits.push(&contents[inherit]);
								}
							}
						}
					}
					_ => {
						continue;
					}
				}
			}
			_ => {}
		}
	}

	let models = models.into_values().flat_map(|mut model| {
		let mut has_primary = false;
		if !model.inherits.is_empty() {
			// Rearranges the primary inherit to the first index
			if let Some(base) = &model.name {
				if let Some(position) = model.inherits.iter().position(|inherit| inherit == base) {
					model.inherits.swap(position, 0);
					has_primary = true;
				}
			}
		}
		let inherits = model
			.inherits
			.into_iter()
			.map(|inherit| ImStr::from(String::from_utf8_lossy(inherit).as_ref()))
			.collect::<Vec<_>>();
		let range = ts_range_to_lsp_range(model.range);
		let byte_range = model.byte_range.map_unit(ByteOffset);
		match (inherits.is_empty(), model.name) {
			(false, None) => Some(Model {
				range,
				byte_range,
				type_: ModelType::Inherit(inherits),
			}),
			(true, None) => None,
			(_, Some(base)) => {
				if has_primary {
					Some(Model {
						range,
						byte_range,
						type_: ModelType::Inherit(inherits),
					})
				} else {
					Some(Model {
						type_: ModelType::Base(ImStr::from(String::from_utf8_lossy(base).as_ref())),
						range,
						byte_range,
					})
				}
			}
		}
	});
	Ok(models.collect())
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

class Bar(models.Model):
	def _default_stuff(): pass
	_name = 'bar'
	what = fields.Foo()
	_inherit = 'baz'
"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = ModelQuery::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[
			&["models", "AbstractModel", "_name"],
			&["models", "AbstractModel", "_inherit"],
			&["models", "Model", "_name"],
			&["models", "Model", "_inherit"],
		];
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
