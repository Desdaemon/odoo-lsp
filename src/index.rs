use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

use dashmap::DashMap;
use globwalk::FileType;
use lasso::{Key, Spur, ThreadedRodeo};
use log::{debug, info, warn};
use miette::{diagnostic, Context, IntoDiagnostic};
use ropey::Rope;
use tower_lsp::lsp_types::notification::{Progress, ShowMessage};
use tower_lsp::lsp_types::request::{ShowDocument, ShowMessageRequest};
use tower_lsp::lsp_types::*;
use tree_sitter::QueryCursor;
use ts_macros::query;
use xmlparser::{Token, Tokenizer};

use crate::model::{Model, ModelIndex, ModelType};
use crate::record::Record;
use crate::utils::{ts_range_to_lsp_range, ByteOffset, ByteRange, RangeExt, Usage};
use crate::{format_loc, ImStr};

mod record;
pub use record::{RecordId, SymbolMap, SymbolSet};
mod symbol;
pub use symbol::{interner, Symbol};
mod template;
pub use crate::template::{Template, TemplateName};
pub use template::TemplateIndex;
mod component;
pub use crate::component::{Component, ComponentName};
pub use component::ComponentQuery;

use crate::template::{gather_templates, NewTemplate};

pub type Interner = ThreadedRodeo;

#[derive(Default)]
pub struct Index {
	pub roots: DashMap<ImStr, SymbolMap<Module, ImStr>>,
	pub records: record::RecordIndex,
	pub templates: template::TemplateIndex,
	pub models: ModelIndex,
	pub components: component::ComponentIndex,
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
	Components(HashMap<ComponentName, Component>),
}

pub struct AddRootResults {
	pub module_count: usize,
	pub record_count: usize,
	pub model_count: usize,
	pub template_count: usize,
	pub component_count: usize,
	pub elapsed: Duration,
}

impl Index {
	pub fn mark_n_sweep(&self) {
		let pre = self.records.len();
		self.records.retain(|_, record| !record.deleted);
		info!("(mark_n_sweep) deleted {} records", pre - self.records.len());
	}
	pub async fn add_root(
		&self,
		root: &str,
		progress: Option<(&tower_lsp::Client, ProgressToken)>,
		tsconfig: bool,
	) -> miette::Result<Option<AddRootResults>> {
		if self.roots.contains_key(root) {
			return Ok(None);
		}

		let t0 = tokio::time::Instant::now();
		let manifests = globwalk::glob_builder(format!("{root}/**/__manifest__.py"))
			.file_type(FileType::FILE | FileType::SYMLINK)
			.follow_links(true)
			.build()
			.into_diagnostic()
			.with_context(|| format!("Could not glob into {root}"))?;

		let mut module_count = 0;
		let mut outputs = tokio::task::JoinSet::new();
		let interner = interner();
		for manifest in manifests {
			module_count += 1;
			let manifest = manifest.into_diagnostic()?;
			let module_dir = manifest
				.path()
				.parent()
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
			let module_path = module_dir
				.strip_prefix(root)
				.map_err(|_| miette::diagnostic!("module_dir={:?} is not a subpath of root={}", module_dir, root))?;
			if !self.roots.entry(root.into()).or_default().insert_checked(
				module_key.into_usize() as _,
				module_path.to_str().expect("non-utf8 path").into(),
			) && module_name == "base"
			{
				if let Some((client, _)) = &progress {
					let resp = client
						.send_request::<ShowMessageRequest>(ShowMessageRequestParams {
							typ: MessageType::WARNING,
							message: concat!(
								"Duplicate base module found. The language server's performance may be affected.\n",
								"Please configure your workspace (or .odoo_lsp) to only include one version of Odoo as described in the wiki.",
							)
							.to_string(),
							actions: Some(vec![
								MessageActionItem {
									title: "Go to odoo-lsp wiki".to_string(),
									properties: Default::default(),
								},
								MessageActionItem {
									title: "Dismiss".to_string(),
									properties: Default::default(),
								},
							]),
						})
						.await;
					match resp {
						Ok(Some(resp)) if resp.title == "Go to odoo-lsp wiki" => {
							_ = client
								.send_request::<ShowDocument>(ShowDocumentParams {
									uri: Url::parse("https://github.com/Desdaemon/odoo-lsp/wiki#usage").unwrap(),
									external: Some(true),
									take_focus: None,
									selection: None,
								})
								.await;
						}
						_ => {}
					}
				}
			}
			if tsconfig {
				continue;
			}
			let xmls = globwalk::glob_builder(format!("{}/**/*.xml", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format_loc!("Could not glob into {:?}", manifest.path()))?;
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
				.with_context(|| format_loc!("Could not glob into {:?}", manifest.path()))?;
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
			let scripts = globwalk::glob_builder(format!("{}/**/*.js", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
				.into_diagnostic()
				.with_context(|| format_loc!("Could not glob into {:?}", manifest.path()))?;
			for js in scripts {
				let Ok(js) = js else { continue };
				let path = js.path().to_path_buf();
				outputs.spawn(component::add_root_js(path));
			}
		}

		let mut record_count = 0;
		let mut model_count = 0;
		let mut template_count = 0;
		let mut component_count = 0;
		while let Some(outputs) = outputs.join_next().await {
			let outputs = match outputs {
				Ok(Ok(out)) => out,
				Ok(Err(err)) => {
					warn!("{}", err);
					continue;
				}
				Err(err) => {
					debug!("task failed: {err}");
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
				Output::Components(components) => {
					component_count += components.len();
					self.components.extend(components);
				}
			}
		}
		Ok(Some(AddRootResults {
			module_count,
			record_count,
			model_count,
			template_count,
			component_count,
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
	pub fn module_of_path(&self, path: &Path) -> Option<ModuleName> {
		use std::path::Component;
		for entry in self.roots.iter() {
			if let Ok(path) = path.strip_prefix(entry.key()) {
				for component in path.components() {
					if let Component::Normal(norm) = component {
						if let Some(module) = interner().get(&norm.to_string_lossy()) {
							if entry.value().contains_key(module.into_usize() as _) {
								return Some(module.into());
							}
						}
					}
				}
			}
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
						ByteOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(record);
				}
				"template" => {
					if let Some(template) = Record::template(
						ByteOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)? {
						records.push(template);
					} else {
						gather_templates(path_uri, &mut reader, rope.clone(), &mut templates, true)?;
					}
				}
				"menuitem" => {
					let menuitem = Record::menuitem(
						ByteOffset(span.start()),
						module_name,
						path_uri,
						&mut reader,
						rope.clone(),
					)?;
					records.extend(menuitem);
				}
				"templates" => {
					gather_templates(path_uri, &mut reader, rope.clone(), &mut templates, false)?;
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
	ModelQuery(Model, Name);
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
 (#match? @NAME "^_(name|inherits?)$"))"#
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

	let mut models = HashMap::new();
	struct NewModel<'a> {
		range: tree_sitter::Range,
		byte_range: ByteRange,
		name: Option<&'a [u8]>,
		inherits: Vec<&'a [u8]>,
	}
	for match_ in cursor.matches(query, ast.root_node(), contents) {
		let model_node = match_
			.nodes_for_capture_index(ModelQuery::Model as _)
			.next()
			.ok_or_else(|| diagnostic!("(index_models) model_node"))?;
		let capture = match_
			.nodes_for_capture_index(ModelQuery::Name as _)
			.next()
			.ok_or_else(|| diagnostic!("(index_models) name"))?;
		let model = models.entry(model_node.byte_range()).or_insert_with(|| NewModel {
			name: None,
			range: model_node.range(),
			byte_range: model_node.byte_range().map_unit(ByteOffset),
			inherits: vec![],
		});
		match &contents[capture.byte_range()] {
			b"_name" => {
				let Some(name_decl) = capture.next_named_sibling() else {
					continue;
				};
				if name_decl.kind() == "string" {
					let name = name_decl.byte_range().shrink(1);
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
						let inherit = inherit_decl.byte_range().shrink(1);
						if !inherit.is_empty() {
							model.inherits.push(&contents[inherit]);
						}
					}
					"list" => {
						for maybe_inherit_decl in inherit_decl.named_children(&mut inherit_decl.walk()) {
							if maybe_inherit_decl.kind() == "string" {
								let inherit = maybe_inherit_decl.byte_range().shrink(1);
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
			b"_inherits" => {
				let Some(inherit_dict) = capture.next_named_sibling() else {
					continue;
				};
				if inherit_dict.kind() != "dictionary" {
					continue;
				}
				for pair in inherit_dict.named_children(&mut inherit_dict.walk()) {
					if pair.kind() != "pair" {
						continue;
					}
					let key = pair.named_child(0).expect("key");
					if key.kind() == "string" {
						model.inherits.push(&contents[key.byte_range().shrink(1)]);
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
		let byte_range = model.byte_range;
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
						type_: ModelType::Base {
							name: ImStr::from(String::from_utf8_lossy(base).as_ref()),
							ancestors: inherits,
						},
						range,
						byte_range,
					})
				}
			}
		}
	});
	Ok(models.collect())
}

impl Index {
	pub fn statistics(&self) -> serde_json::Value {
		let Self {
			roots,
			records,
			templates,
			models,
			components,
		} = self;
		let mut modules = roots.usage();
		modules.0 = roots.iter().map(|entry| entry.value().len()).sum::<usize>();
		serde_json::json! {{
			"modules": modules,
			"records": records.statistics(),
			"templates": templates.statistics(),
			"models": models.statistics(),
			"components": components.statistics(),
		}}
	}
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
