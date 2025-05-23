//! The main indexer for all language items, including [`Record`]s, QWeb [`Template`]s, and Owl [`Component`]s

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use anyhow::anyhow;
use dashmap::DashMap;
use derive_more::Deref;
use globwalk::FileType;
use ignore::gitignore::Gitignore;
use ignore::Match;
use lasso::{Spur, ThreadedRodeo};
use request::WorkDoneProgressCreate;
use ropey::Rope;
use smart_default::SmartDefault;
use tower_lsp_server::{lsp_types::*, Client};
use tracing::{debug, warn};
use tree_sitter::QueryCursor;
use ts_macros::query;
use xmlparser::{Token, Tokenizer};

pub use crate::component::{Component, ComponentName};
use crate::model::{Model, ModelIndex, ModelType};
use crate::record::Record;
use crate::template::{gather_templates, NewTemplate};
pub use crate::template::{Template, TemplateName};
use crate::utils::{path_contains, ts_range_to_lsp_range, uri_from_file_path, ByteOffset, ByteRange, MinLoc, RangeExt};
use crate::{errloc, format_loc, loc, ok, ImStr};

mod js;
mod record;
mod symbol;
mod template;

pub use js::JsQuery;
pub use record::{RecordId, SymbolMap, SymbolSet};
pub use symbol::{PathSymbol, Symbol, _G, _I, _P, _R};
pub use template::TemplateIndex;

pub type Interner = Wrapper<ThreadedRodeo>;

#[derive(Default, Deref)]
#[repr(transparent)]
pub struct Wrapper<T>(#[deref] T);

impl Interner {
	fn get_counter(&self) -> &'static DashMap<Spur, u32> {
		static COUNTER: std::sync::OnceLock<DashMap<Spur, u32>> = std::sync::OnceLock::new();
		COUNTER.get_or_init(|| DashMap::with_shard_amount(4))
	}
	pub fn get_or_intern<T: AsRef<str>>(&self, value: T) -> Spur {
		let out = self.0.get_or_intern(value);
		*self.get_counter().entry(out).or_default().value_mut() += 1;
		out
	}
	pub fn get_or_intern_path(&self, path: &Path) -> Spur {
		self.get_or_intern(path.to_string_lossy())
	}
}

#[derive(SmartDefault)]
pub struct Index {
	/// root -> module key -> module's relpath to root
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub roots: DashMap<PathBuf, HashMap<Symbol<Module>, ImStr>>,
	pub records: record::RecordIndex,
	pub templates: template::TemplateIndex,
	pub models: ModelIndex,
	pub components: js::ComponentIndex,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub widgets: DashMap<ImStr, MinLoc>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub actions: DashMap<ImStr, MinLoc>,
}

pub type ModuleName = Symbol<Module>;
#[derive(Debug)]
pub enum Module {}

enum Output {
	Nothing,
	Xml {
		records: Vec<Record>,
		templates: Vec<NewTemplate>,
	},
	Models {
		path: PathSymbol,
		models: Vec<Model>,
	},
	JsItems {
		components: HashMap<ComponentName, Component>,
		widgets: Vec<(ImStr, MinLoc)>,
		actions: Vec<(ImStr, MinLoc)>,
	},
}

#[derive(Debug)]
pub struct AddRootResults {
	pub module_count: usize,
	pub record_count: usize,
	pub model_count: usize,
	pub template_count: usize,
	pub component_count: usize,
	pub elapsed: Duration,
}

impl Index {
	pub fn delete_marked_entries(&self) {
		self.records.retain(|_, record| !record.deleted);
		self.models.retain(|_, model| !model.deleted);

		for mut model in self.models.iter_mut() {
			let before = model.ancestors.len();
			model.ancestors.retain(|key| self.models.contains_key(key));
			if model.ancestors.len() != before {
				model.fields = None;
				model.methods = None;
			}
		}
	}
	#[tracing::instrument(skip_all, fields(root=format!("{}", root.display())), ret)]
	pub async fn add_root(
		&self,
		root: &Path,
		client: Option<Client>,
		tsconfig: bool,
	) -> anyhow::Result<Option<AddRootResults>> {
		if self.roots.contains_key(root) {
			return Ok(None);
		}

		let token = ProgressToken::String(format!(
			"odoo-lsp/indexing/{}",
			root.file_name().and_then(|ostr| ostr.to_str()).unwrap_or("<<invalid>>")
		));

		let mut progress = None;
		if let Some(client) = client.as_ref() {
			if client
				.send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams { token: token.clone() })
				.await
				.is_ok()
			{
				progress = Some(Arc::new(client.progress(token, "Indexing").begin().await));
			}
		}

		let t0 = tokio::time::Instant::now();
		let manifests = ok!(
			globwalk::glob_builder(root.join("**/__manifest__.py").to_string_lossy())
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build(),
			"Could not glob into {:?}",
			root
		);
		let mut gitignore = ignore::gitignore::GitignoreBuilder::new(root);
		gitignore
			.add(".gitignore")
			.inspect(|err| warn!("error adding {root:?}/.gitignore: {err:?}"));
		let gitignore = gitignore
			.build()
			.inspect_err(|err| warn!("gitignore error for {root:?}: {err:?}"))
			.ok();

		let mut module_count = 0;
		let mut outputs = tokio::task::JoinSet::new();
		let root_key = _I(root.to_string_lossy());
		for manifest in manifests {
			let Ok(manifest) = manifest else { continue };
			let Some(module_dir) = manifest.path().parent() else {
				continue;
			};
			fn matched_top_to_bottom(gitignore: &Gitignore, path: &Path) -> bool {
				let ancestors = path.ancestors().collect::<Vec<_>>();
				for ancestor in ancestors.into_iter().rev() {
					if let Match::Ignore(_) = gitignore.matched(ancestor, true) {
						return true;
					}
				}
				false
			}
			if let Some(true) = gitignore
				.as_ref()
				.map(|g| matched_top_to_bottom(g, module_dir.strip_prefix(root).unwrap()))
			{
				continue;
			}
			module_count += 1;
			let module_name = module_dir.file_name().unwrap().to_string_lossy().to_string();
			let module_key = _I(&module_name);
			let module_path = ok!(
				module_dir.strip_prefix(root),
				"module_dir={:?} is not a subpath of root={:?}",
				module_dir,
				root
			);
			let duplicate = self
				.roots
				.entry(root.into())
				.or_default()
				.insert(module_key.into(), module_path.to_str().expect("non-utf8 path").into());
			if let Some(duplicate) = duplicate {
				if let (Some(client), "base") = (client.clone(), module_name.as_str()) {
					tokio::spawn(Self::notify_duplicate_base(
						client,
						duplicate,
						module_path.to_path_buf(),
					));
				}
			}
			if tsconfig {
				continue;
			}
			let xmls = ok!(
				globwalk::glob_builder(format!("{}/**/*.xml", module_dir.display()))
					.file_type(FileType::FILE | FileType::SYMLINK)
					.follow_links(true)
					.build(),
				"Could not glob into {:?}",
				manifest.path()
			);
			for xml in xmls {
				let Ok(xml) = xml else { continue };
				outputs.spawn(add_root_xml(root_key, xml.path().to_path_buf(), module_key.into()));
			}
			let pys = ok!(
				globwalk::glob_builder(format!("{}/**/*.py", module_dir.display()))
					.file_type(FileType::FILE | FileType::SYMLINK)
					.follow_links(true)
					.build(),
				"Could not glob into {:?}",
				manifest.path()
			);
			for py in pys {
				let py = match py {
					Ok(entry) => entry,
					Err(err) => {
						debug!("{err}");
						continue;
					}
				};
				let path = py.path().to_path_buf();
				outputs.spawn(add_root_py(root_key, path));
			}
			let scripts = ok!(
				globwalk::glob_builder(format!("{}/**/*.js", module_dir.display()))
					.file_type(FileType::FILE | FileType::SYMLINK)
					.follow_links(true)
					.build(),
				"Could not glob into {:?}",
				manifest.path()
			);
			for js in scripts {
				let Ok(js) = js else { continue };
				let path = js.path().to_path_buf();
				outputs.spawn(js::add_root_js(root_key, path));
			}
			if let Some(progress) = progress.clone() {
				// we cannot use tokio::spawn here, because later the progress will
				// be unwrapped and we want to ensure it's the only instance by then
				// this means that this cloned process cannot outlive outputs
				outputs.spawn(async move {
					progress.report(module_name.clone()).await;
					Ok(Output::Nothing)
				});
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
					warn!("task failed: {err}");
					continue;
				}
				Err(err) => {
					debug!("join error: {err}");
					continue;
				}
			};
			match outputs {
				Output::Nothing => {}
				Output::Xml { records, templates } => {
					record_count += records.len();
					self.records.append(None, records).await;
					template_count += templates.len();
					self.templates.append(templates).await;
				}
				Output::Models { path, models } => {
					model_count += models.len();
					self.models.append(path, false, &models).await;
				}
				Output::JsItems {
					components,
					widgets,
					actions,
				} => {
					component_count += components.len();
					self.components.extend(components);
					for (widget, loc) in widgets {
						if !self.widgets.contains_key(&widget) {
							self.widgets.insert(widget, loc);
						}
					}
					for (action, loc) in actions {
						if !self.actions.contains_key(&action) {
							self.actions.insert(action, loc);
						}
					}
				}
			}
		}
		if let Some(progress) = progress.take() {
			Arc::into_inner(progress)
				.unwrap()
				.finish_with_message(format!("completed for {}", root.to_string_lossy()))
				.await;
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
	async fn notify_duplicate_base(client: Client, old_path: ImStr, new_path: PathBuf) {
		let resp = client
			.show_message_request(
				MessageType::WARNING,
				concat!(
					"Duplicate base module found. The language server's performance may be affected.\n",
					"Please configure your workspace (or .odoo_lsp) to only include one version of Odoo as described in the wiki.",
				)
				.to_string(),
				Some(vec![
					MessageActionItem {
						title: "Go to odoo-lsp wiki".to_string(),
						properties: Default::default(),
					},
					MessageActionItem {
						title: "Dismiss".to_string(),
						properties: Default::default(),
					},
					MessageActionItem {
						title: "Show conflicting modules".to_string(),
						properties: Default::default(),
					},
				]),
			)
			.await;
		match resp {
			Ok(Some(resp)) if resp.title == "Go to odoo-lsp wiki" => {
				_ = client
					.show_document(ShowDocumentParams {
						uri: Uri::from_str("https://github.com/Desdaemon/odoo-lsp/wiki#usage").unwrap(),
						external: Some(true),
						take_focus: None,
						selection: None,
					})
					.await;
			}
			Ok(Some(resp)) if resp.title == "Show conflicting modules" => {
				_ = client
					.show_document(ShowDocumentParams {
						uri: uri_from_file_path(Path::new(old_path.as_str())).unwrap(),
						external: Some(false),
						take_focus: Some(true),
						selection: None,
					})
					.await;
				_ = client
					.show_document(ShowDocumentParams {
						uri: uri_from_file_path(&new_path).unwrap(),
						external: Some(false),
						take_focus: Some(false),
						selection: None,
					})
					.await;
			}
			_ => {}
		}
	}
	pub fn remove_root(&self, root: &Path) {
		self.roots.remove(root);
		for mut entry in self.records.iter_mut() {
			let module = _R(entry.module);
			if path_contains(root, module) {
				entry.deleted = true;
			}
		}

		for mut entry in self.models.iter_mut() {
			if entry
				.base
				.as_ref()
				.is_some_and(|base| path_contains(root, base.0.path.to_path()))
			{
				entry.deleted = true;
			}
		}
	}
	/// Has complexity of `O(len(self.roots))`
	pub fn module_of_path(&self, path: &Path) -> Option<ModuleName> {
		use std::path::Component;
		for entry in self.roots.iter() {
			if let Ok(path) = path.strip_prefix(entry.key()) {
				for component in path.components() {
					if let Component::Normal(norm) = component {
						if let Some(module) = _G(norm.to_string_lossy()) {
							if entry.value().contains_key(&module.into()) {
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

async fn add_root_xml(root: Spur, path: PathBuf, module_name: ModuleName) -> anyhow::Result<Output> {
	let path_uri = PathSymbol::strip_root(root, &path);
	let file = ok!(tokio::fs::read(&path).await, "Could not read {}", path.display());
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

#[rustfmt::skip]
query! {
	#[derive(Debug, PartialEq, Eq)]
	ModelQuery(Model, Name);
((class_definition
  (argument_list [
    (identifier) @_Model
    (attribute
      (identifier) @_models (identifier) @_Model) ])
  (block
    (expression_statement
      (assignment (identifier) @NAME)) )) @MODEL
  (#eq? @_models "models")
  (#match? @_Model "^(Transient|Abstract)?Model$")
  (#match? @NAME "^_(name|inherits?)$"))
}

async fn add_root_py(root: Spur, path: PathBuf) -> anyhow::Result<Output> {
	let contents = ok!(tokio::fs::read(&path).await, "Could not read {}", path.display());

	let path = PathSymbol::strip_root(root, &path);
	let models = index_models(&contents)?;
	Ok(Output::Models { path, models })
}

pub fn index_models(contents: &[u8]) -> anyhow::Result<Vec<Model>> {
	let mut parser = tree_sitter::Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into())?;

	let ast = parser
		.parse(contents, None)
		.ok_or_else(|| anyhow!("{} AST not parsed", loc!()))?;
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
			.ok_or_else(|| errloc!("model_node"))?;
		let capture = match_
			.nodes_for_capture_index(ModelQuery::Name as _)
			.next()
			.ok_or_else(|| errloc!("capture"))?;
		let model = models.entry(model_node.id()).or_insert_with(|| NewModel {
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
				// if _name is not defined and _inherit = [A, B, C], A is considered the inherit base by default
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

#[cfg(test)]
mod tests {
	use crate::index::ModelQuery;
	use pretty_assertions::assert_eq;
	use tree_sitter::{Parser, QueryCursor};

	use super::index_models;

	#[test]
	fn test_model_query() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
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

class Base(models.Model):
	_name = 'haha'
	_description = 'What?'
	_order = 'foo, bar'

"#;
		let ast = parser.parse(&contents[..], None).unwrap();
		let query = ModelQuery::query();
		let mut cursor = QueryCursor::new();
		let expected: &[&[&str]] = &[
			&["models", "AbstractModel", "_name"],
			&["models", "AbstractModel", "_inherit"],
			&["models", "Model", "_name"],
			&["models", "Model", "_inherit"],
			&["models", "Model", "_name"],
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

	#[test]
	fn test_index_query() {
		let models = index_models(
			br#"
class TransifexCodeTranslation(models.Model):
    _name = "transifex.code.translation"
    _description = "Code Translation"
    _log_access = False
"#,
		)
		.unwrap();
		assert!(matches!(
			&models[..],
			[super::Model {
				type_: super::ModelType::Base { .. },
				..
			}]
		));
	}
}
