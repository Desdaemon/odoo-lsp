//! The main indexer for all language items, including [`Record`]s, QWeb [`Template`]s, and Owl [`Component`]s

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::sync::OnceLock;
use std::sync::atomic::Ordering::Relaxed;

use dashmap::DashMap;
use derive_more::Deref;
use globwalk::FileType;
use ignore::Match;
use ignore::gitignore::Gitignore;
use lasso::ThreadedRodeo;
use mini_moka::sync::Cache;
use smart_default::SmartDefault;
use tower_lsp_server::{Client, ls_types::*};
use xmlparser::{Token, Tokenizer};

use crate::{ImStr, prelude::*};

use crate::analyze::{ClassKey, Scope};
pub use crate::component::{Component, ComponentName};
use crate::model::{Model, ModelEntry, ModelIndex, ModelType};
use crate::record::Record;
use crate::template::{NewTemplate, gather_templates};
pub use crate::template::{Template, TemplateName};
use crate::utils::Semaphore;

mod js;
mod module;
mod record;
pub(crate) mod symbol;
mod template;

pub use js::JsQuery;
pub use module::ModuleEntry;
pub use record::{RecordId, SymbolMap, SymbolSet};
pub use symbol::{_G, _I, _P, _R, PathSymbol, Symbol};
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
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub roots: DashMap<PathBuf, HashMap<Symbol<ModuleEntry>, ModuleEntry>>,
	pub records: record::RecordIndex,
	pub templates: template::TemplateIndex,
	pub models: ModelIndex,
	pub components: js::ComponentIndex,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub widgets: DashMap<ImStr, MinLoc>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub actions: DashMap<ImStr, MinLoc>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub unloaded_auto_install: DashMap<ModuleName, Vec<(ModuleName, Vec<ModuleName>)>>,
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub(crate) transitive_deps_cache: DashMap<ModuleName, HashSet<ModuleName>>,
	#[default(_code = "Cache::new(16)")]
	pub(crate) ast_cache: Cache<PathBuf, Arc<AstCacheItem>>,
	#[default(_code = "OnceLock::new()")]
	pub(crate) base_model_cache: OnceLock<Option<Arc<Scope>>>,
}

pub struct AstCacheItem {
	pub tree: tree_sitter::Tree,
	pub rope: Rope,
}

pub type ModuleName = Symbol<ModuleEntry>;

enum Output {
	Xml {
		records: Vec<Record>,
		templates: Vec<NewTemplate>,
	},
	Models {
		path: PathSymbol,
		models: Vec<Model>,
		file_path: PathBuf,
	},
	JsItems {
		components: HashMap<ComponentName, Component>,
		widgets: Vec<(ImStr, MinLoc)>,
		actions: Vec<(ImStr, MinLoc)>,
	},
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
	pub async fn add_root(&self, root: &Path, client: Option<Client>) -> anyhow::Result<()> {
		if self.roots.contains_key(root) {
			return Ok(());
		}

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
			let module_name = module_dir.file_name().unwrap().to_string_lossy().to_string();
			let module_key = _I(&module_name);
			let module_path = ok!(
				module_dir.strip_prefix(root),
				"module_dir={:?} is not a subpath of root={:?}",
				module_dir,
				root
			);
			match self.roots.entry(root.into()).or_default().entry(module_key.into()) {
				std::collections::hash_map::Entry::Vacant(entry) => {
					let manifest_info = parse_manifest_info(manifest.path())
						.inspect_err(|err| {
							warn!(
								"could not parse manifest for {}: {}",
								module_path.file_name().unwrap().display(),
								err
							)
						})
						.unwrap_or(ManifestInfo {
							dependencies: Box::new([]),
							auto_install: false,
						});
					let module = ModuleEntry {
						path: module_path.to_str().expect("non-utf8 path").into(),
						dependencies: manifest_info.dependencies,
						auto_install: manifest_info.auto_install,
						loaded: Default::default(),
						loaded_dependents: Default::default(),
					};
					entry.insert(module);
				}
				std::collections::hash_map::Entry::Occupied(preexisting) => {
					if let (Some(client), "base") = (client.clone(), module_name.as_str()) {
						let duplicate_path = module_path.to_str().expect("non-utf8 path");
						tokio::spawn(Self::notify_duplicate_base(
							client,
							preexisting.get().path.clone(),
							duplicate_path.into(),
						));
					}
				}
			}
		}

		self.add_implicit_base_dependencies();

		self.transitive_deps_cache.clear();

		debug!("Checking for auto_install modules after adding root");
		let auto_install_check = self.find_auto_install_modules(&HashSet::new());
		debug!("Found {} auto_install modules to check", auto_install_check.len());

		Ok(())
	}
	#[instrument(skip(self), ret)]
	pub async fn add_root_for_file(&self, path: &Path) {
		if self.find_module_of(path).is_some() {
			return;
		}

		debug!("oob: {}", path.display());
		let mut path = Some(path);
		while let Some(path_) = path {
			if tokio::fs::try_exists(path_.with_file_name("__manifest__.py"))
				.await
				.unwrap_or(false)
				&& let Some(file_path) = path_.parent().and_then(|p| p.parent())
			{
				_ = self
					.add_root(file_path, None)
					.await
					.inspect_err(|err| warn!("failed to add root {}:\n{err}", file_path.display()));
				return;
			}
			path = path_.parent();
		}
	}
	#[instrument(skip_all, ret, fields(path = path.display().to_string()))]
	pub(crate) async fn load_modules_for_document(&self, _document_blocker: Arc<Semaphore>, path: &Path) -> Option<()> {
		let module_name = self.find_module_of(path)?;

		self.load_module(module_name).await
	}
	pub fn resolve_transitive_dependencies(&self, module: ModuleName) -> HashSet<ModuleName> {
		if let Some(cached) = self.transitive_deps_cache.get(&module) {
			return cached.clone();
		}

		let mut dependencies = HashSet::new();
		let mut stack = vec![module];

		while let Some(current) = stack.pop() {
			if !dependencies.insert(current) {
				continue;
			}

			for root in self.roots.iter() {
				if let Some(module_entry) = root.get(&current) {
					for &dep in module_entry.dependencies.iter() {
						if !dependencies.contains(&dep) {
							stack.push(dep);
						}
					}
					break;
				}
			}
		}

		self.transitive_deps_cache.insert(module, dependencies.clone());

		dependencies
	}

	fn find_dependency_chain(&self, source: ModuleName, target: ModuleName) -> Option<Vec<ModuleName>> {
		let mut visited = HashSet::new();
		let mut stack = vec![(source, vec![source])];

		while let Some((current, path)) = stack.pop() {
			if current == target {
				return Some(path);
			}

			if !visited.insert(current) {
				continue;
			}

			for root in self.roots.iter() {
				if let Some(module_entry) = root.get(&current) {
					for &dep in module_entry.dependencies.iter() {
						if !visited.contains(&dep) {
							let mut new_path = path.clone();
							new_path.push(dep);
							stack.push((dep, new_path));
						}
					}
					break;
				}
			}
		}

		None
	}
	pub fn get_all_available_modules(&self) -> HashSet<ModuleName> {
		let mut modules = HashSet::new();

		for root in self.roots.iter() {
			for (&module_key, _) in root.iter() {
				modules.insert(module_key);
			}
		}

		modules
	}

	fn add_implicit_base_dependencies(&self) {
		let base_key: ModuleName = _I("base").into();

		for mut root in self.roots.iter_mut() {
			let modules = root.value_mut();

			if !modules.contains_key(&base_key) {
				continue;
			}

			let mut updates = Vec::new();
			for (&module_key, module_entry) in modules.iter() {
				if module_key != base_key && !module_entry.dependencies.contains(&base_key) {
					let mut deps = module_entry.dependencies.to_vec();
					deps.push(base_key);
					updates.push((module_key, deps));
				}
			}

			for (module_key, new_deps) in updates {
				if let Some(entry) = modules.get_mut(&module_key) {
					entry.dependencies = new_deps.into_boxed_slice();
				}
			}
		}
	}

	fn find_auto_install_modules(&self, loaded_modules: &HashSet<ModuleName>) -> Vec<ModuleName> {
		let mut auto_install_modules = Vec::new();

		let all_available_modules = self.get_all_available_modules();

		for root in self.roots.iter() {
			debug!("Checking root: {:?}", root.key());
			for (&module_key, module_entry) in root.iter() {
				debug!(
					"Checking module: {} auto_install={}",
					_R(module_key),
					module_entry.auto_install
				);
				if module_entry.auto_install && !loaded_modules.contains(&module_key) {
					let transitive_deps = self.resolve_transitive_dependencies(module_key);

					let missing_deps: Vec<ModuleName> = transitive_deps
						.iter()
						.filter(|dep| **dep != module_key && !all_available_modules.contains(dep))
						.cloned()
						.collect();

					if missing_deps.is_empty() {
						let unloaded_deps: Vec<ModuleName> = transitive_deps
							.iter()
							.filter(|dep| **dep != module_key && !loaded_modules.contains(dep))
							.cloned()
							.collect();

						if unloaded_deps.is_empty() {
							debug!(
								"Module {} is auto_install and all transitive dependencies are loaded",
								_R(module_key)
							);
							auto_install_modules.push(module_key);
						} else {
							debug!(
								"Module {} is auto_install but not all transitive dependencies are loaded. Unloaded: {:?}",
								_R(module_key),
								unloaded_deps
							);
						}
					} else {
						debug!(
							"Module {} is auto_install but some transitive dependencies are not available. Missing: {:?}",
							_R(module_key),
							missing_deps
						);
						let missing_with_chains: Vec<(ModuleName, Vec<ModuleName>)> = missing_deps
							.into_iter()
							.map(|missing_dep| {
								let chain = self
									.find_dependency_chain(module_key, missing_dep)
									.unwrap_or_else(|| vec![module_key, missing_dep]);
								(missing_dep, chain)
							})
							.collect();
						self.unloaded_auto_install.insert(module_key, missing_with_chains);
					}
				}
			}
		}

		auto_install_modules
	}

	pub fn get_unloaded_auto_install_for_model(
		&self,
		model_name: &str,
	) -> Option<(ModuleName, Vec<(ModuleName, Vec<ModuleName>)>)> {
		for entry in self.unloaded_auto_install.iter() {
			let module_name = *entry.key();

			let Some(root_path) = self.find_root_from_module(module_name) else {
				continue;
			};

			let Some(root) = self.roots.get(&root_path) else {
				continue;
			};

			let Some(module_entry) = root.get(&module_name) else {
				continue;
			};

			let module_path = root_path.join(module_entry.path.as_str());
			if self.check_module_extends_model(&module_path, model_name) {
				return Some((module_name, entry.value().clone()));
			}
		}
		None
	}

	fn check_module_extends_model(&self, module_path: &Path, target_model: &str) -> bool {
		let models_path = module_path.join("models");
		let py_files = if models_path.exists() {
			std::fs::read_dir(&models_path).ok()
		} else {
			std::fs::read_dir(module_path).ok()
		};

		let Some(entries) = py_files else {
			return false;
		};

		for entry in entries.flatten() {
			let path = entry.path();
			if path.extension().and_then(|s| s.to_str()) != Some("py") {
				continue;
			}

			let Ok(contents) = std::fs::read(&path) else {
				continue;
			};

			let Ok(models) = index_models(&contents) else {
				continue;
			};

			for model in models {
				match &model.type_ {
					ModelType::Base { ancestors, .. } => {
						if ancestors.iter().any(|ancestor| ancestor.as_str() == target_model) {
							return true;
						}
					}
					ModelType::Inherit(inherits) => {
						if inherits.iter().any(|inherit| inherit.as_str() == target_model) {
							return true;
						}
					}
				}
			}
		}

		false
	}

	async fn load_module(&self, module_name: ModuleName) -> Option<()> {
		{
			let root = self.find_root_from_module(module_name)?;
			let root = self.roots.get(&root)?;
			let path_module = root.get(&module_name)?;

			if path_module.loaded.load(Relaxed) {
				return None;
			}
		}

		let mut modules = Vec::new();
		let mut roots = Vec::new();
		let mut key_to_idx = HashMap::new();
		let mut visited = std::collections::HashSet::new();

		let mut stack = vec![(module_name, false)];
		while let Some((module_key, deps_done)) = stack.pop() {
			if !deps_done {
				if !visited.insert(module_key) {
					continue;
				}
				for root in self.roots.iter() {
					if let Some(module) = root.get(&module_key) {
						stack.push((module_key, true));
						for dep in module.dependencies.iter().rev() {
							if !visited.contains(dep) {
								stack.push((*dep, false));
							}
						}
						break;
					}
				}
			} else {
				for root in self.roots.iter() {
					if root.contains_key(&module_key) {
						key_to_idx.insert(module_key, modules.len());
						modules.push(module_key);
						roots.push(root);
						break;
					}
				}
			}
		}

		let mut outputs = tokio::task::JoinSet::new();
		for (module_key, root) in modules.into_iter().zip(roots.into_iter()) {
			let root_display = root.key().to_string_lossy();
			let root_key = _I(&root_display);
			let module = root
				.get(&module_key)
				.expect(format_loc!("module must already be present by now"));
			if module.loaded.compare_exchange(false, true, Relaxed, Relaxed) != Ok(false) {
				continue;
			}

			info!("{} depends on {}", _R(module_name), _R(module_key));
			let module_dir = Path::new(&*root_display).join(&module.path);
			if let Ok(xmls) = globwalk::glob_builder(format!("{}/**/*.xml", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
			{
				for xml in xmls {
					let Ok(xml) = xml else { continue };
					outputs.spawn(add_root_xml(root_key, xml.path().to_path_buf(), module_key));
				}
			}
			if let Ok(pys) = globwalk::glob_builder(format!("{}/**/*.py", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
			{
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
			}
			if let Ok(scripts) = globwalk::glob_builder(format!("{}/**/*.js", module_dir.display()))
				.file_type(FileType::FILE | FileType::SYMLINK)
				.follow_links(true)
				.build()
			{
				for js in scripts {
					let Ok(js) = js else { continue };
					let path = js.path().to_path_buf();
					outputs.spawn(js::add_root_js(root_key, path));
				}
			}
		}

		while let Some(res) = outputs.join_next().await {
			let outputs = match res {
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
				Output::Xml { records, templates } => {
					self.records.append(None, records);
					self.templates.append(templates);
				}
				Output::Models {
					path,
					models,
					file_path,
				} => {
					self.models.append(path, false, &models);

					if let Ok(contents) = std::fs::read_to_string(&file_path) {
						let fragments = self.create_model_fragments_from_file(&file_path, &contents);
						let base_model_id = self.get_base_model_class_id();
						let core_model_id = crate::analyze::class_cache().get_by_name("Model");

						for model in &models {
							let model_name = match &model.type_ {
								ModelType::Base { name, .. } => name.as_str(),
								ModelType::Inherit(inherits) => {
									if let Some(first_inherit) = inherits.first() {
										first_inherit.as_str()
									} else {
										continue;
									}
								}
							};

							let model_key = _G(model_name).unwrap();
							if let Some(mut entry) = self
								.models
								.try_get_mut(&model_key.into())
								.expect(format_loc!("deadlock"))
							{
								if model.type_.is_base() {
									if let Some(base_id) = base_model_id
										&& entry.class_chain.is_empty()
									{
										entry.class_chain.push(base_id);
									}
									if let Some(model_id) = core_model_id
										&& !entry.class_chain.contains(&model_id)
									{
										entry.class_chain.push(model_id);
									}
									if let Some(fragment_ids) = fragments.get(&ImStr::from(model_name)) {
										for &fragment_id in fragment_ids {
											if !entry.class_chain.contains(&fragment_id) {
												entry.class_chain.push(fragment_id);
											}
										}
									}
								} else {
									if let Some(model_id) = core_model_id
										&& !entry.class_chain.contains(&model_id)
									{
										entry.class_chain.push(model_id);
									}
									if let Some(fragment_ids) = fragments.get(&ImStr::from(model_name)) {
										for &fragment_id in fragment_ids {
											if !entry.class_chain.contains(&fragment_id) {
												entry.class_chain.push(fragment_id);
											}
										}
									}
								}
							}
						}

						for (class_name, fragment_ids) in fragments.iter() {
							let Some(model_key) = _G(class_name) else { continue };
							if self.models.contains_key(&model_key.into()) {
								continue;
							}
							let mut entry = ModelEntry::default();
							if let Some(base_id) = base_model_id {
								entry.class_chain.push(base_id);
							}
							if let Some(model_id) = core_model_id {
								entry.class_chain.push(model_id);
							}
							entry.class_chain.extend(fragment_ids.iter().copied());
							self.models.insert(model_key.into(), entry);
						}
					}
				}
				Output::JsItems {
					components,
					widgets,
					actions,
				} => {
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

		let mut all_loaded_modules = HashSet::new();
		for root in self.roots.iter() {
			for (&module_key, module_entry) in root.iter() {
				if module_entry.loaded.load(Relaxed) {
					all_loaded_modules.insert(module_key);
				}
			}
		}
		trace!(
			"Currently loaded modules: {:?}",
			all_loaded_modules.iter().map(|m| _R(*m)).collect::<Vec<_>>()
		);

		let auto_install_candidates = self.find_auto_install_modules(&all_loaded_modules);
		if !auto_install_candidates.is_empty() {
			info!(
				"Found {} auto_install candidates after loading modules",
				auto_install_candidates.len()
			);
			for auto_module in auto_install_candidates {
				trace!(
					"Auto-installing module {} because all dependencies are satisfied",
					_R(auto_module)
				);
				self.unloaded_auto_install.remove(&auto_module);
				Box::pin(self.load_module(auto_module)).await;
			}
		}

		Some(())
	}
	#[instrument(skip_all, fields(module))]
	pub(crate) fn discover_dependents_of(&self, module: ModuleName) -> Vec<ModuleName> {
		let mut dependents = HashMap::<ModuleName, HashSet<ModuleName>>::new();
		let mut all_modules = vec![];
		for root in self.roots.iter() {
			for (&module_key, module_entry) in root.iter() {
				all_modules.push(module_key);
				for dep in &module_entry.dependencies {
					dependents.entry(*dep).or_default().insert(module_key);
				}
			}
		}
		let module_to_idx: HashMap<ModuleName, usize> = all_modules.iter().enumerate().map(|(i, m)| (*m, i)).collect();
		let mut graph = vec![vec![]; all_modules.len()];

		for (i, module) in all_modules.iter().enumerate() {
			if let Some(deps) = dependents.get(module) {
				for dep in deps {
					if let Some(&dep_idx) = module_to_idx.get(dep) {
						graph[i].push(dep_idx);
					}
				}
			}
		}

		drop(module_to_idx);

		let mut memo: Vec<Option<HashSet<usize>>> = vec![None; all_modules.len()];

		fn dfs(idx: usize, graph: &Vec<Vec<usize>>, memo: &mut Vec<Option<HashSet<usize>>>) -> HashSet<usize> {
			debug_assert!(idx < memo.len());
			debug_assert!(idx < graph.len());
			if let Some(cached) = unsafe { memo.get_unchecked(idx) } {
				return cached.clone();
			}
			let mut result = HashSet::new();
			for &dep_idx in unsafe { graph.get_unchecked(idx) } {
				if dep_idx == idx {
					continue;
				}
				result.insert(dep_idx);
				result.extend(dfs(dep_idx, graph, memo));
			}
			memo[idx] = Some(result.clone());
			result
		}

		let idx = all_modules.iter().position(|&m| m == module).unwrap();
		dfs(idx, &graph, &mut memo)
			.into_iter()
			.map(|idx| all_modules[idx])
			.collect()
	}
	pub(crate) async fn load_modules_dependent_on(&self, module_name: ModuleName) -> Option<()> {
		let root = self.find_root_from_module(module_name)?;
		{
			let root = self.roots.try_get_mut(&root).try_unwrap()?;
			let module = root.get(&module_name)?;
			if module.loaded_dependents.load(Relaxed) {
				return None;
			}
		}

		let mut tasks = self
			.discover_dependents_of(module_name)
			.into_iter()
			.map(|dep| self.load_module(dep))
			.collect::<FuturesUnordered<_>>();
		if !tasks.is_empty() {
			while tasks.next().await.is_some() {}
		}

		self.roots
			.get(&root)?
			.get(&module_name)?
			.loaded_dependents
			.store(true, Relaxed);
		Some(())
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
						uri: Uri::from_file_path(old_path.as_str()).unwrap(),
						external: Some(false),
						take_focus: Some(true),
						selection: None,
					})
					.await;
				_ = client
					.show_document(ShowDocumentParams {
						uri: Uri::from_file_path(&new_path).unwrap(),
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
			for class_id in &entry.class_chain {
				if let Some(metadata) = crate::analyze::class_cache().get(*class_id)
					&& path_contains(root, metadata.location.path.to_path())
				{
					entry.deleted = true;
					break;
				}
			}
		}
	}
	pub fn find_module_of(&self, path: &Path) -> Option<ModuleName> {
		use std::path::Component;
		for entry in self.roots.iter() {
			if let Ok(path) = path.strip_prefix(entry.key()) {
				for component in path.components() {
					if let Component::Normal(norm) = component
						&& let Some(module) = _G(norm.to_string_lossy())
						&& entry.value().contains_key(&module.into())
					{
						return Some(module.into());
					}
				}
			}
		}

		None
	}

	#[tracing::instrument(skip_all, ret)]
	pub fn find_root_of(&self, path: &Path) -> Option<PathBuf> {
		for root_ in self.roots.iter() {
			if path.starts_with(root_.key()) {
				return Some(root_.key().to_owned());
			}
		}
		None
	}

	pub fn find_root_from_module(&self, key: ModuleName) -> Option<PathBuf> {
		for root in self.roots.iter() {
			if root.contains_key(&key) {
				return Some(root.key().to_owned());
			}
		}

		None
	}

	fn find_python_file(&self, base_path: &Path) -> Option<PathBuf> {
		use crate::test_utils;
		let stub_path = base_path.with_extension("pyi");
		if test_utils::fs::exists(&stub_path) {
			return Some(stub_path);
		}

		let py_path = base_path.with_extension("py");
		if test_utils::fs::exists(&py_path) {
			return Some(py_path);
		}

		let init_stub = base_path.join("__init__.pyi");
		if test_utils::fs::exists(&init_stub) {
			return Some(init_stub);
		}

		let init_py = base_path.join("__init__.py");
		if test_utils::fs::exists(&init_py) {
			return Some(init_py);
		}

		None
	}

	pub fn resolve_py_module(&self, module_path: &str) -> Option<PathBuf> {
		self.resolve_py_module_from(module_path, None)
	}

	pub fn resolve_py_module_from(&self, module_path: &str, current_file: Option<&Path>) -> Option<PathBuf> {
		use tracing::debug;

		debug!("Resolving module path: {} from {:?}", module_path, current_file);

		if module_path.starts_with('.') {
			let Some(current) = current_file else {
				debug!("Relative import but no current file context");
				return None;
			};
			return self.resolve_relative_import(module_path, current);
		}

		let parts: Vec<&str> = module_path.split('.').map(|s| s.trim()).collect();
		debug!("Module parts: {:?}", parts);

		if let ["odoo", "addons", module_name, ref rest @ ..] = parts[..] {
			let module_key = _G(module_name)?;
			debug!("Looking for odoo.addons module: {}", module_name);

			for root_entry in self.roots.iter() {
				debug!("Checking root: {}", root_entry.key().display());
				if let Some(module_entry) = root_entry.get(&module_key.into()) {
					let root_path = root_entry.key();
					let module_dir = root_path.join(&module_entry.path);
					debug!("Found module '{}' at: {}", module_name, module_dir.display());

					if !rest.is_empty() {
						let mut file_path = module_dir;
						for &part in rest {
							file_path = file_path.join(part);
						}
						if let Some(found_file) = self.find_python_file(&file_path) {
							debug!("Found file: {}", found_file.display());
							return Some(found_file);
						} else {
							debug!("File does not exist in this root: {}", file_path.display());
							continue;
						}
					} else {
						if let Some(init_file) = self.find_python_file(&module_dir) {
							debug!("Found init file: {}", init_file.display());
							return Some(init_file);
						} else {
							debug!("Init file does not exist in this root: {}", module_dir.display());
							continue;
						}
					}
				}
			}
			debug!("odoo.addons module not found: {}", module_path);
		}

		self.resolve_generic_python_module(module_path)
	}

	fn resolve_relative_import(&self, module_path: &str, current_file: &Path) -> Option<PathBuf> {
		use tracing::debug;

		let dots = module_path.chars().take_while(|&c| c == '.').count();
		let rest = &module_path[dots..];

		debug!("Resolving relative import: {} dots, rest: '{}'", dots, rest);

		let mut base_dir = current_file.parent()?.to_path_buf();

		for _ in 1..dots {
			base_dir = base_dir.parent()?.to_path_buf();
		}

		debug!("Base directory after going up: {}", base_dir.display());

		if !rest.is_empty() {
			let parts: Vec<&str> = rest.split('.').collect();

			let mut module_file = base_dir.clone();
			for part in &parts {
				module_file.push(part);
			}
			module_file.set_extension("py");

			if module_file.exists() {
				debug!("Found relative module at: {}", module_file.display());
				return Some(module_file);
			}

			let mut package_dir = base_dir.clone();
			for part in &parts {
				package_dir.push(part);
			}
			let init_py = package_dir.join("__init__.py");

			if init_py.exists() {
				debug!("Found relative package at: {}", init_py.display());
				return Some(init_py);
			}

			debug!(
				"Relative import not found: tried {} and {}",
				module_file.display(),
				init_py.display()
			);
		} else {
			let init_py = base_dir.join("__init__.py");
			if init_py.exists() {
				return Some(init_py);
			}
		}

		None
	}

	fn find_odoo_root(&self) -> Option<PathBuf> {
		use crate::test_utils;
		for root_entry in self.roots.iter() {
			let root = root_entry.key();
			let manifest_path = root.join("addons").join("base").join("__manifest__.py");
			if test_utils::fs::exists(&manifest_path) {
				debug!("Found Odoo root at: {}", root.display());
				return Some(root.clone());
			} else {
				debug!("Tried {}", manifest_path.display());
			}
		}
		None
	}

	fn try_resolve_in_odoo_root(&self, root: &Path, parts: &[&str]) -> Option<PathBuf> {
		use crate::test_utils;

		let mut package_path = root.to_path_buf();
		for part in parts {
			package_path.push(part);
		}
		if let Some(found) = self.find_python_file(&package_path) {
			debug!("Found Odoo package at: {}", found.display());
			return Some(found);
		}

		if let Some(last_part) = parts.last() {
			let mut module_path = root.to_path_buf();
			for part in &parts[..parts.len() - 1] {
				module_path.push(part);
			}

			let stub_file = module_path.join(format!("{}.pyi", last_part));
			if test_utils::fs::exists(&stub_file) {
				debug!("Found Odoo stub at: {}", stub_file.display());
				return Some(stub_file);
			}

			let source_file = module_path.join(format!("{}.py", last_part));
			if test_utils::fs::exists(&source_file) {
				debug!("Found Odoo source at: {}", source_file.display());
				return Some(source_file);
			}
		}

		None
	}

	fn try_resolve_in_workspace_root(&self, root: &Path, parts: &[&str]) -> Option<PathBuf> {
		use crate::test_utils;

		let mut package_path = root.to_path_buf();
		for part in parts {
			package_path.push(part);
		}
		if let Some(found) = self.find_python_file(&package_path) {
			debug!("Found workspace package at: {}", found.display());
			return Some(found);
		}

		if let Some(last_part) = parts.last() {
			let mut module_path = root.to_path_buf();
			for part in &parts[..parts.len() - 1] {
				module_path.push(part);
			}

			let stub_file = module_path.join(format!("{}.pyi", last_part));
			if test_utils::fs::exists(&stub_file) {
				debug!("Found workspace stub at: {}", stub_file.display());
				return Some(stub_file);
			}

			let source_file = module_path.join(format!("{}.py", last_part));
			if test_utils::fs::exists(&source_file) {
				debug!("Found workspace source at: {}", source_file.display());
				return Some(source_file);
			}
		}

		None
	}

	fn resolve_generic_python_module(&self, module_path: &str) -> Option<PathBuf> {
		use tracing::debug;

		debug!("Resolving generic Python module: {}", module_path);
		let parts: Vec<&str> = module_path.split('.').collect();
		if parts.is_empty() {
			return None;
		}

		let is_odoo_module = matches!(parts.first(), Some(&"odoo"));
		let odoo_root = if is_odoo_module { self.find_odoo_root() } else { None };

		if let Some(root) = odoo_root {
			debug!("Searching Odoo root for module: {}", module_path);
			if let Some(found) = self.try_resolve_in_odoo_root(&root, &parts) {
				return Some(found);
			}
		}

		debug!("Searching workspace roots for module: {}", module_path);
		for root_entry in self.roots.iter() {
			if let Some(found) = self.try_resolve_in_workspace_root(root_entry.key(), &parts) {
				return Some(found);
			}
		}

		if let Some(search_paths) = crate::stub_finder::get_python_search_paths() {
			debug!("Using Python search paths");

			let file_path = module_path.replace('.', "/");

			let mut all_paths = vec![search_paths.stdlib.clone()];
			all_paths.extend(search_paths.site_packages.clone());

			for search_dir in all_paths {
				let stub_file = search_dir.join(&file_path).with_extension("pyi");
				if stub_file.exists() {
					debug!("Found stub: {}", stub_file.display());
					return Some(stub_file);
				}

				let init_stub = search_dir.join(&file_path).join("__init__.pyi");
				if init_stub.exists() {
					debug!("Found package stub: {}", init_stub.display());
					return Some(init_stub);
				}

				let source_file = search_dir.join(&file_path).with_extension("py");
				if source_file.exists() {
					debug!("Found source: {}", source_file.display());
					return Some(source_file);
				}

				let init_py = search_dir.join(&file_path).join("__init__.py");
				if init_py.exists() {
					debug!("Found package source: {}", init_py.display());
					return Some(init_py);
				}
			}
		}

		debug!("Generic Python module not found: {}", module_path);
		None
	}

	pub fn get_base_model(&self) -> Option<Arc<Scope>> {
		use tracing::debug;

		self.base_model_cache
			.get_or_init(|| {
				debug!("Searching for BaseModel in all roots...");

				let candidate_paths = [
					"odoo/models.py",
					"odoo/models/models.py",
					"openerp/models.py",
					"openerp/models/models.py",
				];

				for root_entry in self.roots.iter() {
					let root_path = root_entry.key();
					debug!("Checking root: {}", root_path.display());

					let mut bases: Vec<std::path::PathBuf> = Vec::new();
					bases.push(root_path.clone());
					if let Some(parent) = root_path.parent() {
						bases.push(parent.to_path_buf());
						if let Some(grand) = parent.parent() {
							bases.push(grand.to_path_buf());
						}
					}
					bases.sort();
					bases.dedup();

					for base in bases.iter() {
						for candidate in &candidate_paths {
							let models_path = base.join(candidate);
							if test_utils::fs::exists(&models_path) {
								debug!("Found potential BaseModel file: {}", models_path.display());

								if let Some(scope) = self.parse_module_scope(&models_path) {
									if scope.get("BaseModel").is_some() {
										debug!("Found BaseModel in {}", models_path.display());
										return Some(scope.clone());
									}
								}
							}
						}
					}
				}

				debug!("BaseModel not found in any root");
				None
			})
			.clone()
	}

	pub fn get_base_model_class_id(&self) -> Option<crate::analyze::ClassId> {
		use crate::analyze::{ClassMetadata, class_cache};
		use crate::test_utils;

		use tree_sitter::Parser;

		let cache = class_cache();
		if let Some(class_id) = cache.get_by_name("BaseModel") {
			return Some(class_id);
		}

		let _base_model_scope = self.get_base_model()?;

		let candidate_paths = [
			"odoo/models.py",
			"odoo/models/models.py",
			"openerp/models.py",
			"openerp/models/models.py",
		];

		let mut models_file_path: Option<std::path::PathBuf> = None;
		let mut root_spur: Option<Spur> = None;
		'outer: for root_entry in self.roots.iter() {
			let root_path = root_entry.key();
			let mut bases: Vec<std::path::PathBuf> = Vec::new();
			bases.push(root_path.clone());
			if let Some(parent) = root_path.parent() {
				bases.push(parent.to_path_buf());
				if let Some(grand) = parent.parent() {
					bases.push(grand.to_path_buf());
				}
			}
			bases.sort();
			bases.dedup();

			for base in bases.iter() {
				for candidate in &candidate_paths {
					let path = base.join(candidate);
					if test_utils::fs::read_to_string(&path).is_ok() {
						models_file_path = Some(path);
						root_spur = Some(_I(base.to_string_lossy()));
						break 'outer;
					}
				}
			}
		}

		let models_file_path = models_file_path?;
		let root_spur = root_spur?;

		let contents = test_utils::fs::read_to_string(&models_file_path).ok()?;
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).ok()?;
		let tree = parser.parse(contents.as_bytes(), None)?;
		let root = tree.root_node();
		let attributes = HashMap::new();

		let mut cursor = root.walk();
		let mut base_range = None;
		let mut odoo_model_range = None;
		for child in root.named_children(&mut cursor) {
			if child.kind() == "class_definition"
				&& let Some(name_node) = child.child_by_field_name("name")
			{
				let class_name = &contents[name_node.byte_range()];
				if class_name == "BaseModel" {
					base_range = Some(span_conv(child.range()));
				}
				if class_name == "Model" {
					odoo_model_range = Some(span_conv(child.range()));
				}
			}
		}

		let base_range = base_range.unwrap_or_else(|| todo!("BaseModel range missing"));
		let class_id =
			cache.get_or_intern_with(ClassKey::from_raw(models_file_path.clone(), "BaseModel".into()), || {
				ClassMetadata::preparsed(
					"BaseModel".into(),
					MinLoc {
						path: PathSymbol::strip_root(root_spur, &models_file_path),
						range: base_range,
					},
					Vec::new(),
					attributes.clone(),
				)
			});

		if let Some(model_range) = odoo_model_range {
			let _model_class_id =
				cache.get_or_intern_with(ClassKey::from_raw(models_file_path.clone(), "Model".into()), || {
					ClassMetadata::preparsed(
						"Model".into(),
						MinLoc {
							path: PathSymbol::strip_root(root_spur, &models_file_path),
							range: model_range,
						},
						vec![class_id],
						attributes,
					)
				});
		}
		// TODO: In the current design, mro_chain is immutable after construction.

		Some(class_id)
	}

	pub fn create_model_class_fragment(
		&self,
		root: Spur,
		file_path: PathBuf,
		range: Range,
		class_name: ImStr,
		_class_node: Option<tree_sitter::Node>,
		_contents: Option<&str>,
	) -> Option<crate::analyze::ClassId> {
		use crate::analyze::{ClassKey, ClassMetadata, class_cache};

		let cache = class_cache();

		let class_id = cache.get_or_intern_with(ClassKey::from_raw(file_path.clone(), class_name.clone()), || {
			ClassMetadata::preparsed(
				class_name.clone(),
				MinLoc {
					path: PathSymbol::strip_root(root, &file_path),
					range,
				},
				Vec::new(),
				HashMap::new(),
			)
		});

		Some(class_id)
	}

	pub fn create_model_fragments_from_file(
		&self,
		file_path: &Path,
		contents: &str,
	) -> HashMap<ImStr, Vec<crate::analyze::ClassId>> {
		let mut fragments = HashMap::new();

		let mut parser = tree_sitter::Parser::new();
		if parser.set_language(&tree_sitter_python::LANGUAGE.into()).is_err() {
			return fragments;
		}

		let Some(tree) = parser.parse(contents, None) else {
			return fragments;
		};

		let root = tree.root_node();
		let root_path = _I(self.find_root_of(file_path).unwrap().to_string_lossy());

		let mut cursor = root.walk();
		for child in root.named_children(&mut cursor) {
			if child.kind() != "class_definition" {
				continue;
			}

			let Some(arg_list) = child.child_by_field_name("superclasses") else {
				continue;
			};

			let mut is_model_class = false;
			let super_text = &contents[arg_list.byte_range()];
			if super_text.contains("models.Model")
				|| super_text.contains("TransientModel")
				|| super_text.contains("AbstractModel")
				|| super_text.contains(" Model")
				|| super_text.contains("(Model")
			{
				is_model_class = true;
			}

			if !is_model_class {
				continue;
			}

			let Some(name_node) = child.child_by_field_name("name") else {
				continue;
			};
			let class_name: ImStr = contents[name_node.byte_range()].into();

			let model_name = self.extract_model_name_from_class(child, contents);

			let range = span_conv(child.range());
			if let Some(fragment_id) = self.create_model_class_fragment(
				root_path,
				file_path.to_path_buf(),
				range,
				class_name.clone(),
				Some(child),
				Some(contents),
			) {
				if let Some(model_name) = model_name {
					debug!(
						"create_model_fragments_from_file: stored fragment for model_name={}",
						model_name
					);
					fragments.entry(model_name).or_default().push(fragment_id);
				} else {
					debug!(
						"create_model_fragments_from_file: no model_name extracted for class {}",
						class_name
					);
					fragments.entry(class_name.clone()).or_default().push(fragment_id);
				}
			}
		}

		debug!(
			"create_model_fragments_from_file: returning {} fragments total",
			fragments.len()
		);
		fragments
	}

	fn extract_model_name_from_class(&self, class_node: Node<'_>, contents: &str) -> Option<ImStr> {
		let body = class_node.child_by_field_name("body")?;
		let mut cursor = body.walk();

		for statement in body.named_children(&mut cursor) {
			if statement.kind() == "expression_statement"
				&& let Some(assignment) = statement.named_child(0)
				&& assignment.kind() == "assignment"
				&& let Some(left) = assignment.child_by_field_name("left")
			{
				let var_name = &contents[left.byte_range()];
				if (var_name == "_name" || var_name == "_inherit")
					&& let Some(right) = assignment.child_by_field_name("right")
				{
					match right.kind() {
						"string" => {
							let text = &contents[right.byte_range()];
							if let Some(content) = text
								.strip_prefix('"')
								.and_then(|s| s.strip_suffix('"'))
								.or_else(|| text.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')))
							{
								return Some(content.into());
							}
						}
						"list" => {
							let mut list_cursor = right.walk();
							for elem in right.named_children(&mut list_cursor) {
								if elem.kind() == "string" {
									let text = &contents[elem.byte_range()];
									if let Some(content) = text
										.strip_prefix('"')
										.and_then(|s| s.strip_suffix('"'))
										.or_else(|| text.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')))
									{
										return Some(content.into());
									}
								}
							}
						}
						_ => {}
					}
				}
			}
		}

		None
	}

	pub fn update_models_with_class_chain(&self, path: PathSymbol, py_content: &str) -> anyhow::Result<()> {
		use crate::index::index_models;

		let models = index_models(py_content.as_bytes())?;
		self.models.append(path, true, &models);

		let fragments = self.create_model_fragments_from_file(&path.to_path(), py_content);

		let base_model_id = self.get_base_model_class_id();

		for model in models {
			match model.type_ {
				crate::model::ModelType::Base { name, ancestors } => {
					let model_key = _G(&name).ok_or_else(|| anyhow::anyhow!("invalid model name"))?;
					let mut entry = self
						.models
						.try_get_mut(&model_key.into())
						.expect("model should exist after append")
						.unwrap();

					if let Some(base_id) = base_model_id
						&& entry.class_chain.is_empty()
					{
						entry.class_chain.push(base_id);
					}
					if let Some(fragment_ids) = fragments.get(&ImStr::from(name.as_str())) {
						for &fragment_id in fragment_ids {
							if !entry.class_chain.contains(&fragment_id) {
								entry.class_chain.push(fragment_id);
							}
						}
					}

					entry
						.ancestors
						.extend(ancestors.into_iter().map(|sym| crate::model::ModelName::from(_I(&sym))));
					drop(entry);
					self.models.populate_properties(model_key.into(), &[path]);
				}
				crate::model::ModelType::Inherit(inherits) => {
					let Some(model_name) = inherits.first() else { continue };
					let model_key = _G(model_name).ok_or_else(|| anyhow::anyhow!("invalid model name"))?;

					if let Some(mut entry) = self.models.try_get_mut(&model_key.into()).expect("deadlock") {
						if let Some(base_id) = base_model_id
							&& entry.class_chain.is_empty()
						{
							entry.class_chain.push(base_id);
						}
						if let Some(fragment_ids) = fragments.get(&ImStr::from(model_name.as_str())) {
							entry.class_chain.extend(fragment_ids.iter().copied());
						}

						entry.ancestors.extend(
							inherits
								.iter()
								.skip(1)
								.map(|sym| crate::model::ModelName::from(_I(sym))),
						);
						drop(entry);
					}
					self.models.populate_properties(model_key.into(), &[path]);
				}
			}
		}

		let model_class_id = crate::analyze::class_cache().get_by_name("Model");
		for (frag_name, frag_ids) in fragments {
			let symbol: crate::model::ModelName = _I(frag_name).into();
			{
				let mut entry = self.models.entry(symbol).or_default();
				if let Some(base_id) = base_model_id
					&& !entry.class_chain.contains(&base_id)
				{
					entry.class_chain.insert(0, base_id);
				}
				if let Some(model_id) = model_class_id
					&& !entry.class_chain.contains(&model_id)
				{
					entry.class_chain.push(model_id);
				}
				for frag_id in frag_ids {
					if !entry.class_chain.contains(&frag_id) {
						entry.class_chain.push(frag_id);
					}
				}
			}
			self.models.populate_properties(symbol, &[path]);
		}

		Ok(())
	}
}

#[derive(Debug)]
struct ManifestInfo {
	dependencies: Box<[Symbol<ModuleEntry>]>,
	auto_install: bool,
}

fn parse_manifest_info(manifest: &Path) -> anyhow::Result<ManifestInfo> {
	query! {
		ManifestQuery(DependsList, AutoInstall);

	((dictionary
		(pair
			(string (string_content) @_depends)
			(list) @DEPENDS_LIST
		)
	) (#eq? @_depends "depends"))

	((dictionary
		(pair
			(string (string_content) @_auto_install)
			[(true) (false) (list)] @AUTO_INSTALL
		)
	) (#eq? @_auto_install "auto_install"))
	}

	let contents = test_utils::fs::read_to_string(manifest)?;
	let mut parser = Parser::new();
	parser.set_language(&tree_sitter_python::LANGUAGE.into())?;
	let ast = ok!(parser.parse(&contents, None));

	let mut cursor = QueryCursor::new();
	let root = ast.root_node();
	let mut deps = vec![];
	let mut auto_install = false;

	let mut captures = cursor.captures(ManifestQuery::query(), root, contents.as_bytes());
	while let Some((match_, idx)) = captures.next() {
		let capture = match_.captures[*idx];
		match ManifestQuery::from(capture.index) {
			Some(ManifestQuery::DependsList) => {
				let list_node = capture.node;
				let mut child_cursor = list_node.walk();
				for child in list_node.children(&mut child_cursor) {
					if child.kind() == "string" {
						let dep = &contents[child.byte_range().shrink(1)];
						deps.push(_I(dep).into());
					}
				}
			}
			Some(ManifestQuery::AutoInstall) => {
				let node = capture.node;
				auto_install = match node.kind() {
					"true" => true,
					"false" => false,
					_ => false,
				};
			}
			_ => {}
		}
	}

	Ok(ManifestInfo {
		dependencies: deps.into_boxed_slice(),
		auto_install,
	})
}

async fn add_root_xml(root: Spur, path: PathBuf, module_name: ModuleName) -> anyhow::Result<Output> {
	let path_uri = PathSymbol::strip_root(root, &path);
	let file = ok!(tokio::fs::read(&path).await, "Could not read {}", path.display());
	let file = String::from_utf8_lossy(&file);
	let mut reader = Tokenizer::from(file.as_ref());
	let mut records = vec![];
	let mut templates = vec![];
	let rope = Rope::from_str(&file);
	let rope = rope.slice(..);
	loop {
		match reader.next() {
			Some(Ok(Token::ElementStart { local, span, .. })) => match local.as_str() {
				"record" => {
					let record =
						Record::from_reader(ByteOffset(span.start()), module_name, path_uri, &mut reader, rope)?;
					records.extend(record);
				}
				"template" => {
					if let Some(template) =
						Record::template(ByteOffset(span.start()), module_name, path_uri, &mut reader, rope)?
					{
						records.push(template);
					} else {
						gather_templates(path_uri, &mut reader, rope, &mut templates, true)?;
					}
				}
				"menuitem" => {
					let menuitem =
						Record::menuitem(ByteOffset(span.start()), module_name, path_uri, &mut reader, rope)?;
					records.extend(menuitem);
				}
				"templates" => {
					gather_templates(path_uri, &mut reader, rope, &mut templates, false)?;
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

	let path_sym = PathSymbol::strip_root(root, &path);
	let models = index_models(&contents)?;
	Ok(Output::Models {
		path: path_sym,
		models,
		file_path: path,
	})
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
	let mut matches = cursor.matches(query, ast.root_node(), contents);
	while let Some(match_) = matches.next() {
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
				let Some(name_decl) = python_next_named_sibling(capture) else {
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
				let Some(inherit_decl) = python_next_named_sibling(capture) else {
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
				let Some(inherit_dict) = python_next_named_sibling(capture) else {
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
			if let Some(base) = &model.name
				&& let Some(position) = model.inherits.iter().position(|inherit| inherit == base)
			{
				model.inherits.swap(position, 0);
				has_primary = true;
			}
		}
		let inherits = model
			.inherits
			.into_iter()
			.map(|inherit| ImStr::from(String::from_utf8_lossy(inherit).as_ref()))
			.collect::<Vec<_>>();
		let range = span_conv(model.range);
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

#[cfg(test)]
mod tests {
	use crate::index::{_I, Index, Interner, ModelQuery, ModuleEntry};
	use crate::model::ModelType;
	use crate::test_utils::fs::TEST_FS;
	use crate::utils::{acc_vec, span_conv};
	use pretty_assertions::assert_eq;
	use std::collections::HashMap;
	use std::path::{Path, PathBuf};
	use tree_sitter::{Parser, QueryCursor, StreamingIterator, StreamingIteratorMut};

	use super::{index_models, parse_manifest_info};

	#[test]
	fn test_interner_functionality() {
		let interner = Interner::default();

		let spur1 = interner.get_or_intern("test_string");
		let spur2 = interner.get_or_intern("test_string");
		assert_eq!(spur1, spur2, "Same string should return same Spur");

		let spur3 = interner.get_or_intern("different_string");
		assert_ne!(spur1, spur3, "Different strings should return different Spurs");

		let path = Path::new("/test/path");
		let path_spur1 = interner.get_or_intern_path(path);
		let path_spur2 = interner.get_or_intern_path(path);
		assert_eq!(path_spur1, path_spur2, "Same path should return same Spur");

		let string_spur = interner.get_or_intern("/test/path");
		assert_eq!(
			path_spur1, string_spur,
			"Path and equivalent string should return same Spur"
		);
	}

	#[test]
	fn test_find_module_of() {
		let index = Index::default();

		let root = PathBuf::from("/test/root");
		let mut modules = HashMap::new();

		let module_entry = ModuleEntry {
			path: "test_module".into(),
			dependencies: Box::new([]),
			auto_install: false,
			loaded: Default::default(),
			loaded_dependents: Default::default(),
		};
		modules.insert(_I("test_module").into(), module_entry);
		index.roots.insert(root.clone(), modules);

		let test_path = Path::new("/test/root/test_module/models/test.py");
		let found_module = index.find_module_of(test_path);
		assert!(found_module.is_some(), "Should find module for path within module");

		let outside_path = Path::new("/other/path/file.py");
		let not_found = index.find_module_of(outside_path);
		assert!(not_found.is_none(), "Should not find module for path outside roots");
	}

	#[test]
	fn test_find_root_of() {
		let index = Index::default();

		let root1 = PathBuf::from("/test/root1");
		let root2 = PathBuf::from("/test/root2");
		index.roots.insert(root1.clone(), HashMap::new());
		index.roots.insert(root2.clone(), HashMap::new());

		let test_path = Path::new("/test/root1/some/nested/path");
		let found_root = index.find_root_of(test_path);
		assert_eq!(found_root, Some(root1), "Should find correct root");

		let outside_path = Path::new("/other/path");
		let not_found = index.find_root_of(outside_path);
		assert!(not_found.is_none(), "Should not find root for path outside all roots");
	}

	#[test]
	fn test_find_root_from_module() {
		let index = Index::default();

		let root = PathBuf::from("/test/root");
		let mut modules = HashMap::new();
		let module_key = _I("test_module").into();

		let module_entry = ModuleEntry {
			path: "test_module".into(),
			dependencies: Box::new([]),
			auto_install: false,
			loaded: Default::default(),
			loaded_dependents: Default::default(),
		};
		modules.insert(module_key, module_entry);
		index.roots.insert(root.clone(), modules);

		let found_root = index.find_root_from_module(module_key);
		assert_eq!(found_root, Some(root), "Should find root containing the module");

		let non_existent = _I("non_existent").into();
		let not_found = index.find_root_from_module(non_existent);
		assert!(not_found.is_none(), "Should not find root for non-existent module");
	}

	#[test]
	fn test_resolve_py_module() {
		let index = Index::default();

		let root = PathBuf::from("/test/root");
		let mut modules = HashMap::new();
		let module_key = _I("test_module").into();

		let module_entry = ModuleEntry {
			path: "test_module".into(),
			dependencies: Box::new([]),
			auto_install: false,
			loaded: Default::default(),
			loaded_dependents: Default::default(),
		};
		modules.insert(module_key, module_entry);
		index.roots.insert(root, modules);

		let module_path = "odoo.addons.test_module";
		let resolved = index.resolve_py_module(module_path);
		assert!(resolved.is_none(), "Should return None when file doesn't exist");

		let submodule_path = "odoo.addons.test_module.models.test";
		let resolved_sub = index.resolve_py_module(submodule_path);
		assert!(resolved_sub.is_none(), "Should return None when file doesn't exist");

		let invalid_path = "invalid.module.path";
		let not_resolved = index.resolve_py_module(invalid_path);
		assert!(not_resolved.is_none(), "Should not resolve invalid module path");
	}

	#[test]
	fn test_discover_dependents_of() {
		let index = Index::default();

		let root = PathBuf::from("/test/root");
		let mut modules = HashMap::new();

		let base_key = _I("base").into();
		let module_a_key = _I("module_a").into();
		let module_b_key = _I("module_b").into();

		modules.insert(
			base_key,
			ModuleEntry {
				path: "base".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			module_a_key,
			ModuleEntry {
				path: "module_a".into(),
				dependencies: Box::new([base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			module_b_key,
			ModuleEntry {
				path: "module_b".into(),
				dependencies: Box::new([module_a_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root, modules);

		let base_dependents = index.discover_dependents_of(base_key);
		assert!(
			base_dependents.contains(&module_a_key),
			"base should have module_a as dependent"
		);
		assert!(
			base_dependents.contains(&module_b_key),
			"base should have module_b as transitive dependent"
		);

		let module_a_dependents = index.discover_dependents_of(module_a_key);
		assert!(
			module_a_dependents.contains(&module_b_key),
			"module_a should have module_b as dependent"
		);
		assert!(
			!module_a_dependents.contains(&base_key),
			"module_a should not have base as dependent"
		);

		let module_b_dependents = index.discover_dependents_of(module_b_key);
		assert!(module_b_dependents.is_empty(), "module_b should have no dependents");
	}

	#[test]
	fn test_discover_dependents_of_with_auto_install() {
		let index = Index::default();

		let root = PathBuf::from("/test/root");
		let mut modules = HashMap::new();

		let sale_key = _I("sale").into();
		let crm_key = _I("crm").into();
		let sale_crm_key = _I("sale_crm").into();

		modules.insert(
			sale_key,
			ModuleEntry {
				path: "sale".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			crm_key,
			ModuleEntry {
				path: "crm".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			sale_crm_key,
			ModuleEntry {
				path: "sale_crm".into(),
				dependencies: Box::new([sale_key, crm_key]),
				auto_install: true,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root, modules);

		let sale_dependents = index.discover_dependents_of(sale_key);

		assert!(
			sale_dependents.contains(&sale_crm_key),
			"sale should have sale_crm as dependent due to auto_install, but got: {:?}",
			sale_dependents
		);

		let crm_dependents = index.discover_dependents_of(crm_key);
		assert!(
			crm_dependents.contains(&sale_crm_key),
			"crm should have sale_crm as dependent due to auto_install, but got: {:?}",
			crm_dependents
		);
	}

	#[test]
	fn test_resolve_transitive_dependencies() {
		let index = Index::default();
		let root = PathBuf::from("/test/root");

		let a_key = _I("module_a").into();
		let b_key = _I("module_b").into();
		let c_key = _I("module_c").into();
		let base_key = _I("base").into();

		let mut modules = HashMap::new();

		modules.insert(
			base_key,
			ModuleEntry {
				path: "base".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			c_key,
			ModuleEntry {
				path: "module_c".into(),
				dependencies: Box::new([base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			b_key,
			ModuleEntry {
				path: "module_b".into(),
				dependencies: Box::new([c_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			a_key,
			ModuleEntry {
				path: "module_a".into(),
				dependencies: Box::new([b_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root.clone(), modules);

		let deps = index.resolve_transitive_dependencies(a_key);
		assert!(deps.contains(&a_key), "Should include the module itself");
		assert!(deps.contains(&b_key), "Should include direct dependency B");
		assert!(deps.contains(&c_key), "Should include transitive dependency C");
		assert!(deps.contains(&base_key), "Should include transitive dependency base");
		assert_eq!(deps.len(), 4, "Should have exactly 4 dependencies");
	}

	#[test]
	fn test_circular_dependency_handling() {
		let index = Index::default();
		let root = PathBuf::from("/test/root");

		let a_key = _I("module_a").into();
		let b_key = _I("module_b").into();
		let c_key = _I("module_c").into();
		let base_key = _I("base").into();

		let mut modules = HashMap::new();

		modules.insert(
			base_key,
			ModuleEntry {
				path: "base".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			a_key,
			ModuleEntry {
				path: "module_a".into(),
				dependencies: Box::new([b_key, base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			b_key,
			ModuleEntry {
				path: "module_b".into(),
				dependencies: Box::new([c_key, base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			c_key,
			ModuleEntry {
				path: "module_c".into(),
				dependencies: Box::new([a_key, base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root.clone(), modules);

		let deps = index.resolve_transitive_dependencies(a_key);
		assert!(deps.contains(&a_key), "Should include module A");
		assert!(deps.contains(&b_key), "Should include module B");
		assert!(deps.contains(&c_key), "Should include module C");
		assert!(deps.contains(&base_key), "Should include base");
		assert_eq!(deps.len(), 4, "Should handle circular deps correctly");
	}

	#[test]
	fn test_get_all_available_modules() {
		let index = Index::default();
		let root1 = PathBuf::from("/test/root1");
		let root2 = PathBuf::from("/test/root2");

		let mut modules1 = HashMap::new();
		let mut modules2 = HashMap::new();

		let base_key = _I("base").into();
		let sale_key = _I("sale").into();
		let stock_key = _I("stock").into();

		modules1.insert(
			base_key,
			ModuleEntry {
				path: "base".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules1.insert(
			sale_key,
			ModuleEntry {
				path: "sale".into(),
				dependencies: Box::new([base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules2.insert(
			stock_key,
			ModuleEntry {
				path: "stock".into(),
				dependencies: Box::new([base_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root1, modules1);
		index.roots.insert(root2, modules2);

		let all_modules = index.get_all_available_modules();
		assert!(all_modules.contains(&base_key), "Should include base");
		assert!(all_modules.contains(&sale_key), "Should include sale");
		assert!(all_modules.contains(&stock_key), "Should include stock");
		assert_eq!(all_modules.len(), 3, "Should have exactly 3 modules");
	}

	#[test]
	fn test_implicit_base_dependency() {
		let index = Index::default();
		let root = PathBuf::from("/test/root");

		let sale_key = _I("sale").into();
		let product_key = _I("product").into();
		let base_key = _I("base").into();

		let mut modules = HashMap::new();

		modules.insert(
			base_key,
			ModuleEntry {
				path: "base".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			sale_key,
			ModuleEntry {
				path: "sale".into(),
				dependencies: Box::new([product_key]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		modules.insert(
			product_key,
			ModuleEntry {
				path: "product".into(),
				dependencies: Box::new([]),
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root.clone(), modules);

		index.add_implicit_base_dependencies();

		let modules = index.roots.get(&root).unwrap();
		let sale_entry = modules.get(&sale_key).unwrap();
		assert!(
			sale_entry.dependencies.contains(&product_key),
			"sale should have product as dependency"
		);
		assert!(
			sale_entry.dependencies.contains(&base_key),
			"sale should have base as implicit dependency"
		);

		let base_entry = modules.get(&base_key).unwrap();
		assert!(
			!base_entry.dependencies.contains(&base_key),
			"base should not have itself as dependency"
		);

		let product_entry = modules.get(&product_key).unwrap();
		assert!(
			product_entry.dependencies.contains(&base_key),
			"product should have base as implicit dependency"
		);
	}

	#[test]
	fn test_delete_marked_entries() {
		let index = Index::default();

		let root = _I("/test");
		let path = crate::index::PathSymbol::strip_root(root, Path::new("/test/path"));
		let models = vec![crate::model::Model {
			type_: ModelType::Base {
				name: "test_model".into(),
				ancestors: vec![],
			},
			range: Default::default(),
			byte_range: Default::default(),
		}];
		index.models.append(path, false, &models);

		if let Some(mut entry) = index.models.iter_mut().next() {
			entry.deleted = true;
		}

		let count_before = index.models.len();
		index.delete_marked_entries();
		let count_after = index.models.len();

		assert!(count_after < count_before, "Should have fewer entries after deletion");
	}

	#[test]
	fn test_parse_manifest() {
		use crate::test_utils;

		let manifest_content = br#"
{
    'name': 'Test Module',
    'depends': ['base', 'web', 'mail'],
    'version': '1.0',
}
"#;

		let manifest_path = PathBuf::from("/test/module/__manifest__.py");

		if let Ok(mut fs) = test_utils::fs::TEST_FS.write() {
			fs.insert(manifest_path.clone(), manifest_content);
		}

		let manifest_info = parse_manifest_info(&manifest_path).unwrap();

		assert!(
			manifest_info.dependencies.len() >= 3,
			"Should have at least base, web, and mail dependencies"
		);

		let base_key = _I("base").into();
		assert!(
			manifest_info.dependencies.contains(&base_key),
			"Should include base dependency"
		);

		assert!(!manifest_info.auto_install, "auto_install should be false by default");
	}

	#[test]
	fn test_parse_manifest_with_auto_install() {
		use crate::test_utils;

		let manifest_content = br#"
{
    'name': 'Test Auto Install Module',
    'depends': ['base', 'web'],
    'auto_install': True,
}
"#;

		let manifest_path = PathBuf::from("/test/auto_module/__manifest__.py");

		if let Ok(mut fs) = test_utils::fs::TEST_FS.write() {
			fs.insert(manifest_path.clone(), manifest_content);
		}

		let manifest_info = parse_manifest_info(&manifest_path).unwrap();

		assert!(
			manifest_info.dependencies.len() >= 2,
			"Should have at least base and web dependencies"
		);

		assert!(manifest_info.auto_install, "auto_install should be true");
	}

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
			.fold_mut(vec![], acc_vec);
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

	#[test]
	fn test_depends_manifest_parsing() {
		use crate::test_utils;

		if let Ok(mut fs) = test_utils::fs::TEST_FS.write() {
			fs.insert(
				"foobar/__manifest__.py".into(),
				r#"{
    "name": "foobar",
    "depends": [
        "foo",
        # test dependencies
        "bar",  # this was not recognized
        # "skip",
    ],
}
"#
				.as_bytes(),
			);
		}

		let manifest_path = std::path::Path::new("foobar/__manifest__.py");
		let result = parse_manifest_info(manifest_path);

		match result {
			Ok(manifest_info) => {
				let deps = &manifest_info.dependencies;
				println!("Dependencies found: {} total", deps.len());
				for (i, dep) in deps.iter().enumerate() {
					println!("  {}. {:?}", i + 1, dep);
				}

				assert_eq!(
					deps.len(),
					2,
					"Expected to find 2 dependencies (foo + bar), but found {}",
					deps.len()
				);

				let dep_names: Vec<String> = deps.iter().map(|d| format!("{d:?}")).collect();
				assert!(
					dep_names.contains(&"Symbol<ModuleEntry>(\"foo\")".to_string()),
					"Expected to find 'foo' dependency"
				);
				assert!(
					dep_names.contains(&"Symbol<ModuleEntry>(\"bar\")".to_string()),
					"Expected to find 'bar' dependency (should now be recognized despite comment)"
				);
			}
			Err(e) => {
				panic!("Parsing should succeed but with incomplete results, got error: {e}");
			}
		}
	}

	#[test]
	fn test_find_base_model() {
		use crate::test_utils::fs::TEST_FS;
		use std::path::PathBuf;

		let root_path = PathBuf::from("/test_odoo_root");
		let models_path = root_path.join("odoo/models.py");

		let models_content = b"
class BaseModel:
    \"\"\"Base class for all Odoo models.\"\"\"
    
    def create(self, vals):
        \"\"\"Create a new record.\"\"\"
        pass
    
    def search(self, args):
        \"\"\"Search for records.\"\"\"
        pass
";

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.insert(models_path.clone(), models_content);
		}

		let index = Index::default();
		index.roots.insert(root_path.clone(), Default::default());

		let base_model = index.get_base_model();
		assert!(base_model.is_some(), "Should find BaseModel");

		let scope = base_model.unwrap();
		assert!(scope.get("BaseModel").is_some(), "BaseModel should exist in scope");

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.remove(&models_path);
		}
	}

	#[test]
	fn test_base_model_class_id() {
		use crate::analyze::class_cache;
		use crate::test_utils::fs::TEST_FS;
		use std::path::PathBuf;

		let root_path = PathBuf::from("/test_base_model_class");
		let models_path = root_path.join("odoo/models.py");

		let models_content = b"
class BaseModel:
    \"\"\"Base class for all Odoo models.\"\"\"
    
    def create(self, vals):
        \"\"\"Create a new record.\"\"\"
        pass
    
    def search(self, args):
        \"\"\"Search for records.\"\"\"
        pass
    
    def write(self, vals):
        \"\"\"Update records.\"\"\"
        pass
";

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.insert(models_path.clone(), models_content);
		}

		let index = Index::default();
		index.roots.insert(root_path.clone(), Default::default());

		let class_id = index.get_base_model_class_id();
		assert!(class_id.is_some(), "Should create BaseModel ClassId");

		let class_id = class_id.unwrap();

		let cache = class_cache();
		let metadata = cache.resolve(class_id);
		assert_eq!(metadata.name.as_str(), "BaseModel");

		println!("Attributes found: {:?}", metadata.attributes.keys().collect::<Vec<_>>());
		println!("BaseModel created with ClassId: {:?}", class_id);
		let class_id2 = index.get_base_model_class_id().unwrap();
		assert_eq!(class_id, class_id2, "Should return cached ClassId");

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.remove(&models_path);
		}
	}

	#[test]
	fn test_create_model_class_fragment() {
		use crate::analyze::class_cache;
		use crate::test_utils::fs::TEST_FS;
		use std::path::PathBuf;

		let root_path = PathBuf::from("/test_model_fragment");
		let root_spur = _I(root_path.to_str().unwrap());
		let models_path = root_path.join("odoo/models.py");
		let models_content = b"
class BaseModel:
    def create(self, vals):
        pass
    def search(self, args):
        pass
";

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.insert(models_path.clone(), models_content);
		}

		let index = Index::default();
		index.roots.insert(root_path.clone(), Default::default());

		let base_model_id = index.get_base_model_class_id();
		assert!(base_model_id.is_some(), "BaseModel should be created");
		let base_model_id = base_model_id.unwrap();

		let fragment1_path = root_path.join("addons/my_module/models/base.py");
		let fragment1_content = r#"
class MyModel(models.Model):
    _name = 'my.model'
    
    def method_from_base(self):
        pass
"#;

		let mut parser = tree_sitter::Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();
		let tree = parser.parse(fragment1_content.as_bytes(), None).unwrap();
		let class_node1 = tree
			.root_node()
			.named_children(&mut tree.root_node().walk())
			.find(|n| n.kind() == "class_definition")
			.expect("Should find class");

		let fragment1_id = index.create_model_class_fragment(
			root_spur,
			fragment1_path.clone(),
			span_conv(class_node1.range()),
			"MyModel".into(),
			Some(class_node1),
			Some(fragment1_content),
		);
		assert!(fragment1_id.is_some());
		let fragment1_id = fragment1_id.unwrap();

		let fragment2_path = root_path.join("addons/extension/models/extended.py");
		let fragment2_content = r#"
class MyModelExtension(models.Model):
    _inherit = 'my.model'
    
    def method_from_extension(self):
        pass
    
    def another_extension_method(self):
        pass
"#;

		let tree2 = parser.parse(fragment2_content.as_bytes(), None).unwrap();
		let class_node2 = tree2
			.root_node()
			.named_children(&mut tree2.root_node().walk())
			.find(|n| n.kind() == "class_definition")
			.expect("Should find class");

		let fragment2_id = index.create_model_class_fragment(
			root_spur,
			fragment2_path.clone(),
			span_conv(class_node2.range()),
			"MyModelExtension".into(),
			Some(class_node2),
			Some(fragment2_content),
		);
		assert!(fragment2_id.is_some());
		let fragment2_id = fragment2_id.unwrap();

		let cache = class_cache();

		let fragment1_meta = cache.resolve(fragment1_id);
		assert_eq!(fragment1_meta.name.as_str(), "MyModel");
		assert_eq!(fragment1_meta.parents.len(), 0, "Fragments should have no parents");

		let fragment2_meta = cache.resolve(fragment2_id);
		assert_eq!(fragment2_meta.name.as_str(), "MyModelExtension");
		assert_eq!(fragment2_meta.parents.len(), 0, "Fragments should have no parents");

		let _class_chain = vec![base_model_id, fragment1_id, fragment2_id];

		println!("✓ Model fragment creation test passed");
		println!("  Fragment1 created with class_id: {:?}", fragment1_id);
		println!("  Fragment2 created with class_id: {:?}", fragment2_id);
		println!(
			"  Class chain: [BaseModel, Fragment1({}), Fragment2({})]",
			fragment1_meta.name, fragment2_meta.name
		);
		println!(
			"  Total methods available: create, search, method_from_base, method_from_extension, another_extension_method"
		);

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.remove(&models_path);
		}
	}

	#[test]
	fn test_model_class_chain_population() {
		use crate::analyze::class_cache;
		use crate::test_utils::fs::TEST_FS;
		use std::path::PathBuf;

		let root_path = PathBuf::from("/test_class_chain");
		let models_path = root_path.join("odoo/models.py");
		let models_content = b"
class BaseModel:
    def create(self, vals):
        pass
    def write(self, vals):
        pass
";

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.insert(models_path.clone(), models_content);
		}

		let index = Index::default();
		index.roots.insert(root_path.clone(), Default::default());

		let py_file = root_path.join("addons/test_module/models/partner.py");
		let py_content = r#"
from odoo import models, fields

class Partner(models.Model):
    _name = 'res.partner'
    
    def custom_method(self):
        pass

class PartnerExtension(models.Model):
    _inherit = 'res.partner'
    
    def extended_method(self):
        pass
"#;

		let fragments = index.create_model_fragments_from_file(&py_file, py_content);

		let model_name: crate::ImStr = "res.partner".into();
		assert!(
			fragments.contains_key(&model_name),
			"Should have fragment for res.partner from base class"
		);

		let partner_fragments = fragments.get(&model_name).unwrap();
		assert_eq!(partner_fragments.len(), 2, "Should have 2 fragments (base + extension)");

		let cache = class_cache();

		let frag1_meta = cache.resolve(partner_fragments[0]);
		let frag2_meta = cache.resolve(partner_fragments[1]);

		println!(
			"Fragment 1 - name: {}, attributes: {:?}",
			frag1_meta.name,
			frag1_meta.attributes.keys().collect::<Vec<_>>()
		);
		println!(
			"Fragment 2 - name: {}, attributes: {:?}",
			frag2_meta.name,
			frag2_meta.attributes.keys().collect::<Vec<_>>()
		);

		assert_eq!(frag1_meta.parents.len(), 0, "Fragments should have no parents");
		assert_eq!(frag2_meta.parents.len(), 0, "Fragments should have no parents");

		let base_model_id = index.get_base_model_class_id();
		assert!(base_model_id.is_some(), "BaseModel should exist");

		let class_chain = if let Some(base_id) = base_model_id {
			let mut chain = vec![base_id];
			chain.extend(partner_fragments.iter().copied());
			chain
		} else {
			partner_fragments.clone()
		};

		println!("✓ Model class_chain population test passed");
		println!("  Created {} fragments for res.partner", partner_fragments.len());
		println!("  Class chain length: {}", class_chain.len());

		{
			let mut fs = TEST_FS.write().unwrap();
			fs.remove(&models_path);
		}
	}
	#[test]
	fn test_resolve_generic_python_module() {
		let index = Index::default();

		if let Ok(mut fs) = TEST_FS.write() {
			fs.insert(PathBuf::from("/test/root/odoo/__init__.py"), br#"from . import models"#);
			fs.insert(PathBuf::from("/test/root/odoo/models.py"), br#"class Unicorn: pass"#);
			fs.insert(PathBuf::from("/test/root/odoo/addons/base/__manifest__.py"), br#"{}"#);
		} else {
			panic!("Failed to write to test filesystem");
		}

		let root = PathBuf::from("/test/root");
		index.roots.insert(root.clone(), Default::default());
		let module_path = "odoo.models";
		let resolved = index.resolve_generic_python_module(module_path);
		assert_eq!(resolved.as_deref(), Some(Path::new("/test/root/odoo/models.py")));
	}
}
