//! The main indexer for all language items, including [`Record`]s, QWeb [`Template`]s, and Owl [`Component`]s

use mini_moka::sync::Cache;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::sync::atomic::Ordering::Relaxed;

use dashmap::DashMap;
use derive_more::Deref;
use globwalk::FileType;
use ignore::Match;
use ignore::gitignore::Gitignore;
use lasso::ThreadedRodeo;
use smart_default::SmartDefault;
use tower_lsp_server::{Client, lsp_types::*};
use xmlparser::{Token, Tokenizer};

use crate::prelude::*;

pub use crate::component::{Component, ComponentName};
use crate::model::{Model, ModelIndex, ModelLocation, ModelType};
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
	/// root -> module key -> module's relpath to root
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
	/// Tracks auto_install modules that couldn't be loaded due to missing dependencies
	/// Maps module_name -> list of missing dependencies with their dependency chains
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub unloaded_auto_install: DashMap<ModuleName, Vec<(ModuleName, Vec<ModuleName>)>>,
	/// Cache for transitive dependencies to avoid recalculation
	#[default(_code = "DashMap::with_shard_amount(4)")]
	pub(crate) transitive_deps_cache: DashMap<ModuleName, HashSet<ModuleName>>,
	#[default(_code = "Cache::new(16)")]
	pub(crate) ast_cache: Cache<PathBuf, Arc<AstCacheItem>>,
}

pub struct AstCacheItem {
	pub tree: tree_sitter::Tree,
	pub rope: Rope,
}

pub type ModuleName = Symbol<ModuleEntry>;

enum Output {
	// Nothing,
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

		// Add implicit base dependencies to all modules
		self.add_implicit_base_dependencies();

		// Clear transitive dependencies cache since modules have been added/modified
		self.transitive_deps_cache.clear();

		// After adding all modules in the root, check for auto_install modules with unsatisfied dependencies		// We pass an empty set because no modules are loaded yet, this will identify all auto_install
		// modules and track those with missing dependencies
		debug!("Checking for auto_install modules after adding root");
		let auto_install_check = self.find_auto_install_modules(&HashSet::new());
		debug!("Found {} auto_install modules to check", auto_install_check.len());

		Ok(())
	}
	#[instrument(skip_all, ret, fields(path = path.display().to_string()))]
	pub(crate) async fn load_modules_for_document(&self, document_blocker: Arc<Semaphore>, path: &Path) -> Option<()> {
		let _blocker = document_blocker.block();
		let module_name = self.find_module_of(path)?;

		self.load_module(module_name).await
	}
	/// Resolve all transitive dependencies for a module
	pub fn resolve_transitive_dependencies(&self, module: ModuleName) -> HashSet<ModuleName> {
		// Check cache first
		if let Some(cached) = self.transitive_deps_cache.get(&module) {
			return cached.clone();
		}

		let mut dependencies = HashSet::new();
		let mut stack = vec![module];

		while let Some(current) = stack.pop() {
			// If we've already processed this module, skip it (handles circular deps)
			if !dependencies.insert(current) {
				continue;
			}

			// Find the module in any root
			for root in self.roots.iter() {
				if let Some(module_entry) = root.get(&current) {
					// Add all direct dependencies to the stack
					for &dep in module_entry.dependencies.iter() {
						if !dependencies.contains(&dep) {
							stack.push(dep);
						}
					}
					break;
				}
			}
		}

		// Cache the result
		self.transitive_deps_cache.insert(module, dependencies.clone());

		dependencies
	}

	/// Find the dependency chain from source module to target module
	/// Returns None if no path exists, or Some(path) where path includes both source and target
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

			// Find the module in any root
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
	/// Get all modules that are physically present in the filesystem
	pub fn get_all_available_modules(&self) -> HashSet<ModuleName> {
		let mut modules = HashSet::new();

		for root in self.roots.iter() {
			for (&module_key, _) in root.iter() {
				modules.insert(module_key);
			}
		}

		modules
	}

	/// Add implicit base dependency to all modules except base itself
	fn add_implicit_base_dependencies(&self) {
		let base_key: ModuleName = _I("base").into();

		for mut root in self.roots.iter_mut() {
			let modules = root.value_mut();

			// Only add implicit base dependencies if base module exists
			if !modules.contains_key(&base_key) {
				continue;
			}

			// Collect modules that need base dependency
			let mut updates = Vec::new();
			for (&module_key, module_entry) in modules.iter() {
				if module_key != base_key && !module_entry.dependencies.contains(&base_key) {
					let mut deps = module_entry.dependencies.to_vec();
					deps.push(base_key);
					updates.push((module_key, deps));
				}
			}

			// Apply updates
			for (module_key, new_deps) in updates {
				if let Some(entry) = modules.get_mut(&module_key) {
					entry.dependencies = new_deps.into_boxed_slice();
				}
			}
		}
	}

	/// Find all auto_install modules whose dependencies are satisfied by the given set of modules
	fn find_auto_install_modules(&self, loaded_modules: &HashSet<ModuleName>) -> Vec<ModuleName> {
		let mut auto_install_modules = Vec::new();

		// Get all available modules (not just loaded ones)
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
					// Get all transitive dependencies for this module
					let transitive_deps = self.resolve_transitive_dependencies(module_key);

					// Check if all transitive dependencies are available (not necessarily loaded)
					let missing_deps: Vec<ModuleName> = transitive_deps
						.iter()
						.filter(|dep| **dep != module_key && !all_available_modules.contains(dep))
						.cloned()
						.collect();

					if missing_deps.is_empty() {
						// Now check if all transitive dependencies are loaded
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
						// Track this module as unloaded with its missing dependencies and their chains
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

	/// Check if there are any unloaded auto_install modules that might extend a model
	/// Returns the first unloaded auto_install module with its missing dependencies and chains
	pub fn get_unloaded_auto_install_for_model(
		&self,
		model_name: &str,
	) -> Option<(ModuleName, Vec<(ModuleName, Vec<ModuleName>)>)> {
		// Check each unloaded auto_install module to see if it extends the given model
		for entry in self.unloaded_auto_install.iter() {
			let module_name = *entry.key();

			// Find the module's root and path
			let Some(root_path) = self.find_root_from_module(module_name) else {
				continue;
			};

			let Some(root) = self.roots.get(&root_path) else {
				continue;
			};

			let Some(module_entry) = root.get(&module_name) else {
				continue;
			};

			// Check if this module extends the given model by scanning its Python files
			let module_path = root_path.join(module_entry.path.as_str());
			if self.check_module_extends_model(&module_path, model_name) {
				return Some((module_name, entry.value().clone()));
			}
		}
		None
	}

	/// Check if a module extends a specific model by scanning its Python files
	fn check_module_extends_model(&self, module_path: &Path, target_model: &str) -> bool {
		// Look for Python files in the module
		let models_path = module_path.join("models");
		let py_files = if models_path.exists() {
			// Check models directory
			std::fs::read_dir(&models_path).ok()
		} else {
			// Check module root
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

			// Read and parse the Python file
			let Ok(contents) = std::fs::read(&path) else {
				continue;
			};

			// Parse models from the file
			let Ok(models) = index_models(&contents) else {
				continue;
			};

			// Check if any model inherits from the target model
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
				// Find the module and push dependencies first
				for root in self.roots.iter() {
					if let Some(module) = root.get(&module_key) {
						stack.push((module_key, true)); // revisit after deps
						for dep in module.dependencies.iter().rev() {
							if !visited.contains(dep) {
								stack.push((*dep, false));
							}
						}
						break;
					}
				}
			} else {
				// All dependencies processed, add to result
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
			info!("{} depends on {}", _R(module_name), _R(module_key));
			let root_display = root.key().to_string_lossy();
			let root_key = _I(&root_display);
			let module = root
				.get(&module_key)
				.expect(format_loc!("module must already be present by now"));
			let module_dir = Path::new(&*root_display).join(&module.path);
			if module.loaded.compare_exchange(false, true, Relaxed, Relaxed) != Ok(false) {
				continue;
			}

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
				Output::Models { path, models } => {
					self.models.append(path, false, &models);
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

		// After loading all requested modules, check for auto_install modules
		let mut all_loaded_modules = HashSet::new();
		for root in self.roots.iter() {
			for (&module_key, module_entry) in root.iter() {
				if module_entry.loaded.load(Relaxed) {
					all_loaded_modules.insert(module_key);
				}
			}
		}
		debug!(
			"Currently loaded modules: {:?}",
			all_loaded_modules.iter().map(|m| _R(*m)).collect::<Vec<_>>()
		);

		// Find and load auto_install modules
		let auto_install_candidates = self.find_auto_install_modules(&all_loaded_modules);
		if !auto_install_candidates.is_empty() {
			info!(
				"Found {} auto_install candidates after loading modules",
				auto_install_candidates.len()
			);
			for auto_module in auto_install_candidates {
				info!(
					"Auto-installing module {} because all dependencies are satisfied",
					_R(auto_module)
				);
				// Remove from unloaded_auto_install since it's now being loaded
				self.unloaded_auto_install.remove(&auto_module);
				// Recursively load auto_install modules
				Box::pin(self.load_module(auto_module)).await;
			}
		}

		Some(())
	}
	#[instrument(skip_all, fields(module))]
	pub(crate) fn discover_dependents_of(&self, module: ModuleName) -> Vec<ModuleName> {
		let mut dependents = HashMap::<ModuleName, HashSet<ModuleName>>::new();
		let mut all_modules = vec![];
		// First, collect all modules and their direct dependencies
		for root in self.roots.iter() {
			for (&module_key, module_entry) in root.iter() {
				all_modules.push(module_key);
				for dep in &module_entry.dependencies {
					dependents.entry(*dep).or_default().insert(module_key);
				}
			}
		}
		// Compute transitive dependents for each module
		let module_to_idx: HashMap<ModuleName, usize> = all_modules.iter().enumerate().map(|(i, m)| (*m, i)).collect();
		let mut graph = vec![vec![]; all_modules.len()];

		// Build the graph: for each module, add edges to its direct dependents
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

		// DP/memoization for transitive dependents
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
	/// Inverse of [Index::load_module], useful for requests that work in the context of the larger codebase.
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
			if let Some(ModelLocation(ref loc, _)) = entry.base
				&& path_contains(root, loc.path.to_path())
			{
				entry.deleted = true;
			}
		}
	}
	/// Has complexity of `O(len(self.roots))`
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

	/// Resolves a Python module path like "odoo.addons.some_module.controllers.main" to its file path
	pub fn resolve_py_module(&self, module_path: &str) -> Option<PathBuf> {
		use tracing::debug;

		debug!("Resolving module path: {}", module_path);
		let parts: Vec<&str> = module_path.split('.').map(|s| s.trim()).collect();
		debug!("Module parts: {:?}", parts);

		// Handle odoo.addons.module_name.* pattern
		if parts.len() >= 3 && parts[0] == "odoo" && parts[1] == "addons" {
			let module_name = parts[2];
			let module_key = _G(module_name)?;
			debug!("Looking for module: {}", module_name);

			// Find the root and module entry
			for root_entry in self.roots.iter() {
				debug!("Checking root: {}", root_entry.key().display());
				if let Some(module_entry) = root_entry.get(&module_key.into()) {
					let root_path = root_entry.key();
					let module_dir = root_path.join(&module_entry.path);
					debug!("Found module at: {}", module_dir.display());

					// Build the file path from remaining parts
					if parts.len() > 3 {
						let mut file_path = module_dir;
						for part in &parts[3..] {
							file_path = file_path.join(part);
						}
						file_path.set_extension("py");
						debug!("Checking file path: {}", file_path.display());

						if file_path.exists() {
							debug!("Found file: {}", file_path.display());
							return Some(file_path);
						} else {
							debug!("File does not exist: {}", file_path.display());
						}
					} else {
						// Just the module itself - look for __init__.py
						let init_path = module_dir.join("__init__.py");
						debug!("Checking init path: {}", init_path.display());
						if init_path.exists() {
							debug!("Found init file: {}", init_path.display());
							return Some(init_path);
						}
					}
				}
			}
		}

		debug!("Module resolution failed for: {}", module_path);
		None
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
	use crate::index::{_I, Index, Interner, ModelQuery, ModuleEntry};
	use crate::model::ModelType;
	use crate::utils::acc_vec;
	use pretty_assertions::assert_eq;
	use std::collections::HashMap;
	use std::path::{Path, PathBuf};
	use tree_sitter::{Parser, QueryCursor, StreamingIterator, StreamingIteratorMut};

	use super::{index_models, parse_manifest_info};

	#[test]
	fn test_interner_functionality() {
		let interner = Interner::default();

		// Test get_or_intern with strings
		let spur1 = interner.get_or_intern("test_string");
		let spur2 = interner.get_or_intern("test_string");
		assert_eq!(spur1, spur2, "Same string should return same Spur");

		let spur3 = interner.get_or_intern("different_string");
		assert_ne!(spur1, spur3, "Different strings should return different Spurs");

		// Test get_or_intern_path
		let path = Path::new("/test/path");
		let path_spur1 = interner.get_or_intern_path(path);
		let path_spur2 = interner.get_or_intern_path(path);
		assert_eq!(path_spur1, path_spur2, "Same path should return same Spur");

		// Test that path and string with same content return same Spur
		let string_spur = interner.get_or_intern("/test/path");
		assert_eq!(
			path_spur1, string_spur,
			"Path and equivalent string should return same Spur"
		);
	}

	#[test]
	fn test_find_module_of() {
		let index = Index::default();

		// Setup test data
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

		// Test finding module
		let test_path = Path::new("/test/root/test_module/models/test.py");
		let found_module = index.find_module_of(test_path);
		assert!(found_module.is_some(), "Should find module for path within module");

		// Test path outside any module
		let outside_path = Path::new("/other/path/file.py");
		let not_found = index.find_module_of(outside_path);
		assert!(not_found.is_none(), "Should not find module for path outside roots");
	}

	#[test]
	fn test_find_root_of() {
		let index = Index::default();

		// Setup test data
		let root1 = PathBuf::from("/test/root1");
		let root2 = PathBuf::from("/test/root2");
		index.roots.insert(root1.clone(), HashMap::new());
		index.roots.insert(root2.clone(), HashMap::new());

		// Test finding root
		let test_path = Path::new("/test/root1/some/nested/path");
		let found_root = index.find_root_of(test_path);
		assert_eq!(found_root, Some(root1), "Should find correct root");

		// Test path not under any root
		let outside_path = Path::new("/other/path");
		let not_found = index.find_root_of(outside_path);
		assert!(not_found.is_none(), "Should not find root for path outside all roots");
	}

	#[test]
	fn test_find_root_from_module() {
		let index = Index::default();

		// Setup test data
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

		// Test finding root from module
		let found_root = index.find_root_from_module(module_key);
		assert_eq!(found_root, Some(root), "Should find root containing the module");

		// Test non-existent module
		let non_existent = _I("non_existent").into();
		let not_found = index.find_root_from_module(non_existent);
		assert!(not_found.is_none(), "Should not find root for non-existent module");
	}

	#[test]
	fn test_resolve_py_module() {
		let index = Index::default();

		// Setup test data
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

		// Test resolving odoo.addons.module_name pattern
		let module_path = "odoo.addons.test_module";
		let resolved = index.resolve_py_module(module_path);
		// Note: This will return None in test because the file doesn't actually exist
		// In a real scenario with actual files, this would resolve to the __init__.py path
		assert!(resolved.is_none(), "Should return None when file doesn't exist");

		// Test resolving with submodule
		let submodule_path = "odoo.addons.test_module.models.test";
		let resolved_sub = index.resolve_py_module(submodule_path);
		assert!(resolved_sub.is_none(), "Should return None when file doesn't exist");

		// Test invalid module path
		let invalid_path = "invalid.module.path";
		let not_resolved = index.resolve_py_module(invalid_path);
		assert!(not_resolved.is_none(), "Should not resolve invalid module path");
	}

	#[test]
	fn test_discover_dependents_of() {
		let index = Index::default();

		// Setup test data with dependencies: base -> module_a -> module_b
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

		// Test discovering dependents of base (should find module_a and module_b)
		let base_dependents = index.discover_dependents_of(base_key);
		assert!(
			base_dependents.contains(&module_a_key),
			"base should have module_a as dependent"
		);
		assert!(
			base_dependents.contains(&module_b_key),
			"base should have module_b as transitive dependent"
		);

		// Test discovering dependents of module_a (should find module_b)
		let module_a_dependents = index.discover_dependents_of(module_a_key);
		assert!(
			module_a_dependents.contains(&module_b_key),
			"module_a should have module_b as dependent"
		);
		assert!(
			!module_a_dependents.contains(&base_key),
			"module_a should not have base as dependent"
		);

		// Test discovering dependents of module_b (should find none)
		let module_b_dependents = index.discover_dependents_of(module_b_key);
		assert!(module_b_dependents.is_empty(), "module_b should have no dependents");
	}

	#[test]
	fn test_discover_dependents_of_with_auto_install() {
		let index = Index::default();

		// Setup test data: sale and crm modules, plus sale_crm with auto_install
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

		// Test: sale should have sale_crm as dependent
		let sale_dependents = index.discover_dependents_of(sale_key);

		assert!(
			sale_dependents.contains(&sale_crm_key),
			"sale should have sale_crm as dependent due to auto_install, but got: {:?}",
			sale_dependents
		);

		// Test: crm should have sale_crm as dependent
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

		// Setup: A depends on B, B depends on C, C depends on base
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

		// Test that resolving A returns all transitive dependencies
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

		// Setup: A depends on B, B depends on C, C depends on A (circular)
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
				dependencies: Box::new([a_key, base_key]), // Circular dependency back to A
				auto_install: false,
				loaded: Default::default(),
				loaded_dependents: Default::default(),
			},
		);

		index.roots.insert(root.clone(), modules);

		// Test that circular dependencies don't cause infinite loop
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

		// Test that we get all modules from all roots
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

		// Test that non-base modules get base as implicit dependency
		let sale_key = _I("sale").into();
		let product_key = _I("product").into();
		let base_key = _I("base").into();

		let mut modules = HashMap::new();

		// Add base module first
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

		// Add sale module with only product as explicit dependency
		// This should get base added implicitly
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

		// Add product module with no dependencies
		// This should get base added implicitly
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

		// Process implicit base dependencies
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

		// Test that base module doesn't get base as dependency
		let base_entry = modules.get(&base_key).unwrap();
		assert!(
			!base_entry.dependencies.contains(&base_key),
			"base should not have itself as dependency"
		);

		// Test that product gets base as implicit dependency
		let product_entry = modules.get(&product_key).unwrap();
		assert!(
			product_entry.dependencies.contains(&base_key),
			"product should have base as implicit dependency"
		);
	}

	#[test]
	fn test_delete_marked_entries() {
		let index = Index::default();

		// Add some test models
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

		// Mark an entry for deletion
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

		// Setup test manifest content
		let manifest_content = br#"
{
    'name': 'Test Module',
    'depends': ['base', 'web', 'mail'],
    'version': '1.0',
}
"#;

		let manifest_path = PathBuf::from("/test/module/__manifest__.py");

		// Mock the file system
		if let Ok(mut fs) = test_utils::fs::TEST_FS.write() {
			fs.insert(manifest_path.clone(), manifest_content);
		}

		let manifest_info = parse_manifest_info(&manifest_path).unwrap();

		// Should include 'base' (auto-added) plus the dependencies from the manifest
		assert!(
			manifest_info.dependencies.len() >= 3,
			"Should have at least base, web, and mail dependencies"
		);

		// Check that base is included (it's auto-added for non-base modules)
		let base_key = _I("base").into();
		assert!(
			manifest_info.dependencies.contains(&base_key),
			"Should include base dependency"
		);

		// Check auto_install is false by default
		assert!(!manifest_info.auto_install, "auto_install should be false by default");
	}

	#[test]
	fn test_parse_manifest_with_auto_install() {
		use crate::test_utils;

		// Setup test manifest content with auto_install
		let manifest_content = br#"
{
    'name': 'Test Auto Install Module',
    'depends': ['base', 'web'],
    'auto_install': True,
}
"#;

		let manifest_path = PathBuf::from("/test/auto_module/__manifest__.py");

		// Mock the file system
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
}
