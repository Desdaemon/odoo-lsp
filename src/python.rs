use std::borrow::Cow;
use std::path::Path;

use lasso::Spur;
use ropey::Rope;
use tower_lsp_server::ls_types::*;
use tracing::{debug, instrument, trace, warn};
use tree_sitter::{Node, Parser, QueryMatch};
use ts_macros::query;

use crate::prelude::*;

use crate::analyze::{Type, type_cache};
use crate::index::{_G, _I, _R, PathSymbol, index_models};
use crate::model::{ModelName, ModelType};
use crate::xml::determine_csv_xmlid_subgroup;
use crate::{backend::Backend, backend::Text};

use std::collections::HashMap;

mod completions;
mod diagnostics;

#[cfg(test)]
mod tests;

#[rustfmt::skip]
query! {
	PyCompletions(Request, XmlId, Mapped, MappedTarget, Depends, ReadFn, Model, Prop, ForXmlId, Scope, FieldDescriptor, FieldType, HasGroups);

(call [
  (attribute [
    (identifier) @_env
    (attribute (_) (identifier) @_env)] (identifier) @_ref)
  (attribute
    (identifier) @REQUEST (identifier) @_render)
  (attribute
    (_) (identifier) @FOR_XML_ID)
  (attribute
  	(_) (identifier) @HAS_GROUPS) ]
  (argument_list . (string) @XML_ID)
  (#eq? @_env "env")
  (#eq? @_ref "ref")
  (#eq? @REQUEST "request")
  (#eq? @_render "render")
  (#eq? @FOR_XML_ID "_for_xml_id")
  (#match? @HAS_GROUPS "^(user_has_groups|has_group)$")
)

(subscript [
  (identifier) @_env
  (attribute (_) (identifier) @_env)]
  (string) @MODEL
  (#eq? @_env "env"))

((class_definition
  (block
    (expression_statement
      (assignment
        (identifier) @PROP [
        (string) @MODEL
        (list ((string) @MODEL ","?)*)
        (call
          (attribute
            (identifier) @_fields (identifier) @FIELD_TYPE (#eq? @_fields "fields"))
          (argument_list
            . [
              ((comment)+ (string) @MODEL)
              (string) @MODEL ]?
            // handles `related` `compute` `search` and `inverse`
            ((keyword_argument (identifier) @FIELD_DESCRIPTOR (_)) ","?)*)) ])))))

(call [
  (attribute
    (_) @MAPPED_TARGET (identifier) @_mapper)
  (attribute
    (identifier) @_api (identifier) @DEPENDS)]
  (argument_list (string) @MAPPED)
  (#match? @_mapper "^(mapp|filter|sort|group)ed$")
  (#eq? @_api "api")
  (#match? @DEPENDS "^(depends|constrains|onchange)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @_search)
  (argument_list [
    (list [
      (tuple . (string) @MAPPED)
      (parenthesized_expression (string) @MAPPED)])
    (keyword_argument
      (identifier) @_domain
      (list [
        (tuple . (string) @MAPPED)
        (parenthesized_expression (string) @MAPPED)]))]))
  (#eq? @_domain "domain")
  (#match? @_search "^(search(_(read|count))?|_?read_group|filtered_domain|_where_calc)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @READ_FN)
  (argument_list [
    (list (string) @MAPPED)
    (keyword_argument
      (identifier) @_domain
      (list (string) @MAPPED)) ]))
  (#match? @_domain "^(groupby|aggregates)$")
  (#match? @READ_FN "^(_?read(_group)?|flush_model)$"))

((call
  (attribute
    (_) @MAPPED_TARGET (identifier) @DEPENDS)
  (argument_list . [
    (set (string) @MAPPED)
    (dictionary [
      (pair key: (string) @MAPPED)
      (ERROR (string) @MAPPED)
      (ERROR) @MAPPED ])
    (_ [
      (set (string) @MAPPED)
      (dictionary [
        (pair key: (string) @MAPPED)
        (ERROR (string) @MAPPED) ]) ]) ]))
  (#match? @DEPENDS "^(create|write|copy)$"))

((class_definition
  (block [
    (function_definition) @SCOPE
    (decorated_definition
      (decorator
        (call
          (attribute (identifier) @_api (identifier) @_depends)
          (argument_list ((string) @MAPPED ","?)*)))
      (function_definition) @SCOPE) ]))
  (#eq? @_api "api")
  (#eq? @_depends "depends"))

(class_definition
  (block
    (decorated_definition
      (decorator (_) @_)
      (function_definition) @SCOPE)*)
  (#not-match? @_ "^api.depends"))
}

#[rustfmt::skip]
query! {
	PyImports(ImportModule, ImportName, ImportAlias);

(import_from_statement
  module_name: (dotted_name) @IMPORT_MODULE
  name: (dotted_name) @IMPORT_NAME)

(import_from_statement
  module_name: (relative_import) @IMPORT_MODULE
  name: (dotted_name) @IMPORT_NAME)

(import_from_statement
  module_name: (dotted_name) @IMPORT_MODULE
  name: (aliased_import
    name: (dotted_name) @IMPORT_NAME
    alias: (identifier) @IMPORT_ALIAS))

(import_from_statement
  module_name: (relative_import) @IMPORT_MODULE
  name: (aliased_import
    name: (dotted_name) @IMPORT_NAME
    alias: (identifier) @IMPORT_ALIAS))

(import_from_statement
  module_name: (dotted_name) @IMPORT_MODULE
  (wildcard_import))

(import_from_statement
  module_name: (relative_import) @IMPORT_MODULE
  (wildcard_import))

(import_statement
  name: (dotted_name) @IMPORT_NAME)

(import_statement
  name: (aliased_import
    name: (dotted_name) @IMPORT_NAME
    alias: (identifier) @IMPORT_ALIAS))
}

/// (module (_)*)
fn top_level_stmt(module: Node, offset: usize) -> Option<Node> {
	module
		.named_children(&mut module.walk())
		.find(|child| child.byte_range().contains_end(offset))
}

fn find_symbol_definition<'a>(
	node: tree_sitter::Node<'a>,
	contents: &str,
	symbol_name: &str,
) -> Option<tree_sitter::Node<'a>> {
	use crate::utils::PreTravel;

	// Try to find as a class definition
	if let Some(class_node) = PreTravel::new(node)
		.find(|n| {
			n.kind() == "class_definition"
				&& n.child_by_field_name("name")
					.map(|name_node| symbol_name == &contents[name_node.byte_range()])
					.unwrap_or(false)
		})
		.and_then(|n| n.child_by_field_name("name"))
	{
		return Some(class_node);
	}

	// Try to find as a function definition
	if let Some(func_node) = PreTravel::new(node)
		.find(|n| {
			n.kind() == "function_definition"
				&& n.child_by_field_name("name")
					.map(|name_node| symbol_name == &contents[name_node.byte_range()])
					.unwrap_or(false)
		})
		.and_then(|n| n.child_by_field_name("name"))
	{
		return Some(func_node);
	}

	// Try to find as a module-level assignment (for constants/variables)
	if let Some(assign_node) = PreTravel::new(node)
		.find(|n| {
			if n.kind() == "assignment"
				&& let Some(left_node) = n.child_by_field_name("left")
			{
				return symbol_name == &contents[left_node.byte_range()];
			}
			false
		})
		.and_then(|n| n.child_by_field_name("left"))
	{
		return Some(assign_node);
	}

	// Try to find in import statements (for re-exports)
	// Only handles relative imports like "from . import fields" or "from .. import models"
	// For absolute imports like "from odoo.models import Model", resolution happens elsewhere
	PreTravel::new(node)
		.find(|n| {
			if n.kind() == "import_from" || n.kind() == "import_from_statement" {
				// Only process relative imports (those with a relative_import node or those without a module_name)
				let is_relative = n
					.child_by_field_name("module_name")
					.map(|m| m.kind() == "relative_import")
					.unwrap_or(false);

				if !is_relative {
					return false;
				}

				// Check all name children (there can be multiple for "from . import a, b, c")
				let mut cursor = n.walk();
				for child in n.children(&mut cursor) {
					// Skip the module_name field
					if matches!(child.kind(), "relative_import" | "dotted_name")
						&& n.child_by_field_name("module_name") == Some(child)
					{
						continue;
					}

					match child.kind() {
						"import_alias" | "aliased_import" => {
							// Wrapped in import_alias/aliased_import: "from . import y as z"
							let mut alias_cursor = child.walk();
							for alias_child in child.children(&mut alias_cursor) {
								if alias_child.kind() == "identifier" || alias_child.kind() == "dotted_name" {
									let text = &contents[alias_child.byte_range()];
									// Check both the original name and any alias
									if text == symbol_name {
										return true;
									}
									// Also check after "as"
									if let Some(as_name) = alias_child.next_sibling()
										&& as_name.kind() == "as" && let Some(alias_name) = as_name.next_sibling()
										&& alias_name.kind() == "identifier"
										&& &contents[alias_name.byte_range()] == symbol_name
									{
										return true;
									}
								}
								// For aliased_import, check the alias field
								if alias_child.kind() == "identifier"
									&& &contents[alias_child.byte_range()] == symbol_name
								{
									// Check if this is the alias field
									if child.child_by_field_name("alias") == Some(alias_child) {
										return true;
									}
								}
							}
						}
						"identifier" | "dotted_name" => {
							// Direct identifier: "from . import fields"
							let text = &contents[child.byte_range()];
							if text == symbol_name {
								return true;
							}
						}
						_ => {}
					}
				}
			}
			false
		})
		.and_then(|import_node| {
			// Return the identifier/dotted_name node that matches the symbol
			let mut cursor = import_node.walk();
			for child in import_node.children(&mut cursor) {
				// Skip the module_name field
				if matches!(child.kind(), "relative_import" | "dotted_name")
					&& import_node.child_by_field_name("module_name") == Some(child)
				{
					continue;
				}

				match child.kind() {
					"import_alias" | "aliased_import" => {
						let mut alias_cursor = child.walk();
						for alias_child in child.children(&mut alias_cursor) {
							if (alias_child.kind() == "identifier" || alias_child.kind() == "dotted_name")
								&& &contents[alias_child.byte_range()] == symbol_name
							{
								return Some(alias_child);
							}
							// Check the alias name
							if alias_child.kind() == "identifier"
								&& let Some(as_name) = alias_child.next_sibling()
								&& as_name.kind() == "as"
								&& let Some(alias_name) = as_name.next_sibling()
								&& alias_name.kind() == "identifier"
								&& &contents[alias_name.byte_range()] == symbol_name
							{
								return Some(alias_name);
							}
							// For aliased_import with alias field, check the alias
							if alias_child.kind() == "identifier"
								&& &contents[alias_child.byte_range()] == symbol_name
								&& child.child_by_field_name("alias") == Some(alias_child)
							{
								return Some(alias_child);
							}
						}
					}
					"identifier" | "dotted_name" => {
						// Direct identifier: "from . import fields"
						if &contents[child.byte_range()] == symbol_name {
							return Some(child);
						}
					}
					_ => {}
				}
			}
			None
		})
}

#[derive(Debug)]
struct Mapped<'text> {
	needle: &'text str,
	model: &'text str,
	single_field: bool,
	range: ByteRange,
}

#[derive(Debug, Clone)]
struct ImportInfo {
	module_path: String,
	imported_name: String,
	alias: Option<String>,
}

type ImportMap = HashMap<String, ImportInfo>;

/// Python extensions.
impl Backend {
	/// Attempt to resolve a symbol that might be an import re-export to its actual source.
	/// If a symbol is found in a file and it's an import statement, recursively follow the chain.
	/// This prevents goto-definition from stopping at `__init__.py` re-exports.
	fn follow_import_chain(
		&self,
		file_path: &std::path::Path,
		symbol_name: &str,
		current_file: &str,
		visited: &mut std::collections::HashSet<std::path::PathBuf>,
	) -> anyhow::Result<Option<Location>> {
		// Prevent infinite recursion
		if visited.contains(file_path) {
			debug!("Already visited file, stopping recursion: {}", file_path.display());
			return Ok(None);
		}
		visited.insert(file_path.to_path_buf());

		let target_contents = ok!(
			test_utils::fs::read_to_string(file_path),
			"Failed to read file {}",
			file_path.display(),
		);

		let mut target_parser = Parser::new();
		target_parser
			.set_language(&tree_sitter_python::LANGUAGE.into())
			.map_err(|e| anyhow::anyhow!("Failed to set parser language: {}", e))?;

		let Some(target_ast) = target_parser.parse(&target_contents, None) else {
			debug!("Failed to parse file");
			return Ok(None);
		};

		if let Some(symbol_node) = find_symbol_definition(target_ast.root_node(), &target_contents, symbol_name) {
			let range = symbol_node.range();

			// Check if this symbol is part of an import statement
			// Walk up the tree to find the import statement node
			let mut current = Some(symbol_node);
			while let Some(node) = current {
				if matches!(
					node.kind(),
					"import_from" | "import_from_statement" | "import_statement"
				) {
					// This is an import - try to parse where it's importing from
					debug!("Found import statement for '{}'", symbol_name);

					// Parse imports from this file to understand what's being imported
					if let Ok(file_imports) = self.parse_imports(&target_contents) {
						// Look for an import entry for this symbol
						if let Some(import_info) = file_imports
							.get(symbol_name)
							.or_else(|| file_imports.values().find(|info| info.imported_name == symbol_name))
						{
							debug!(
								"Symbol '{}' is an import from module '{}'",
								symbol_name, import_info.module_path
							);

							// Try to resolve the actual module file
							if let Some(actual_file) = self
								.index
								.resolve_py_module_from(&import_info.module_path, Some(Path::new(current_file)))
							{
								debug!("Resolved import to actual module: {}", actual_file.display());

								// Check if we've found the actual module file
								if import_info.module_path.ends_with(&import_info.imported_name)
									|| import_info
										.module_path
										.ends_with(&format!(".{}", import_info.imported_name))
								{
									// This is a module import (e.g., "from . import models")
									// Return the module location
									debug!("This is a module import, returning module location");
									return Ok(Some(Location {
										uri: Uri::from_file_path(actual_file).unwrap(),
										range: Range::new(Position::new(0, 0), Position::new(0, 0)),
									}));
								}

								// Otherwise, we need to find the imported symbol in the target module
								// Recursively follow the chain for the symbol
								return self.follow_import_chain(
									&actual_file,
									&import_info.imported_name,
									current_file,
									visited,
								);
							}
						}
					}
					break;
				}
				current = node.parent();
			}

			// Not an import, return the definition location we found
			debug!(
				"Found non-import definition for '{}' at line {}",
				symbol_name, range.start_point.row
			);
			return Ok(Some(Location {
				uri: Uri::from_file_path(file_path).unwrap(),
				range: span_conv(range),
			}));
		}

		Ok(None)
	}

	/// Resolve module.symbol attribute access to definition
	/// Handles cases like: `os.path` -> finds path in os module
	fn resolve_module_attribute_location(
		&self,
		module_name: &str,
		attr_name: &str,
		imports: &ImportMap,
		current_file: &str,
	) -> anyhow::Result<Option<Location>> {
		use std::path::Path;

		// Check if module_name is actually an imported module
		let import_info = imports.get(module_name).or_else(|| {
			// Also check by imported_name (handles "import module" pattern)
			imports
				.values()
				.find(|info| info.imported_name == module_name && info.alias.is_none())
		});

		let Some(import_info) = import_info else {
			debug!("'{}' is not an imported module", module_name);
			return Ok(None);
		};

		debug!(
			"Resolving module.attribute: {}.{} from module '{}'",
			module_name, attr_name, import_info.module_path
		);

		// If the imported_name is the module_name we're accessing, resolve where it points to
		// This handles "from odoo import models" -> models.Model
		let target_file_path = if import_info.imported_name == module_name {
			debug!("Resolving re-exported module '{}' from {}", module_name, current_file);

			// Try treating it as a direct submodule (most common case)
			let submodule_path = format!("{}.{}", import_info.module_path, import_info.imported_name);
			debug!("Trying as submodule: {}", submodule_path);
			match self
				.index
				.resolve_py_module_from(&submodule_path, Some(Path::new(current_file)))
			{
				Some(path) => path,
				None => {
					debug!("Failed to resolve submodule: {}", submodule_path);
					return Ok(None);
				}
			}
		} else {
			// Resolve the module file normally
			match self
				.index
				.resolve_py_module_from(&import_info.module_path, Some(Path::new(current_file)))
			{
				Some(path) => path,
				None => {
					debug!("Failed to resolve module: {}", import_info.module_path);
					return Ok(None);
				}
			}
		};

		let file_path = target_file_path;

		// Read and parse the module
		let target_contents = ok!(
			test_utils::fs::read_to_string(&file_path),
			"Failed to read module file {}",
			file_path.display(),
		);

		let mut target_parser = Parser::new();
		target_parser
			.set_language(&tree_sitter_python::LANGUAGE.into())
			.map_err(|e| anyhow::anyhow!("Failed to set parser language: {}", e))?;

		let Some(target_ast) = target_parser.parse(&target_contents, None) else {
			debug!("Failed to parse target module");
			return Ok(None);
		};

		// Find the symbol in the target module
		if let Some(symbol_node) = find_symbol_definition(target_ast.root_node(), &target_contents, attr_name) {
			let range = symbol_node.range();
			debug!(
				"Found {}.{} at line {}, col {}",
				module_name, attr_name, range.start_point.row, range.start_point.column
			);
			return Ok(Some(Location {
				uri: Uri::from_file_path(file_path).unwrap(),
				range: span_conv(range),
			}));
		}

		// Symbol not found as a definition - don't try fallback imports
		// This avoids triggering parse_imports which can cause diagnostic errors
		debug!(
			"Symbol '{}' not found in module '{}'",
			attr_name, import_info.module_path
		);
		Ok(None)
	}

	/// Helper function to resolve import-based jump-to-definition requests.
	/// Returns the location if successful, None if not found, or an error if resolution fails.
	fn resolve_import_location(
		&self,
		imports: &ImportMap,
		identifier: &str,
		current_file: &str,
	) -> anyhow::Result<Option<Location>> {
		// First try direct lookup by identifier (handles usage sites and direct imports)
		let import_info = if let Some(info) = imports.get(identifier) {
			Some(info)
		} else {
			// If not found, check if identifier matches any imported_name (handles import statements with aliases)
			imports.values().find(|info| info.imported_name == identifier)
		};

		let Some(import_info) = import_info else {
			return Ok(None);
		};

		// Enhanced debugging with alias information
		if let Some(alias) = &import_info.alias {
			debug!(
				"Found aliased import '{}' -> '{}' from module '{}'",
				alias, import_info.imported_name, import_info.module_path
			);
		} else {
			debug!(
				"Found direct import '{}' from module '{}'",
				import_info.imported_name, import_info.module_path
			);
		}

		let Some(file_path) = self
			.index
			.resolve_py_module_from(&import_info.module_path, Some(Path::new(current_file)))
		else {
			debug!("Failed to resolve module path: {}", import_info.module_path);
			return Ok(None);
		};

		debug!("Resolved file path: {}", file_path.display());

		// Check if this is a module import (from . import foo)
		// In this case, the imported_name is the module itself, not a symbol within it
		// e.g., "from . import logging" -> module_path=".logging", imported_name="logging"
		if import_info.module_path.ends_with(&import_info.imported_name)
			|| import_info
				.module_path
				.ends_with(&format!(".{}", import_info.imported_name))
		{
			// This is a module import, return the module file location
			debug!(
				"Module import detected, returning module location: {}",
				file_path.display()
			);
			return Ok(Some(Location {
				uri: Uri::from_file_path(file_path).unwrap(),
				range: Range::new(Position::new(0, 0), Position::new(0, 0)),
			}));
		}

		// Use follow_import_chain to resolve the symbol and follow any import chains
		let symbol_name = &import_info.imported_name;
		if let Some(alias) = &import_info.alias {
			debug!(
				"Looking for original symbol '{}' (aliased as '{}') in target file",
				symbol_name, alias
			);
		} else {
			debug!("Looking for symbol '{}' in target file", symbol_name);
		}

		let mut visited = std::collections::HashSet::new();
		if let Some(location) = self.follow_import_chain(&file_path, symbol_name, current_file, &mut visited)? {
			if let Some(alias) = &import_info.alias {
				debug!(
					"Successfully resolved symbol '{}' (aliased as '{}') to {:?}",
					symbol_name, alias, location
				);
			} else {
				debug!("Successfully resolved symbol '{}' to {:?}", symbol_name, location);
			}
			return Ok(Some(location));
		}

		// Symbol not found directly, but it might be imported in the target file
		// Parse imports from the target file to see if it's re-exported there
		if let Ok(target_imports) = self.parse_imports(&ok!(
			test_utils::fs::read_to_string(&file_path),
			"Failed to read file {}",
			file_path.display()
		)) && let Some(target_import) = target_imports
			.get(symbol_name)
			.or_else(|| target_imports.values().find(|info| info.imported_name == *symbol_name))
		{
			debug!(
				"Symbol '{}' is re-exported from module '{}' in {}",
				symbol_name,
				target_import.module_path,
				file_path.display()
			);

			// Recursively resolve the symbol from its actual source
			if let Some(actual_file) = self
				.index
				.resolve_py_module_from(&target_import.module_path, Some(Path::new(current_file)))
			{
				let mut re_visited = std::collections::HashSet::new();
				if let Some(location) = self.follow_import_chain(
					&actual_file,
					&target_import.imported_name,
					current_file,
					&mut re_visited,
				)? {
					debug!("Resolved symbol '{}' through re-export to {:?}", symbol_name, location);
					return Ok(Some(location));
				}
			}
		}

		// Symbol not found as a definition in the module
		// Check if the target module has wildcard imports that might export this symbol
		let target_file_str = ok!(file_path.to_str(), "Invalid file path");
		let target_contents_str = ok!(test_utils::fs::read_to_string(&file_path), "Failed to read file");
		if let Ok(target_imports) = self.parse_imports(&target_contents_str) {
			for (key, wildcard_info) in target_imports {
				if !key.starts_with("*from_") || wildcard_info.imported_name != "*" {
					continue;
				}

				debug!(
					"Checking wildcard import from '{}' for symbol '{}'",
					wildcard_info.module_path, symbol_name
				);

				// Try to resolve the wildcard source
				if let Some(wildcard_source_file) = self
					.index
					.resolve_py_module_from(&wildcard_info.module_path, Some(Path::new(target_file_str)))
				{
					// Try to find the symbol in the wildcard source
					let mut wildcard_visited = std::collections::HashSet::new();
					if let Ok(Some(location)) = self.follow_import_chain(
						&wildcard_source_file,
						symbol_name,
						current_file,
						&mut wildcard_visited,
					) {
						debug!(
							"Found symbol '{}' in wildcard import from '{}' in module '{}'",
							symbol_name,
							wildcard_info.module_path,
							file_path.display()
						);
						return Ok(Some(location));
					}
				}
			}
		}

		// Try to check if it's a submodule (e.g., "from odoo import fields" where fields is a module)
		let submodule_path = format!("{}.{}", import_info.module_path, symbol_name);
		debug!(
			"Symbol '{}' not found in module, trying submodule path: {}",
			symbol_name, submodule_path
		);

		if let Some(submodule_file) = self
			.index
			.resolve_py_module_from(&submodule_path, Some(Path::new(current_file)))
		{
			debug!("Submodule resolved to: {}", submodule_file.display());
			return Ok(Some(Location {
				uri: Uri::from_file_path(submodule_file).unwrap(),
				range: Range::new(Position::new(0, 0), Position::new(0, 0)),
			}));
		}

		// Submodule not found either
		if let Some(alias) = &import_info.alias {
			debug!(
				"Symbol '{}' (aliased as '{}') not found in target file using tree-sitter",
				symbol_name, alias
			);
		} else {
			debug!("Symbol '{}' not found in target file using tree-sitter", symbol_name);
		}
		Ok(None)
	}

	/// Try to resolve a symbol from wildcard imports
	/// Only called when normal resolution fails, to minimize work
	fn resolve_from_wildcard_imports(
		&self,
		identifier: &str,
		imports: &ImportMap,
		current_file: &str,
	) -> anyhow::Result<Option<Location>> {
		use std::path::Path;

		// Look for wildcard imports in the imports map
		for (key, import_info) in imports {
			// Check if this is a wildcard import (has "*" in the key)
			if !key.starts_with("*from_") || import_info.imported_name != "*" {
				continue;
			}

			debug!(
				"Checking wildcard import from module '{}' for symbol '{}'",
				import_info.module_path, identifier
			);

			// Resolve the wildcard source module
			let Some(source_file) = self
				.index
				.resolve_py_module_from(&import_info.module_path, Some(Path::new(current_file)))
			else {
				debug!("Failed to resolve wildcard source module: {}", import_info.module_path);
				continue;
			};

			// Parse the source module to see if it has the symbol
			let source_contents = match test_utils::fs::read_to_string(&source_file) {
				Ok(contents) => contents,
				Err(_) => {
					debug!("Failed to read wildcard source file: {}", source_file.display());
					continue;
				}
			};

			// Try to find the symbol in the source module
			let mut parser = Parser::new();
			if let Err(_) = parser.set_language(&tree_sitter_python::LANGUAGE.into()) {
				continue;
			}

			let Some(_ast) = parser.parse(&source_contents, None) else {
				continue;
			};

			// Use follow_import_chain to find the symbol in the source
			let mut visited = std::collections::HashSet::new();
			if let Ok(Some(location)) = self.follow_import_chain(&source_file, identifier, current_file, &mut visited) {
				debug!(
					"Found symbol '{}' in wildcard import from module '{}'",
					identifier, import_info.module_path
				);
				return Ok(Some(location));
			}
		}

		debug!("Symbol '{}' not found in any wildcard imports", identifier);
		Ok(None)
	}

	#[tracing::instrument(skip_all, ret, fields(uri))]
	pub fn on_change_python(
		&self,
		text: &Text,
		uri: &Uri,
		rope: RopeSlice<'_>,
		old_rope: Option<Rope>,
	) -> anyhow::Result<()> {
		let mut parser = Parser::new();
		parser
			.set_language(&tree_sitter_python::LANGUAGE.into())
			.expect("bug: failed to init python parser");
		self.update_ast(text, uri, rope, old_rope, parser)?;

		// Also update models on change (not just on save), so that class_chain is populated
		// for completions that happen before the file is saved
		if let Some(path) = uri.to_file_path()
			&& let Some(root_path) = self.index.find_root_of(&path)
		{
			let root = _I(root_path.to_string_lossy().as_ref());
			let rope_copy = Rope::from(rope);
			// Clone the Text enum variant
			let text_copy = match text {
				Text::Full(s) => Text::Full(s.clone()),
				Text::Delta(d) => Text::Delta(d.clone()),
			};
			if let Err(err) = self.update_models(text_copy, &path, root, rope_copy) {
				debug!("update_models failed: {err:?}");
			}
		}
		Ok(())
	}

	/// Parse import statements from Python content and return a map of imported names to their module paths
	fn parse_imports(&self, contents: &str) -> anyhow::Result<ImportMap> {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into())?;

		let ast = parser
			.parse(contents, None)
			.ok_or_else(|| errloc!("Failed to parse Python AST"))?;
		let query = PyImports::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut imports = ImportMap::new();

		debug!("Parsing imports from {} bytes", contents.len());

		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			let mut module_path = None;
			let mut import_name = None;
			let mut alias = None;

			debug!("Found import match with {} captures", match_.captures.len());

			for capture in match_.captures {
				let capture_text = &contents[capture.node.byte_range()];
				debug!("Capture {}: = '{}'", capture.index, capture_text);

				match PyImports::from(capture.index) {
					Some(PyImports::ImportModule) => {
						module_path = Some(capture_text.to_string());
					}
					Some(PyImports::ImportName) => {
						import_name = Some(capture_text.to_string());
					}
					Some(PyImports::ImportAlias) => {
						alias = Some(capture_text.to_string());
					}
					_ => {}
				}
			}

			// Add the module itself to imports (so "from odoo import X" makes "odoo" clickable)
			if let Some(module) = &module_path {
				// Extract the top-level module name
				let top_level_module = if module.chars().all(|c| c == '.') {
					// Pure relative import like "." or "..", skip
					None
				} else {
					module.split('.').next().map(|first_part| first_part.to_string())
				};

				if let Some(top_level) = top_level_module {
					// Only add if not already in imports (avoid overwriting named imports)
					if !imports.contains_key(&top_level) {
						debug!("Adding module {} to imports (can goto-def on module name)", top_level);
						imports.insert(
							top_level.clone(),
							ImportInfo {
								module_path: top_level.clone(),
								imported_name: top_level.clone(),
								alias: None,
							},
						);
					}
				}
			}

			if let Some(name) = import_name {
				let full_module_path = if let Some(module) = module_path {
					// Check if this is "from . import foo" vs "from .utils import foo"
					// In "from . import foo", module is "." and name is "foo" (the submodule)
					// In "from .utils import foo", module is ".utils" and name is "foo" (the symbol)

					// If module is just dots (relative import with no module part),
					// then name is actually a submodule, not a symbol
					if module.chars().all(|c| c == '.') {
						// This is "from . import foo" or "from .. import bar"
						// Append the name to the module path
						format!("{}{}", module, name)
					} else {
						// This is "from .utils import foo" or "from package import symbol"
						module
					}
				} else {
					name.clone() // For "import name", the module path is the name itself
				};

				let key = alias.as_ref().unwrap_or(&name).clone();
				debug!("Adding import: {} -> {} (from module {})", key, name, full_module_path);
				imports.insert(
					key,
					ImportInfo {
						module_path: full_module_path,
						imported_name: name,
						alias,
					},
				);
			} else if let Some(module) = &module_path {
				// This is a wildcard import like "from module import *"
				// Store it with a special "*" imported_name marker
				debug!("Adding wildcard import from module {}", module);
				// Use a special key that won't collide with normal imports
				let wildcard_key = format!("*from_{}*", module);
				imports.insert(
					wildcard_key,
					ImportInfo {
						module_path: module.clone(),
						imported_name: "*".to_string(),
						alias: None,
					},
				);
			}
		}

		debug!("Final imports map: {:?}", imports);
		Ok(imports)
	}
	pub fn update_models(&self, text: Text, path: &Path, root: Spur, rope: Rope) -> anyhow::Result<()> {
		let text = match text {
			Text::Full(text) => Cow::from(text),
			// TODO: Limit range of possible updates based on delta
			Text::Delta(_) => Cow::from(rope.slice(..)),
		};
		let models = index_models(text.as_bytes())?;
		let path_sym = PathSymbol::strip_root(root, path);
		self.index.models.append(path_sym, true, &models);

		// Create fragment ClassIds for each model class definition
		let fragments = self.index.create_model_fragments_from_file(path, &text);

		// Get BaseModel ClassId (if available)
		let base_model_id = self.index.get_base_model_class_id();

		for model in models {
			match model.type_ {
				ModelType::Base { name, ancestors } => {
					let model_key = _G(&name).unwrap();
					let mut entry = self
						.index
						.models
						.try_get_mut(&model_key.into())
						.expect(format_loc!("deadlock"))
						.unwrap();

					// Populate class_chain: [BaseModel, ...fragments]
					// Only add BaseModel once (if not already present)
					if let Some(base_id) = base_model_id
						&& entry.class_chain.is_empty()
					{
						entry.class_chain.push(base_id);
					}

					// Add fragments from this file (avoid duplicates)
					let model_name_str = ImStr::from(name.as_str());
					if let Some(fragment_ids) = fragments.get(&model_name_str) {
						for &fragment_id in fragment_ids {
							if !entry.class_chain.contains(&fragment_id) {
								entry.class_chain.push(fragment_id);
							}
						}
					}

					entry
						.ancestors
						.extend(ancestors.into_iter().map(|sym| ModelName::from(_I(&sym))));
					drop(entry);
					// Don't pass locations_filter - class_chain now has all fragments from module loading
					self.index.models.populate_properties(model_key.into(), &[]);
				}
				ModelType::Inherit(inherits) => {
					let Some(inherited_model_name) = inherits.first() else {
						continue;
					};
					let model_key = _G(inherited_model_name).unwrap();

					// For inherited models, populate class_chain with: [BaseModel, inherited_chain..., new_fragments]
					// This ensures we inherit fields/methods from the parent model first, then any overrides
					if let Some(mut entry) = self
						.index
						.models
						.try_get_mut(&model_key.into())
						.expect(format_loc!("deadlock"))
					{
						// Ensure BaseModel is first in the chain if not already present
						if let Some(base_id) = base_model_id
							&& !entry.class_chain.contains(&base_id)
						{
							entry.class_chain.insert(0, base_id);
						}

						// Add fragments from this file for the inherited model
						// These come AFTER existing chain entries, representing overrides/extensions
						if let Some(fragment_ids) = fragments.get(&ImStr::from(inherited_model_name.as_str())) {
							for &frag_id in fragment_ids {
								if !entry.class_chain.contains(&frag_id) {
									entry.class_chain.push(frag_id);
								}
							}
						}
					}

					// Don't pass locations_filter - class_chain now has all fragments from module loading
					self.index.models.populate_properties(model_key.into(), &[]);
				}
			}
		}

		let model_class_id = crate::analyze::class_cache().get_by_name("Model");
		for (frag_name, frag_ids) in fragments {
			let symbol: ModelName = _I(frag_name).into();
			{
				let mut entry = self.index.models.entry(symbol).or_default();
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
			self.index.models.populate_properties(symbol, &[]);
		}
		Ok(())
	}
	pub async fn did_save_python(&self, uri: Uri, root: Spur) -> anyhow::Result<()> {
		let path = uri.to_file_path().unwrap();
		let zone;
		_ = {
			let mut document = self
				.document_map
				.get_mut(uri.path().as_str())
				.ok_or_else(|| errloc!("(did_save) did not build document"))?;
			zone = document.damage_zone.take();
			let rope = document.rope.clone();
			let text = Cow::from(&document.rope).into_owned();
			self.update_models(Text::Full(text), &path, root, rope)
		}
		.inspect_err(|err| warn!("{err:?}"));
		if zone.is_some() {
			debug!("diagnostics");
			{
				let mut document = self.document_map.get_mut(uri.path().as_str()).unwrap();
				let rope = document.rope.clone();
				let file_path = uri.to_file_path().unwrap();
				self.diagnose_python(
					file_path.to_str().unwrap(),
					rope.slice(..),
					zone,
					&mut document.diagnostics_cache,
				);
				let diags = document.diagnostics_cache.clone();
				self.client.publish_diagnostics(uri, diags, None)
			}
			.await;
		}

		Ok(())
	}
	/// Gathers common information regarding a mapped access aka dot access.
	/// Only makes sense in [`PyCompletions`] queries.
	///
	/// `single_field_override` should be set in special cases where this function may not have the necessary context to
	/// determine whether the needle should be processed in mapped mode or single field mode.
	///
	/// Replacing:
	///
	/// ```text
	///     "foo.bar.baz"
	///           ^cursor
	///      -----------range
	///      ------needle
	/// ```
	///
	/// Not replacing:
	///
	/// ```text
	///     "foo.bar.baz"
	///           ^cursor
	///      -------range
	///      -------needle
	/// ```
	#[instrument(skip_all, ret, fields(range_content = &contents[range.clone()]))]
	fn gather_mapped<'text>(
		&self,
		root: Node,
		match_: &tree_sitter::QueryMatch,
		offset: Option<usize>,
		mut range: core::ops::Range<usize>,
		this_model: Option<&'text str>,
		contents: &'text str,
		for_replacing: bool,
		single_field_override: Option<bool>,
	) -> Option<Mapped<'text>> {
		let mut needle = if for_replacing {
			range = range.shrink(1);
			let offset = offset.unwrap_or(range.end);
			&contents[range.start..offset]
		} else {
			let slice = &contents[range.clone().shrink(1)];
			let relative_start = range.start + 1;
			let offset = offset
				.unwrap_or((range.end - 1).max(relative_start + 1))
				.max(relative_start)
				.min(relative_start + slice.len());
			// assert!(
			// 	offset >= relative_start,
			// 	"offset={} cannot be less than relative_start={}",
			// 	offset,
			// 	relative_start
			// );
			let start = offset - relative_start;
			let slice_till_end = slice.get(start..).unwrap_or("");
			// How many characters until the next period or end-of-string?
			let limit = slice_till_end.find('.').unwrap_or(slice_till_end.len());
			range = relative_start..offset + limit;
			// Cow::from(rope.try_slice(range.clone())?)
			&contents[range.clone()]
		};
		if needle == "|" || needle == "&" {
			return None;
		}

		tracing::trace!("(gather_mapped) {} matches={match_:?}", &contents[range.clone()]);

		let model;
		if let Some(local_model) = match_.nodes_for_capture_index(PyCompletions::MappedTarget as _).next() {
			let model_ = (self.index).model_of_range(root, local_model.byte_range().map_unit(ByteOffset), contents)?;
			model = _R(model_);
		} else if let Some(this_model) = &this_model {
			model = this_model
		} else {
			return None;
		}

		let mut single_field = false;
		if let Some(depends) = match_.nodes_for_capture_index(PyCompletions::Depends as _).next() {
			single_field = matches!(
				&contents[depends.byte_range()],
				"write" | "create" | "constrains" | "onchange"
			);
		} else if let Some(read_fn) = match_.nodes_for_capture_index(PyCompletions::ReadFn as _).next() {
			// read or read_group, fields only
			single_field = true;
			if contents[read_fn.byte_range()].ends_with("read_group") {
				// split off aggregate functions
				needle = match needle.split_once(":") {
					None => needle,
					Some((field, _)) => {
						range = range.start..range.start + field.len();
						field
					}
				}
			}
		} else if let Some(override_) = single_field_override {
			single_field = override_;
		}

		Some(Mapped {
			needle,
			model,
			single_field,
			range: range.map_unit(ByteOffset),
		})
	}
	pub fn python_jump_def(
		&self,
		params: GotoDefinitionParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Location>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let file_path = uri.to_file_path().unwrap();
		let file_path_str = file_path.to_str().unwrap();
		let ast = self
			.ast_map
			.get(file_path_str)
			.ok_or_else(|| errloc!("Did not build AST for {}", file_path_str))?;
		let ByteOffset(offset) = rope_conv(params.text_document_position_params.position, rope);
		let contents = Cow::from(rope);
		let root = some!(top_level_stmt(ast.root_node(), offset));

		// Parse imports from the current file
		let imports = self.parse_imports(&contents).unwrap_or_default();
		debug!("Parsed imports: {:?}", imports);

		// Check for wildcard imports or module names in import statements
		let query = PyImports::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				if range.contains(&offset)
					&& let Some(PyImports::ImportModule) = PyImports::from(capture.index)
				{
					// Cursor is on a module_name in an import statement
					let module_name = &contents[range];
					debug!("Cursor on module_name '{}' in import statement", module_name);
					if let Some(resolved_file) = self
						.index
						.resolve_py_module_from(module_name, Some(Path::new(file_path_str)))
					{
						debug!("Resolved to: {}", resolved_file.display());
						return Ok(Some(Location {
							uri: Uri::from_file_path(resolved_file).unwrap(),
							range: Range::new(Position::new(0, 0), Position::new(0, 0)),
						}));
					}
				}
			}
		}

		// Check if cursor is on an imported identifier or module attribute access
		if let Some(cursor_node) = ast.root_node().descendant_for_byte_range(offset, offset) {
			match cursor_node.kind() {
				"identifier" => {
					let identifier = &contents[cursor_node.byte_range()];
					debug!("Checking identifier '{}' at offset {}", identifier, offset);

					// Check if this identifier is part of a dotted_name in an import statement
					// e.g., cursor on "models" in "from odoo.models import X"
					if let Some(parent) = cursor_node.parent()
						&& parent.kind() == "dotted_name"
					{
						// Check if this dotted_name is a module_name in an import statement
						if let Some(import_node) = parent.parent()
							&& (import_node.kind() == "import_from_statement"
								|| import_node.kind() == "import_statement")
							&& import_node.child_by_field_name("module_name") == Some(parent)
						{
							// Build the module path up to and including the clicked identifier
							let dotted_name_text = &contents[parent.byte_range()];
							debug!(
								"Cursor on identifier '{}' in dotted module name '{}'",
								identifier, dotted_name_text
							);

							// Find which part of the dotted name was clicked
							// e.g., for "odoo.models", if "models" was clicked, resolve "odoo.models"
							let parts: Vec<&str> = dotted_name_text.split('.').collect();
							let clicked_offset = offset - parent.start_byte();

							// Find which segment contains the cursor
							let mut cumulative_len = 0;
							for (i, part) in parts.iter().enumerate() {
								let part_start = cumulative_len;
								let part_end = part_start + part.len();

								if clicked_offset >= part_start && clicked_offset < part_end {
									// The cursor is on this part - resolve up to and including it
									let module_path = parts[..=i].join(".");
									debug!("Resolving partial module path: {}", module_path);

									if let Some(resolved_file) = self
										.index
										.resolve_py_module_from(&module_path, Some(Path::new(file_path_str)))
									{
										debug!("Resolved to: {}", resolved_file.display());
										return Ok(Some(Location {
											uri: Uri::from_file_path(resolved_file).unwrap(),
											range: Range::new(Position::new(0, 0), Position::new(0, 0)),
										}));
									}
									break;
								}

								cumulative_len = part_end + 1; // +1 for the dot
							}
						}
					}

					// Check if this identifier is part of an attribute access (module.symbol)
					if let Some(parent) = cursor_node.parent()
						&& parent.kind() == "attribute"
						&& parent.child_by_field_name("attribute") == Some(cursor_node)
					{
						// Cursor is on the attribute name in "module.symbol"
						if let Some(obj_node) = parent.child_by_field_name("object") {
							let obj_name = &contents[obj_node.byte_range()];
							let attr_name = identifier;
							debug!("Module attribute access: {}.{}", obj_name, attr_name);

							// Try to resolve module.symbol
							if let Some(location) =
								self.resolve_module_attribute_location(obj_name, attr_name, &imports, file_path_str)?
							{
								return Ok(Some(location));
							}
						}
					} else {
						// Regular identifier - try as import
						if let Some(location) = self.resolve_import_location(&imports, identifier, file_path_str)? {
							return Ok(Some(location));
						}

						// If not found in direct imports, try wildcard imports (last resort)
						if let Some(location) =
							self.resolve_from_wildcard_imports(identifier, &imports, file_path_str)?
						{
							return Ok(Some(location));
						}
					}
				}
				"attribute" => {
					// Cursor might be on the dot or object in attribute expression
					// Try to get the attribute name (rightmost identifier)
					if let Some(attr_node) = cursor_node.child_by_field_name("attribute") {
						let attr_name = &contents[attr_node.byte_range()];
						if let Some(obj_node) = cursor_node.child_by_field_name("object") {
							let obj_name = &contents[obj_node.byte_range()];
							debug!("Module attribute access: {}.{}", obj_name, attr_name);

							if let Some(location) =
								self.resolve_module_attribute_location(obj_name, attr_name, &imports, file_path_str)?
							{
								return Ok(Some(location));
							}
						}
					}
				}
				_ => {}
			}
		}

		let query = PyCompletions::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut this_model = ThisModel::default();

		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let slice = Cow::from(ok!(rope.try_slice(range.clone())));
						let mut slice = slice.as_ref();
						if match_
							.nodes_for_capture_index(PyCompletions::HasGroups as _)
							.next()
							.is_some()
						{
							let mut ref_ = None;
							determine_csv_xmlid_subgroup(&mut ref_, (slice, range.clone()), offset);
							(slice, _) = some!(ref_);
						}
						return self
							.index
							.jump_def_xml_id(slice, &params.text_document_position_params.text_document.uri);
					}
					Some(PyCompletions::Model) => {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::Prop as _)
							.next()
							.map(|prop| matches!(&contents[prop.byte_range()], "_name" | "_inherit"))
							.unwrap_or(true);
						if range.contains(&offset) {
							let range = range.shrink(1);
							let slice = ok!(rope.try_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.jump_def_model(&slice);
						} else if range.end < offset && is_meta
						// match_
						// 	.nodes_for_capture_index(PyCompletions::FieldType as _)
						// 	.next()
						// 	.is_none()
						{
							this_model.tag_model(capture.node, match_, root.byte_range(), &contents);
						}
					}
					Some(PyCompletions::Mapped) => {
						if range.contains_end(offset)
							&& let Some(mapped) = self.gather_mapped(
								root,
								match_,
								Some(offset),
								range.clone(),
								this_model.inner,
								&contents,
								false,
								None,
							) {
							let mut needle = mapped.needle;
							let mut model = _I(mapped.model);
							if !mapped.single_field {
								some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
							}
							let model = _R(model);
							return self.index.jump_def_property_name(needle, model);
						} else if let Some(cmdlist) = python_next_named_sibling(capture.node)
							&& Backend::is_commandlist(cmdlist, offset)
						{
							let (needle, _, model) = some!(self.gather_commandlist(
								cmdlist,
								root,
								match_,
								offset,
								range,
								this_model.inner,
								&contents,
								false,
							));
							return self.index.jump_def_property_name(needle, _R(model));
						}
					}
					Some(PyCompletions::FieldDescriptor) => {
						let Some(desc_value) = python_next_named_sibling(capture.node) else {
							continue;
						};

						let descriptor = &contents[capture.node.byte_range()];
						if !desc_value.byte_range().contains_end(offset) {
							continue;
						}
						if matches!(descriptor, "comodel_name") {
							let range = desc_value.byte_range().shrink(1);
							let slice = ok!(rope.try_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.jump_def_model(&slice);
						} else if matches!(descriptor, "compute" | "search" | "inverse" | "related") {
							let single_field = descriptor != "related";
							// same as PyCompletions::Mapped
							let Some(mapped) = self.gather_mapped(
								root,
								match_,
								Some(offset),
								desc_value.byte_range(),
								this_model.inner,
								&contents,
								false,
								Some(single_field),
							) else {
								break;
							};
							let mut needle = mapped.needle;
							let mut model = _I(mapped.model);
							if !mapped.single_field {
								some!(self.index.models.resolve_mapped(&mut model, &mut needle, None).ok());
							}
							let model = _R(model);
							return self.index.jump_def_property_name(needle, model);
						} else if matches!(descriptor, "groups") {
							let range = desc_value.byte_range().shrink(1);
							let value = Cow::from(ok!(rope.try_slice(range.clone())));
							let mut ref_ = None;
							determine_csv_xmlid_subgroup(&mut ref_, (&value, range), offset);
							let (needle, _) = some!(ref_);
							return self.index.jump_def_xml_id(needle, uri);
						}

						return Ok(None);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::HasGroups)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}
		}

		let (model, prop, _) = some!(self.attribute_at_offset(offset, root, &contents));
		self.index.jump_def_property_name(prop, model)
	}
	/// Resolves the attribute and the object's model at the cursor offset
	/// using [`model_of_range`][Index::model_of_range].
	///
	/// Returns `(model, property, range)`.
	fn attribute_at_offset<'out>(
		&'out self,
		offset: usize,
		root: Node<'out>,
		contents: &'out str,
	) -> Option<(&'out str, &'out str, core::ops::Range<usize>)> {
		let (lhs, field, range) = Self::attribute_node_at_offset(offset, root, contents)?;
		let model = (self.index).model_of_range(root, lhs.byte_range().map_unit(ByteOffset), contents)?;
		Some((_R(model), field, range))
	}
	/// Resolves the attribute at the cursor offset.
	/// Returns `(object, field, range)`
	#[instrument(level = "trace", skip_all, ret)]
	pub fn attribute_node_at_offset<'out>(
		mut offset: usize,
		root: Node<'out>,
		contents: &'out str,
	) -> Option<(Node<'out>, &'out str, core::ops::Range<usize>)> {
		if contents.is_empty() {
			return None;
		}
		offset = offset.clamp(0, contents.len() - 1);
		let mut cursor_node = root.descendant_for_byte_range(offset, offset)?;
		let mut real_offset = None;
		if cursor_node.is_named() && !matches!(cursor_node.kind(), "attribute" | "identifier") {
			// We got our cursor left in the middle of nowhere.
			real_offset = Some(offset);
			offset = offset.saturating_sub(1);
			cursor_node = root.descendant_for_byte_range(offset, offset)?;
		}
		trace!(
			"(attribute_node_to_offset) {} cursor={}\n  sexp={}",
			&contents[cursor_node.byte_range()],
			contents.as_bytes()[offset] as char,
			cursor_node.to_sexp(),
		);
		let lhs;
		let rhs;
		if !cursor_node.is_named() {
			// We landed on one of the punctuations inside the attribute.
			// Need to determine which one it is.
			// We cannot depend on prev_named_sibling because the AST may be all messed up
			let idx = contents[..=offset].bytes().rposition(|c| c == b'.')?;
			let ident = contents[..=idx].bytes().rposition(|c| c.is_ascii_alphanumeric())?;
			lhs = root.descendant_for_byte_range(ident, ident)?;
			rhs = python_next_named_sibling(lhs).and_then(|attr| match attr.kind() {
				"identifier" => Some(attr),
				"attribute" => attr.child_by_field_name("attribute"),
				_ => None,
			});
		} else if cursor_node.kind() == "attribute" {
			lhs = cursor_node.child_by_field_name("object")?;
			rhs = cursor_node.child_by_field_name("attribute");
		} else {
			match cursor_node.parent() {
				Some(parent) if parent.kind() == "attribute" => {
					lhs = parent.child_by_field_name("object")?;
					rhs = Some(cursor_node);
				}
				Some(parent) if parent.kind() == "ERROR" => {
					// (ERROR (_) @cursor_node)
					lhs = cursor_node;
					rhs = None;
				}
				_ => return None,
			}
		}
		trace!(
			"(attribute_node_to_offset) lhs={} rhs={:?}",
			&contents[lhs.byte_range()],
			rhs.as_ref().map(|rhs| &contents[rhs.byte_range()]),
		);
		if lhs == cursor_node {
			// We shouldn't recurse into cursor_node itself.
			return None;
		}
		let Some(rhs) = rhs else {
			// In single-expression mode, rhs could be empty in which case
			// we return an empty needle/range.
			let offset = real_offset.unwrap_or(offset);
			return Some((lhs, "", offset..offset));
		};
		let (field, range) = if rhs.range().start_point.row != lhs.range().end_point.row {
			// tree-sitter has an issue with attributes spanning multiple lines
			// which is NOT valid Python, but allows it anyways because tree-sitter's
			// use cases don't require strict syntax trees.
			let offset = real_offset.unwrap_or(offset);
			("", offset..offset)
		} else {
			let range = rhs.byte_range();
			(&contents[range.clone()], range)
		};

		Some((lhs, field, range))
	}
	pub fn python_references(
		&self,
		params: ReferenceParams,
		rope: RopeSlice<'_>,
	) -> anyhow::Result<Option<Vec<Location>>> {
		let ByteOffset(offset) = rope_conv(params.text_document_position.position, rope);
		let uri = &params.text_document_position.text_document.uri;
		let file_path = uri.to_file_path().unwrap();
		let file_path_str = file_path.to_str().unwrap();
		let ast = self
			.ast_map
			.get(file_path_str)
			.ok_or_else(|| errloc!("Did not build AST for {}", file_path_str))?;
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let contents = Cow::from(rope);
		let mut cursor = tree_sitter::QueryCursor::new();
		let path = some!(params.text_document_position.text_document.uri.to_file_path());
		let current_module = self.index.find_module_of(&path);
		let mut this_model = ThisModel::default();

		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::XmlId) if range.contains(&offset) => {
						let range = range.shrink(1);
						let slice = Cow::from(ok!(rope.try_slice(range.clone())));
						let mut slice = slice.as_ref();
						if match_
							.nodes_for_capture_index(PyCompletions::HasGroups as _)
							.next()
							.is_some()
						{
							let mut ref_ = None;
							determine_csv_xmlid_subgroup(&mut ref_, (slice, range.clone()), offset);
							(slice, _) = some!(ref_);
						}
						return self.record_references(&path, slice, current_module);
					}
					Some(PyCompletions::Model) => {
						let range = capture.node.byte_range();
						let is_meta = match_
							.nodes_for_capture_index(PyCompletions::Prop as _)
							.next()
							.map(|prop| matches!(&contents[prop.byte_range()], "_name" | "_inherit"))
							.unwrap_or(true);
						if is_meta && range.contains(&offset) {
							let range = range.shrink(1);
							let slice = ok!(rope.try_slice(range.clone()));
							let slice = Cow::from(slice);
							let slice = some!(_G(slice));
							return self.model_references(&path, &slice.into());
						} else if range.end < offset
							&& match_
								.nodes_for_capture_index(PyCompletions::FieldType as _)
								.next()
								.is_none()
						{
							this_model.tag_model(capture.node, match_, root.byte_range(), &contents);
						}
					}
					Some(PyCompletions::FieldDescriptor) => {
						let Some(desc_value) = python_next_named_sibling(capture.node) else {
							continue;
						};
						let descriptor = &contents[range];
						// TODO: related, when field inheritance is implemented
						if !desc_value.byte_range().contains_end(offset) {
							continue;
						};

						if matches!(descriptor, "comodel_name") {
							let range = desc_value.byte_range().shrink(1);
							let slice = ok!(rope.try_slice(range.clone()));
							let slice = Cow::from(slice);
							let slice = some!(_G(slice));
							return self.model_references(&path, &slice.into());
						} else if matches!(descriptor, "compute" | "search" | "inverse") {
							let range = desc_value.byte_range().shrink(1);
							let model = some!(this_model.inner.as_ref());
							let prop = &contents[range];
							return self.index.method_references(prop, model);
						}

						return Ok(None);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::HasGroups)
					| Some(PyCompletions::Mapped)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}
		}

		let (model, prop, _) = some!(self.attribute_at_offset(offset, root, &contents));
		self.index.method_references(prop, model)
	}

	pub fn python_hover(&self, params: HoverParams, rope: RopeSlice<'_>) -> anyhow::Result<Option<Hover>> {
		let uri = &params.text_document_position_params.text_document.uri;
		let file_path = uri.to_file_path().unwrap();
		let file_path_str = file_path.to_str().unwrap();
		let ast = self
			.ast_map
			.get(file_path_str)
			.ok_or_else(|| errloc!("Did not build AST for {}", file_path_str))?;
		let ByteOffset(offset) = rope_conv(params.text_document_position_params.position, rope);

		let contents = Cow::from(rope);
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let query = PyCompletions::query();
		let mut cursor = tree_sitter::QueryCursor::new();
		let mut this_model = ThisModel::default();

		let mut matches = cursor.matches(query, ast.root_node(), contents.as_bytes());
		while let Some(match_) = matches.next() {
			for capture in match_.captures {
				let range = capture.node.byte_range();
				match PyCompletions::from(capture.index) {
					Some(PyCompletions::Model) => {
						if range.contains_end(offset) {
							let range = range.shrink(1);
							let lsp_range = span_conv(capture.node.range());
							let slice = ok!(rope.try_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.hover_model(&slice, Some(lsp_range), false, None);
						}
						if range.end < offset
							&& match_
								.nodes_for_capture_index(PyCompletions::Prop as _)
								.next()
								.is_some()
						{
							this_model.tag_model(capture.node, match_, root.byte_range(), &contents);
						}
					}
					Some(PyCompletions::Mapped) => {
						if range.contains(&offset) {
							let mapped = some!(self.gather_mapped(
								root,
								match_,
								Some(offset),
								range.clone(),
								this_model.inner,
								&contents,
								false,
								None,
							));
							let mut needle = mapped.needle;
							let mut model = _I(mapped.model);
							let mut range = mapped.range;
							if !mapped.single_field {
								some!(
									self.index
										.models
										.resolve_mapped(&mut model, &mut needle, Some(&mut range))
										.ok()
								);
							}
							let model = _R(model);
							return (self.index).hover_property_name(needle, model, Some(rope_conv(range, rope)));
						} else if let Some(cmdlist) = python_next_named_sibling(capture.node)
							&& Backend::is_commandlist(cmdlist, offset)
						{
							let (needle, range, model) = some!(self.gather_commandlist(
								cmdlist,
								root,
								match_,
								offset,
								range,
								this_model.inner,
								&contents,
								false,
							));
							let range = Some(rope_conv(range, rope));
							return self.index.hover_property_name(needle, _R(model), range);
						}
					}
					Some(PyCompletions::XmlId) if range.contains_end(offset) => {
						let range = range.shrink(1);
						let slice = Cow::from(ok!(rope.try_slice(range.clone())));
						let mut slice = slice.as_ref();
						if match_
							.nodes_for_capture_index(PyCompletions::HasGroups as _)
							.next()
							.is_some()
						{
							let mut ref_ = None;
							determine_csv_xmlid_subgroup(&mut ref_, (slice, range.clone()), offset);
							if let Some((needle, _)) = ref_ {
								slice = needle;
							}
						}
						return (self.index).hover_record(slice, Some(rope_conv(range.map_unit(ByteOffset), rope)));
					}
					Some(PyCompletions::Prop) if range.contains(&offset) => {
						let model = some!(this_model.inner);
						let name = &contents[range];
						let range = span_conv(capture.node.range());
						return self.index.hover_property_name(name, model, Some(range));
					}
					Some(PyCompletions::FieldDescriptor) => {
						let Some(desc_value) = python_next_named_sibling(capture.node) else {
							continue;
						};
						let descriptor = &contents[range];
						if !desc_value.byte_range().contains_end(offset) {
							continue;
						}

						if matches!(descriptor, "comodel_name") {
							let range = desc_value.byte_range().shrink(1);
							let lsp_range = span_conv(desc_value.range());
							let slice = ok!(rope.try_slice(range.clone()));
							let slice = Cow::from(slice);
							return self.index.hover_model(&slice, Some(lsp_range), false, None);
						} else if matches!(descriptor, "compute" | "search" | "inverse" | "related") {
							let single_field = descriptor != "related";
							let mapped = some!(self.gather_mapped(
								root,
								match_,
								Some(offset),
								desc_value.byte_range(),
								this_model.inner,
								&contents,
								false,
								Some(single_field)
							));
							let mut needle = mapped.needle;
							let mut model = _I(mapped.model);
							let mut range = mapped.range;
							if !mapped.single_field {
								some!(
									self.index
										.models
										.resolve_mapped(&mut model, &mut needle, Some(&mut range))
										.ok()
								);
							}
							let model = _R(model);
							return (self.index).hover_property_name(needle, model, Some(rope_conv(range, rope)));
						} else if matches!(descriptor, "groups") {
							let range = desc_value.byte_range().shrink(1);
							let value = Cow::from(ok!(rope.try_slice(range.clone())));
							let mut ref_ = None;
							determine_csv_xmlid_subgroup(&mut ref_, (&value, range), offset);
							let (needle, byte_range) = some!(ref_);
							return self
								.index
								.hover_record(needle, Some(rope_conv(byte_range.map_unit(ByteOffset), rope)));
						}

						return Ok(None);
					}
					Some(PyCompletions::Request)
					| Some(PyCompletions::XmlId)
					| Some(PyCompletions::ForXmlId)
					| Some(PyCompletions::HasGroups)
					| Some(PyCompletions::MappedTarget)
					| Some(PyCompletions::Depends)
					| Some(PyCompletions::ReadFn)
					| Some(PyCompletions::Scope)
					| Some(PyCompletions::Prop)
					| Some(PyCompletions::FieldType)
					| None => {}
				}
			}
		}
		if let Some((model, prop, range)) = self.attribute_at_offset(offset, root, &contents) {
			let lsp_range = Some(rope_conv(range.map_unit(ByteOffset), rope));
			return self.index.hover_property_name(prop, model, lsp_range);
		}

		// Check if cursor is on an imported identifier
		if let Some(cursor_node) = ast.root_node().descendant_for_byte_range(offset, offset)
			&& cursor_node.kind() == "identifier"
		{
			let identifier = &contents[cursor_node.byte_range()];

			// Parse imports from the current file
			let imports = self.parse_imports(&contents).unwrap_or_default();

			// Try to resolve as an import
			if let Some(import_info) = imports.get(identifier).or_else(|| {
				// Check if identifier matches any imported_name (handles import statements with aliases)
				imports.values().find(|info| info.imported_name == identifier)
			}) {
				debug!(
					"Hovering over imported symbol '{}' from module '{}'",
					identifier, import_info.module_path
				);

				// Resolve the module
				if let Some(file_path) = self
					.index
					.resolve_py_module_from(&import_info.module_path, Some(Path::new(file_path_str)))
				{
					debug!("Resolved module to: {}", file_path.display());

					// Parse the module scope to get symbol information
					if let Some(scope) = self.index.parse_module_scope(&file_path)
					&& let symbol_name = &import_info.imported_name
					// Try to find the symbol in the module
					&& let Some(symbol_type) = scope.get(symbol_name.as_str())
					{
						let type_str = format!("{:?}", symbol_type);

						// Build hover content
						let mut contents_parts = vec![];

						// Add the symbol declaration
						if let Some(alias) = &import_info.alias {
							contents_parts.push(format!("```python\n{} (imported as {})\n```", symbol_name, alias));
						} else {
							contents_parts.push(format!("```python\n{}\n```", symbol_name));
						}

						// Add type information
						contents_parts.push(format!("Type: `{}`", type_str));

						// Add source location
						contents_parts.push(format!("From: `{}`", import_info.module_path));

						let hover_contents = contents_parts.join("\n\n");

						return Ok(Some(Hover {
							contents: HoverContents::Markup(MarkupContent {
								kind: MarkupKind::Markdown,
								value: hover_contents,
							}),
							range: Some(span_conv(cursor_node.range())),
						}));
					}
				}
			}
		}

		// No matches, assume arbitrary expression.
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let lsp_range = span_conv(needle.range());
		let (type_, scope) =
			some!((self.index).type_of_range(root, needle.byte_range().map_unit(ByteOffset), &contents));
		if let Some(model) = self.index.try_resolve_model(&type_cache().resolve(type_), &scope) {
			let model = _R(model);
			let identifier = (needle.kind() == "identifier").then(|| &contents[needle.byte_range()]);
			return self.index.hover_model(model, Some(lsp_range), true, identifier);
		}

		self.index.hover_variable(
			(needle.kind() == "identifier").then(|| &contents[needle.byte_range()]),
			type_,
			Some(lsp_range),
		)
	}

	pub(crate) fn python_signature_help(&self, params: SignatureHelpParams) -> anyhow::Result<Option<SignatureHelp>> {
		use std::fmt::Write;

		let uri = &params.text_document_position_params.text_document.uri;
		let document = some!((self.document_map).get(uri.path().as_str()));
		let file_path = uri.to_file_path().unwrap();
		let ast = some!((self.ast_map).get(file_path.to_str().unwrap()));
		let contents = Cow::from(&document.rope);

		let point = tree_sitter::Point::new(
			params.text_document_position_params.position.line as _,
			params.text_document_position_params.position.character as _,
		);
		let node = some!(ast.root_node().descendant_for_point_range(point, point));
		let mut args = node;
		while let Some(parent) = args.parent() {
			if args.kind() == "argument_list" {
				break;
			}
			args = parent;
		}

		if args.kind() != "argument_list" {
			return Ok(None);
		}

		let active_parameter = 'find_param: {
			let ByteOffset(offset) = rope_conv(params.text_document_position_params.position, document.rope.slice(..));
			if let Some(contents) = contents.get(..=offset)
				&& let Some(idx) = contents.bytes().rposition(|c| c == b',' || c == b'(')
			{
				if contents.as_bytes()[idx] == b'(' {
					break 'find_param Some(0);
				}
				let prev_param = args.descendant_for_byte_range(idx, idx).unwrap().prev_named_sibling();
				for (idx, arg) in args.named_children(&mut args.walk()).enumerate() {
					if Some(arg) == prev_param {
						// the index might be intentionally out of bounds w.r.t the actual number of arguments
						// but this is better than leaving it as None because clients infer it as the first argument
						break 'find_param Some((idx + 1) as u32);
					}
				}
			}

			None
		};

		let callee = some!(args.prev_named_sibling());
		let Some((tid, _)) =
			(self.index).type_of_range(ast.root_node(), callee.byte_range().map_unit(ByteOffset), &contents)
		else {
			return Ok(None);
		};
		let Type::Method(model_key, method) = type_cache().resolve(tid) else {
			return Ok(None);
		};
		let method_key = some!(_G(&method));
		let rtype = (self.index).eval_method_rtype(method_key.into(), model_key.into(), None);
		let model = some!((self.index).models.get(&model_key));
		let method_obj = some!(some!(model.methods.as_ref()).get(&method_key.into()));

		let mut label = format!("{method}(");
		let mut parameters = vec![];

		for (idx, param) in method_obj.arguments.as_deref().unwrap_or(&[]).iter().enumerate() {
			let begin;
			if idx == 0 {
				begin = label.len();
				_ = write!(&mut label, "{param}");
			} else {
				begin = label.len() + 2;
				_ = write!(&mut label, ", {param}");
			}
			let end = label.len();
			parameters.push(ParameterInformation {
				label: ParameterLabel::LabelOffsets([begin as _, end as _]),
				documentation: None,
			});
		}

		let rtype = rtype.and_then(|rtype| self.index.type_display(rtype));
		match rtype {
			Some(rtype) => drop(write!(&mut label, ") -> {rtype}")),
			None => label.push_str(") -> ..."),
		};

		let sig = SignatureInformation {
			label,
			active_parameter,
			parameters: Some(parameters),
			documentation: method_obj.docstring.as_ref().map(|doc| {
				Documentation::MarkupContent(MarkupContent {
					kind: MarkupKind::Markdown,
					value: doc.to_string(),
				})
			}),
		};

		Ok(Some(SignatureHelp {
			signatures: vec![sig],
			active_signature: Some(0),
			active_parameter: None,
		}))
	}

	#[allow(clippy::unused_async)] // reason: custom method
	pub async fn debug_inspect_type(
		&self,
		params: TextDocumentPositionParams,
	) -> tower_lsp_server::jsonrpc::Result<Option<String>> {
		let uri = &params.text_document.uri;
		let document = some!(self.document_map.get(uri.path().as_str()));
		let file_path = uri.to_file_path().unwrap();
		let ast = some!(self.ast_map.get(file_path.to_str().unwrap()));
		let rope = &document.rope;
		let contents = Cow::from(rope);
		let ByteOffset(offset) = rope_conv(params.position, rope.slice(..));
		let root = some!(top_level_stmt(ast.root_node(), offset));
		let needle = some!(root.named_descendant_for_byte_range(offset, offset));
		let (type_, _) = some!((self.index).type_of_range(root, needle.byte_range().map_unit(ByteOffset), &contents));
		Ok(Some(format!("{type_:?}").replacen("Text::", "", 1)))
	}
	fn is_commandlist(cmdlist: Node, offset: usize) -> bool {
		matches!(cmdlist.kind(), "list" | "list_comprehension")
			&& cmdlist.byte_range().contains_end(offset)
			&& cmdlist.parent().is_some_and(|parent| parent.kind() == "pair")
	}
	/// `cmdlist` must have been checked by [is_commandlist][Backend::is_commandlist] first
	///
	/// Returns `(needle, range, model)` for a field
	fn gather_commandlist<'text>(
		&self,
		cmdlist: Node,
		root: Node,
		match_: &tree_sitter::QueryMatch,
		offset: usize,
		range: std::ops::Range<usize>,
		this_model: Option<&'text str>,
		contents: &'text str,
		for_replacing: bool,
	) -> Option<(&'text str, ByteRange, Spur)> {
		let mut access = contents[range.shrink(1)].to_string();
		tracing::debug!(
			"gather_commandlist: cmdlist range: {:?}, offset: {}",
			cmdlist.byte_range(),
			offset
		);
		let mut dest = cmdlist.descendant_for_byte_range(offset, offset);
		tracing::debug!("Initial dest: {:?}", dest.map(|n| (n.kind(), n.byte_range())));

		// If we can't find a node at the exact offset, try to find the last string node
		// This handles the case where cursor is after a string without a colon
		if dest.is_none() && offset > cmdlist.start_byte() {
			tracing::debug!("No node at offset {}, trying offset - 1", offset);
			// Try offset - 1 to see if we're just after a string
			if let Some(node) = cmdlist.descendant_for_byte_range(offset - 1, offset - 1) {
				tracing::debug!(
					"Found node at offset - 1: kind={}, range={:?}",
					node.kind(),
					node.byte_range()
				);
				if node.kind() == "string"
					|| (node.kind() == "string_content" && node.parent().map(|p| p.kind()) == Some("string"))
					|| node.kind() == "string_end"
				{
					// Check if this string is not part of a key-value pair (no colon after it)
					let string_node = if node.kind() == "string" {
						node
					// } else if node.kind() == "string_end" && node.parent().map(|p| p.kind()) == Some("string") {
					// 	node.parent()?
					} else {
						node.parent()?
					};
					if let Some(next_sibling) = string_node.next_sibling() {
						tracing::debug!("String has next sibling: {}", next_sibling.kind());
						if next_sibling.kind() != ":" {
							// This is an incomplete field name, provide completions
							dest = Some(string_node);
						}
					} else {
						tracing::debug!("String has no next sibling, treating as incomplete");
						// No next sibling means it's the last element, likely incomplete
						dest = Some(string_node);
					}
				}
			}
		}

		let mut dest = dest?;

		// If we're inside a string_content node, get the parent string node
		if dest.kind() == "string_content" {
			dest = dest.parent()?;
		}

		if dest.kind() != "string" {
			dest = dest.parent()?;
		}
		if dest.kind() != "string" {
			return None;
		}

		// First check if this string is in a broken syntax situation
		// (i.e., it's a key in a dictionary without a following colon)
		let mut is_broken_syntax = false;
		if let Some(parent) = dest.parent() {
			tracing::debug!("String parent kind: {}", parent.kind());
			if parent.kind() == "dictionary" {
				// Check if this string has a colon after it
				if let Some(next_sibling) = dest.next_sibling() {
					tracing::debug!("String next sibling kind: {}", next_sibling.kind());
					if next_sibling.kind() != ":" {
						is_broken_syntax = true;
					}
				} else {
					tracing::debug!("String has no next sibling");
					// No next sibling means it's the last element, likely incomplete
					is_broken_syntax = true;
				}
			} else if parent.kind() == "ERROR" {
				// When there's broken syntax, tree-sitter creates ERROR nodes
				// Check if the parent of the ERROR is a dictionary
				if let Some(grandparent) = parent.parent() {
					tracing::debug!("ERROR parent (grandparent) kind: {}", grandparent.kind());
					if grandparent.kind() == "dictionary" {
						// This is likely a string in a dictionary with broken syntax
						is_broken_syntax = true;
					}
				}
			}
		}

		if is_broken_syntax {
			tracing::debug!("Detected broken syntax: string in dictionary without colon");
			// For broken syntax, we need to continue processing to determine the model
			// but we'll use an empty needle to show all available fields
		}

		let (needle, model_str, range) = if is_broken_syntax {
			// For broken syntax, we don't want to complete the partial field name
			// We want to show all available fields
			// We still need to get the model context, so we'll use the parent model if available
			let range = ByteRange {
				start: ByteOffset(offset),
				end: ByteOffset(offset),
			};
			// Use the this_model if available, otherwise we'll need to determine it from context
			// For command lists without explicit model, we need to continue processing
			// to determine the model from the field context
			let model = this_model.unwrap_or("");
			("", model, range)
		} else {
			// Normal case - complete the field name
			let Mapped {
				needle, model, range, ..
			} = self.gather_mapped(
				root,
				match_,
				Some(offset),
				dest.byte_range(),
				this_model,
				contents,
				for_replacing,
				None,
			)?;
			(needle, model, range)
		};

		tracing::debug!(
			"needle={}, is_broken_syntax={}, model_str={}",
			needle,
			is_broken_syntax,
			model_str
		);

		// recursive descent to collect the chain of fields
		let mut cursor = cmdlist;
		let mut count = 0;
		while count < 30 {
			count += 1;
			let Some(candidate) = cursor.child_with_descendant(dest) else {
				tracing::debug!("child_containing_descendant returned None at count={}", count);
				return None;
			};
			let obj;
			tracing::debug!("candidate kind: {}", candidate.kind());
			if candidate.kind() == "tuple" {
				// (0, 0, {})
				obj = candidate.child_with_descendant(dest)?;
			} else if candidate.kind() == "call" {
				// Command.create({}), but we don't really care if the actual function is called.
				let args = dig!(candidate, argument_list(1))?;
				obj = args.child_with_descendant(dest)?;
			} else {
				return None;
			}
			tracing::debug!("obj kind: {}", obj.kind());
			if obj.kind() == "dictionary" {
				let pair = obj.child_with_descendant(dest)?;
				tracing::debug!("pair kind: {}", pair.kind());
				if pair.kind() != "pair" {
					// Check if this is a broken syntax case (string without colon)
					if pair.kind() == "string" && pair.byte_range().contains(&offset) {
						// This is a string in a dictionary without a colon
						// We're completing field names for this dictionary
						tracing::debug!("Breaking due to broken syntax string in dictionary");
						// Break out of the loop to resolve the model
						break;
					} else if pair.kind() == "ERROR" {
						// When there's broken syntax, tree-sitter might create an ERROR node
						// Check if the ERROR contains our string
						if pair.byte_range().contains(&offset) {
							tracing::debug!("Breaking due to ERROR node containing offset");
							break;
						}
					}
					tracing::debug!("Returning None: pair kind {} is not 'pair'", pair.kind());
					return None;
				}

				let key = dig!(pair, string)?;
				if key.byte_range().contains_end(offset) {
					break;
				}

				cursor = pair.child_with_descendant(dest)?;
				access.push('.');
				access.push_str(&contents[key.byte_range().shrink(1)]);
			} else if obj.kind() == "set" {
				break;
			} else {
				// TODO: (ERROR) case
				return None;
			}
		}

		if count == 30 {
			warn!("recursion limit hit");
		}

		access.push('.'); // to force resolution of the last field
		tracing::debug!("Access path: {}", access);
		tracing::debug!("Initial model before resolve: {}", model_str);
		let access = &mut access.as_str();
		let mut model = _I(model_str);
		if self.index.models.resolve_mapped(&mut model, access, None).is_err() {
			tracing::debug!("resolve_mapped failed for model={} access={}", _R(model), access);
			return None;
		}
		tracing::debug!("Resolved model: {}", _R(model));

		Some((needle, range, model))
	}
}

#[derive(Default, Clone)]
struct ThisModel<'a> {
	inner: Option<&'a str>,
	source: ThisModelKind,
	top_level_range: core::ops::Range<usize>,
}

#[derive(Default, Clone, Copy)]
enum ThisModelKind {
	Primary,
	#[default]
	Inherited,
}

impl<'this> ThisModel<'this> {
	/// Call this on captures of index [`PyCompletions::Model`].
	fn tag_model(
		&mut self,
		model: Node,
		match_: &QueryMatch,
		top_level_range: core::ops::Range<usize>,
		contents: &'this str,
	) {
		if match_
			.nodes_for_capture_index(PyCompletions::FieldType as _)
			.next()
			.is_some()
		{
			// debug_assert!(false, "tag_model called on a class model; handle this manually");
			return;
		}

		debug_assert_eq!(model.kind(), "string");
		let (is_name, mut is_inherit) = match_
			.nodes_for_capture_index(PyCompletions::Prop as _)
			.next()
			.map(|prop| {
				let prop = &contents[prop.byte_range()];
				(prop == "_name", prop == "_inherit")
			})
			.unwrap_or((false, false));
		let top_level_changed = top_level_range != self.top_level_range;
		// If still in same class AND _name already declared, skip.
		is_inherit = is_inherit && (top_level_changed || matches!(self.source, ThisModelKind::Inherited));
		if is_inherit {
			let parent = model.parent().expect(format_loc!("(tag_model) parent"));
			// _inherit = '..' OR _inherit = ['..']
			is_inherit = parent.kind() == "assignment" || parent.kind() == "list" && parent.named_child_count() == 1;
		}
		if is_inherit || is_name && top_level_changed {
			self.inner = Some(&contents[model.byte_range().shrink(1)]);
			self.top_level_range = top_level_range;
			if is_name {
				self.source = ThisModelKind::Primary;
			} else if is_inherit {
				self.source = ThisModelKind::Inherited;
			}
		}
	}
}

fn extract_string_needle_at_offset<'a>(
	rope: RopeSlice<'a>,
	range: core::ops::Range<usize>,
	offset: usize,
) -> anyhow::Result<(Cow<'a, str>, core::ops::Range<ByteOffset>)> {
	let slice = rope.try_slice(range.clone())?;
	let relative_offset = range.start;
	let needle = Cow::from(slice.try_slice(1..offset - relative_offset)?);
	let byte_range = range.shrink(1).map_unit(ByteOffset);
	Ok((needle, byte_range))
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_find_symbol_definition_basic() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();

		// Test case 1: function definition
		let contents = "def hello(): pass";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "hello");
		assert!(result.is_some(), "Should find function 'hello'");

		let node = result.unwrap();
		assert_eq!(
			&contents[node.byte_range()],
			"hello",
			"Should return the 'hello' function name node"
		);

		// Test case 2: class definition
		let contents = "class MyModel: pass";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "MyModel");
		assert!(result.is_some(), "Should find class 'MyModel'");

		let node = result.unwrap();
		assert_eq!(
			&contents[node.byte_range()],
			"MyModel",
			"Should return the 'MyModel' class name node"
		);

		// Test case 3: variable assignment
		let contents = "fields = 42";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "fields");
		assert!(result.is_some(), "Should find variable 'fields'");

		let node = result.unwrap();
		assert_eq!(
			&contents[node.byte_range()],
			"fields",
			"Should return the 'fields' variable name node"
		);

		// Test case 4: Should not find non-existent symbol
		let contents = "def hello(): pass";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "nonexistent");
		assert!(result.is_none(), "Should not find 'nonexistent'");
	}

	#[test]
	fn test_find_symbol_definition_single_dot_reexports() {
		let mut parser = Parser::new();
		parser.set_language(&tree_sitter_python::LANGUAGE.into()).unwrap();

		// Test case 1: from . import fields (single dot re-export)
		let contents = "from . import fields";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "fields");
		assert!(result.is_some(), "Should find 'fields' in 'from . import fields'");

		let node = result.unwrap();
		assert_eq!(
			&contents[node.byte_range()],
			"fields",
			"Should return the 'fields' identifier node"
		);

		// Test case 2: Absolute imports should NOT be found by this function
		// They should be resolved by other mechanisms (module resolution)
		let contents = "from odoo import api";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "api");
		assert!(
			result.is_none(),
			"Absolute imports should not be found by find_symbol_definition"
		);

		// Test case 3: from . import fields as f (aliased single dot re-export)
		let contents = "from . import fields as f";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		let result = find_symbol_definition(root, contents, "f");
		assert!(
			result.is_some(),
			"Should find 'f' (the alias) in 'from . import fields as f'"
		);

		let node = result.unwrap();
		assert_eq!(
			&contents[node.byte_range()],
			"f",
			"Should return the alias 'f' identifier node"
		);

		// Test case 4: Multiple imports with single dot relative import
		let contents = "from . import fields, models, api";
		let tree = parser.parse(contents, None).unwrap();
		let root = tree.root_node();

		// Find fields
		let result = find_symbol_definition(root, contents, "fields");
		assert!(result.is_some(), "Should find 'fields' in multi-import");

		// Find models
		let result = find_symbol_definition(root, contents, "models");
		assert!(result.is_some(), "Should find 'models' in multi-import");

		// Find api
		let result = find_symbol_definition(root, contents, "api");
		assert!(result.is_some(), "Should find 'api' in multi-import");
	}
}
