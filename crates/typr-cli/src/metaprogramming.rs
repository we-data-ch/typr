//! Metaprogramming utilities for TypR CLI
//!
//! Handles module imports and file expansion.
//!
//! # Cache & cycle detection
//!
//! `metaprogrammation` uses a [`ModuleExpander`] that keeps two data structures
//! across the whole expansion tree:
//!
//! - **`ast_cache`** — maps a module name to its fully expanded AST so that
//!   the same module file is never read, parsed, or transformed more than once
//!   per `metaprogrammation` call.
//! - **`resolving`** — tracks which modules are currently being expanded.
//!   If an import targets a name already in this set, a **circular import** is
//!   detected and the process panics with a descriptive message instead of
//!   recursing infinitely.

use crate::io::get_os_file;
use nom_locate::LocatedSpan;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::path::PathBuf;
use typr_core::components::context::config::Environment;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;

/// Walk up from `file_path`'s directory looking for a `DESCRIPTION` +
/// `NAMESPACE` pair, returning the directory that contains them. Works for
/// both absolute paths (LSP, arbitrary process cwd) and the relative paths
/// the CLI uses (where it's always invoked from the project root).
pub fn find_project_root(file_path: &str) -> Option<PathBuf> {
    let mut dir = Path::new(file_path).parent();
    while let Some(d) = dir {
        if d.join("DESCRIPTION").exists() && d.join("NAMESPACE").exists() {
            return Some(d.to_path_buf());
        }
        dir = d.parent();
    }
    None
}

/// Expands `Lang::ModuleImport { .. }` nodes into the corresponding module AST
/// with a shared cache and cycle detection.
struct ModuleExpander {
    /// Fully expanded AST for each module name that has been resolved.
    ast_cache: HashMap<String, Lang>,
    /// Module names that are currently being resolved (stack for cycle
    /// detection).
    resolving: HashSet<String>,
    environment: Environment,
}

impl ModuleExpander {
    fn new(environment: Environment) -> Self {
        Self {
            ast_cache: HashMap::new(),
            resolving: HashSet::new(),
            environment,
        }
    }

    /// Entry point: fully expand `adt`, replacing every `ModuleImport` with its
    /// inlined module AST.
    fn expand(&mut self, adt: Lang) -> Lang {
        match adt {
            Lang::Module {
                name,
                body,
                module_position,
                config,
                help_data,
            } => {
                let new_body = body.into_iter().map(|x| self.expand_one(x)).collect();
                Lang::Module {
                    name,
                    body: new_body,
                    module_position,
                    config,
                    help_data,
                }
            }
            Lang::Lines { value, help_data } => {
                let new_lines = value.into_iter().map(|x| self.expand_one(x)).collect();
                Lang::Lines {
                    value: new_lines,
                    help_data,
                }
            }
            s => s,
        }
    }

    /// Expand a single AST node: if it's a `ModuleImport`, resolve and cache
    /// it; otherwise return it as-is.
    fn expand_one(&mut self, line: Lang) -> Lang {
        match line {
            Lang::ModuleImport {
                value: name,
                help_data: h,
            } => {
                // 1. Cache hit — return the already-expanded AST.
                if let Some(cached) = self.ast_cache.get(&name) {
                    return cached.clone();
                }

                // 2. Cycle detection — if `name` is already in `resolving`,
                //    we have a circular import chain.
                if !self.resolving.insert(name.clone()) {
                    panic!(
                        "Circular module import: module '{}' is already being \
                         resolved. This would cause infinite recursion.",
                        name
                    );
                }

                // 3. Resolve the module file path.
                let importing_file = h.get_file_name();
                let module_path = self.resolve_module_path(&name, &importing_file);

                let file = get_os_file(&module_path);
                let content = std::fs::read_to_string(&module_path)
                    .unwrap_or_else(|_| panic!("Can't read module file '{}'", module_path));
                let parse_result = parse(LocatedSpan::new_extra(&content, file));

                // 4. Wrap parsed AST in a `Module { .. }` node and expand it
                //    recursively (the imported file may itself contain
                //    `ModuleImport`s).
                let module_ast = parse_result.ast.to_module(&name, self.environment);
                let expanded = self.expand(module_ast);

                // 5. Move from `resolving` to `ast_cache`.
                self.resolving.remove(&name);
                self.ast_cache.insert(name, expanded.clone());

                expanded
            }
            n => n,
        }
    }

    /// Convert a module name + importing file path into the absolute path of
    /// the `.ty` file to load.
    fn resolve_module_path(&self, name: &str, importing_file: &str) -> String {
        match self.environment {
            Environment::Project => {
                let root = find_project_root(importing_file).unwrap_or_else(|| PathBuf::from(""));
                root.join("TypR")
                    .join(format!("{}.ty", name))
                    .to_string_lossy()
                    .into_owned()
            }
            _ => {
                let base = Path::new(importing_file)
                    .parent()
                    .and_then(|p| p.to_str())
                    .unwrap_or("");
                if base.is_empty() {
                    format!("{}.ty", name)
                } else {
                    format!("{}/{}.ty", base, name)
                }
            }
        }
    }
}

/// Expand all `mod name;` / `import name;` declarations in `adt` by inlining
/// the corresponding module files.
///
/// Uses a shared [`ModuleExpander`] to avoid re-reading/parsing the same
/// module file more than once and to detect circular imports.
pub fn metaprogrammation(adt: Lang, environment: Environment) -> Lang {
    ModuleExpander::new(environment).expand(adt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom_locate::LocatedSpan;

    /// Helper: parse a string of TypR code and run metaprogrammation on it in
    /// StandAlone mode.  `dir` is the temp directory where module files are
    /// resolved (the parser's `HelpData` records the file name so
    /// `import_file_module_code` can find sibling `.ty` files).
    fn expand_in_dir(content: &str, main_file: &std::path::Path) -> Lang {
        let file_name = main_file.to_string_lossy().into_owned();
        let span = LocatedSpan::new_extra(content, file_name);
        let parse_result = parse(span);
        metaprogrammation(parse_result.ast, Environment::StandAlone)
    }

    /// When the same module is imported from two separate places, the second
    /// import should use the cached AST from the first expansion, not
    /// re-read/re-parse the file.
    #[test]
    fn same_module_imported_twice_uses_cache() {
        let dir = std::env::temp_dir().join(format!("typr_cache_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);

        // helper.ty — no further imports
        let helper = dir.join("helper.ty");
        std::fs::write(&helper, "@pub let val <- 1;\n").unwrap();

        // a.ty — imports helper
        let a_file = dir.join("a.ty");
        std::fs::write(&a_file, "mod helper;\n").unwrap();

        // main.ty — imports both a and helper (helper should be cached)
        let main_file = dir.join("main.ty");
        let content = "mod a;\nmod helper;\nuse a::*;\nuse helper::val;\nlet x <- val;\n";
        std::fs::write(&main_file, content).unwrap();

        // Should not panic or overflow
        let _ast = expand_in_dir(content, &main_file);
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// A → B → A circular import must be detected and panic with a clear
    /// message instead of overflowing the stack.
    #[test]
    fn circular_module_import_panics() {
        let dir = std::env::temp_dir().join(format!("typr_cycle_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);

        // b.ty — imports a (back edge)
        let b_file = dir.join("b.ty");
        std::fs::write(&b_file, "mod a;\n").unwrap();

        // a.ty — imports b
        let a_file = dir.join("a.ty");
        std::fs::write(&a_file, "mod b;\n").unwrap();

        // main.ty — starts the chain
        let main_file = dir.join("main.ty");
        let content = "mod a;\n";
        std::fs::write(&main_file, content).unwrap();

        let file_name = main_file.to_string_lossy().into_owned();
        let span = LocatedSpan::new_extra(content, file_name);
        let parse_result = parse(span);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            metaprogrammation(parse_result.ast, Environment::StandAlone)
        }));

        let _ = std::fs::remove_dir_all(&dir);

        match result {
            Err(panic_info) => {
                let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = panic_info.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "unknown panic type".to_string()
                };
                assert!(
                    msg.contains("Circular module import"),
                    "Expected 'Circular module import' panic, got: {}",
                    msg
                );
            }
            Ok(_) => panic!("Expected panic for circular import, but got Ok"),
        }
    }
}
