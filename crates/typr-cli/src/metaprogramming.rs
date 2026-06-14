//! Metaprogramming utilities for TypR CLI
//!
//! Handles module imports and file expansion.

use crate::io::get_os_file;
use nom_locate::LocatedSpan;
use std::path::Path;
use typr_core::components::context::config::Environment;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;

fn import_file_module_code(line: &Lang, environment: Environment) -> Lang {
    match line {
        Lang::ModuleImport {
            value: name,
            help_data: h,
        } => {
            // Resolve the module file path relative to the importing file's directory.
            // For Project mode the convention is TypR/{name}.ty; for StandAlone/Repl
            // we look next to the file that contains the `mod` statement.
            let module_path = match environment {
                Environment::Project => format!("TypR/{}.ty", name),
                _ => {
                    let importing_file = h.get_file_name();
                    let base = Path::new(&importing_file)
                        .parent()
                        .and_then(|p| p.to_str())
                        .unwrap_or("");
                    if base.is_empty() {
                        format!("{}.ty", name)
                    } else {
                        format!("{}/{}.ty", base, name)
                    }
                }
            };
            let file = get_os_file(&module_path);
            let content = std::fs::read_to_string(&module_path)
                .unwrap_or_else(|_| panic!("Can't read module file '{}'", module_path));
            let parse_result = parse(LocatedSpan::new_extra(&content, file));
            // TODO: propagate errors from imported modules
            metaprogrammation(parse_result.ast.to_module(name, environment), environment)
        }
        n => n.clone(),
    }
}

fn import_file_modules_code(adt: Lang, environment: Environment) -> Lang {
    match adt {
        Lang::Module {
            name,
            body: lines,
            module_position: position,
            config,
            help_data: h,
        } => {
            let new_lines = lines
                .iter()
                .map(|x| import_file_module_code(x, environment))
                .collect::<Vec<_>>();
            Lang::Module {
                name,
                body: new_lines,
                module_position: position,
                config,
                help_data: h,
            }
        }
        Lang::Lines {
            value: lines,
            help_data: h,
        } => {
            let new_lines = lines
                .iter()
                .map(|x| import_file_module_code(x, environment))
                .collect::<Vec<_>>();
            Lang::Lines {
                value: new_lines,
                help_data: h,
            }
        }
        s => s,
    }
}

pub fn metaprogrammation(adt: Lang, environment: Environment) -> Lang {
    import_file_modules_code(adt, environment)
}
