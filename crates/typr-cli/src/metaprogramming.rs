//! Metaprogramming utilities for TypR CLI
//!
//! Handles module imports and file expansion.

use crate::io::{get_os_file, read_file_from_name};
use nom_locate::LocatedSpan;
use typr_core::components::context::config::Environment;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;

fn import_file_module_code(line: &Lang, environment: Environment) -> Lang {
    match line {
        Lang::ModuleImport(name, _h) => {
            let file = get_os_file(&format!("{}.ty", name));
            let parse_result = parse(LocatedSpan::new_extra(
                &read_file_from_name(&name, environment),
                file,
            ));
            // TODO: propagate errors from imported modules
            metaprogrammation(parse_result.ast.to_module(name, environment), environment)
        }
        n => n.clone(),
    }
}

fn import_file_modules_code(adt: Lang, environment: Environment) -> Lang {
    match adt {
        Lang::Module(name, lines, position, config, h) => {
            let new_lines = lines
                .iter()
                .map(|x| import_file_module_code(x, environment))
                .collect::<Vec<_>>();
            Lang::Module(name, new_lines, position, config, h)
        }
        Lang::Lines(lines, h) => {
            let new_lines = lines
                .iter()
                .map(|x| import_file_module_code(x, environment))
                .collect::<Vec<_>>();
            Lang::Lines(new_lines, h)
        }
        s => s,
    }
}

pub fn metaprogrammation(adt: Lang, environment: Environment) -> Lang {
    import_file_modules_code(adt, environment)
}
