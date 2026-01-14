use crate::components::context::config::Environment;
use crate::components::lang::language::Lang;
use crate::my_io::read_file_from_name;
use crate::my_io::get_os_file;
use crate::parse;
use nom_locate::LocatedSpan;

fn import_file_module_code(line: &Lang, environment: Environment) -> Lang {
    match line {
        Lang::ModuleImport(name, _h) => {
            let file = get_os_file(&format!("{}.ty", name));
            metaprogrammation(
                parse(LocatedSpan::new_extra(&read_file_from_name(&name, environment), file))
                .to_module(name, environment),
                environment)
        }
        n => n.clone()
    }
}

fn import_file_modules_code(adt: Lang, environment: Environment) -> Lang {
    match adt {
        Lang::Module(name, lines, position, config, h) => {
            let new_lines = lines.iter()
                .map(|x| import_file_module_code(x, environment))
                .collect::<Vec<_>>();
            Lang::Module(name, new_lines, position, config, h)
        }
        Lang::Lines(lines, h) => {
            let new_lines = lines.iter()
                .map(|x| import_file_module_code(x, environment))
                .collect::<Vec<_>>();
            Lang::Lines(new_lines, h)
        },
        s =>  s 
    }
}

pub fn metaprogrammation(adt: Lang, environment: Environment) -> Lang {
    import_file_modules_code(adt, environment)
}
