use crate::my_io::read_file_from_name;
use crate::my_io::get_os_file;
use crate::Lang;
use crate::parse;
use nom_locate::LocatedSpan;


fn import_file_module_code(line: &Lang) -> Lang {
    match line {
        Lang::ModuleImport(name, _h) => {
            let file = get_os_file(&format!("{}.ty", name));
            metaprogrammation(
                parse(LocatedSpan::new_extra(&read_file_from_name(&name), file))
                .to_module(name))
        }
        n => n.clone()
    }
}

fn import_file_modules_code(adt: Lang) -> Lang {
    match adt {
        Lang::Module(name, lines, h) => {
            let new_lines = lines.iter().map(import_file_module_code).collect::<Vec<_>>();
            Lang::Module(name, new_lines, h)
        }
        _ => panic!("This is not a chaining of lines")
    }
    
}

pub fn metaprogrammation(adt: Lang) -> Lang {
    import_file_modules_code(adt)
}
