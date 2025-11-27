use crate::my_io::read_file_from_name;
use crate::my_io::get_os_file;
use crate::adt::Adt;
use crate::Lang;
use crate::parse;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;


fn import_file_module_code(line: &Lang) -> Lang {
    match line {
        Lang::ModuleImport(name, _h) => {
            let file = get_os_file(&format!("{}.ty", name));
            let new_adt = metaprogrammation(parse(LocatedSpan::new_extra(&read_file_from_name(&name), file)).unwrap().1);
            Lang::Module(name.to_string(), new_adt.0, HelpData::default())
        }
        n => n.clone()
    }
}

fn import_file_modules_code(adt: Adt) -> Adt {
    adt.iter().map(import_file_module_code).collect::<Vec<_>>().into()
}

pub fn metaprogrammation(adt: Adt) -> Adt {
    import_file_modules_code(adt)
}
