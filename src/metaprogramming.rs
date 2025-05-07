use crate::my_io::read_file_from_name;
use crate::my_io::get_os_file;
use crate::adt::Adt;
use crate::Lang;
use crate::parse;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;


fn import_file_module_code(line: &Lang) -> Lang {
    match line {
        Lang::ModImp(name) => {
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

fn private_public_change(module_name: &str, adt: Adt) -> Vec<Lang> {
    adt.0.iter().map(|line| {
        match line {
            Lang::Let(var, typ, body, h) 
                => Lang::Let(
                    var.clone().add_path(module_name),
                    typ.clone(), body.clone(), h.clone()),
            Lang::Alias(var, params, typ, h) 
                => Lang::Alias(
                    var.clone().add_path(module_name),
                    params.clone(), typ.clone(), h.clone()),
            _ => Lang::Empty(line.clone().into())
        }
    }).collect::<Vec<_>>()
}

fn unnest_module(line: &Lang) -> Vec<Lang> {
    match line {
        Lang::Module(name, body, h) 
            => {
                let new_adt = unnest_modules(body.clone().into());
                let mut lines = private_public_change(name, new_adt);
                lines.insert(0, Lang::ModuleDecl(name.to_string(), h.clone()));
                lines
            },
        lang => vec![lang.clone()]
    }
}

fn unnest_modules(adt: Adt) -> Adt {
    adt.iter().flat_map(unnest_module).collect::<Vec<_>>().into()
}

pub fn metaprogrammation(adt: Adt) -> Adt {
    //type_embedding(import_types(import_modules(adt)))
    unnest_modules(import_file_modules_code(adt))
}
