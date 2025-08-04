use crate::Adt;
use crate::Context;
use std::fs::File;
use crate::typing;
use crate::Lang;
use crate::type_printer;
use crate::AdtManager;
use crate::parse;
use crate::read_file;
use crate::metaprogrammation;
use std::io::Write;
use std::path::PathBuf;
use nom_locate::LocatedSpan;
use crate::my_io::get_os_file;
use crate::help_data::HelpData;
use crate::context::CompileMode;

type Span<'a> = LocatedSpan<&'a str, String>;

//pub fn write_adt_to_typescript_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf) -> () {
    //let rstd = include_str!("../configs/typescript/std.ts");
    //let std_path = output_dir.join("std.ts");
    //let mut rstd_file = File::create(std_path).unwrap();
    //rstd_file.write_all(rstd.as_bytes()).unwrap();
//
    //let app_path = output_dir.join("main.ts");
    //let mut app = File::create(app_path).unwrap();
    //let ts_import = include_str!("../configs/typescript/ts_import.ts");
    //let content = format!("{}\n{}\n{}", ts_import, cont.adt.get_adt().to_typescript(cont), adt.to_typescript(cont));
    //app.write_all(content.as_bytes()).unwrap();
//}


//pub fn write_adt_to_assemblyscript_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf) -> () {
    //let rstd = include_str!("../configs/typescript/std.ts");
    //let std_path = output_dir.join("std.ts");
    //let mut rstd_file = File::create(std_path).unwrap();
    //rstd_file.write_all(rstd.as_bytes()).unwrap();
//
    //let app_path = output_dir.join("main.ts");
    //let mut app = File::create(app_path).unwrap();
    //let ts_import = include_str!("../configs/typescript/ts_import.ts");
    //let content = format!("{}\n{}\n{}", ts_import, cont.adt.get_adt().to_assemblyscript(cont), adt.to_assemblyscript(cont));
    //app.write_all(content.as_bytes()).unwrap();
//}

pub fn write_std_for_type_checking(output_dir: &PathBuf) {
    let rstd = include_str!("../configs/r/std.ty");
    let std_path = output_dir.join("std.ty");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();
}

pub fn type_check(adtm: &AdtManager) -> Context {
    let base_context = Context::default()
        //.set_target(Target)
        .set_compile_mode(CompileMode::Header);
    let context = typing(&base_context, &Lang::Sequence(adtm.get_header().0.clone(), HelpData::default())).1.set_compile_mode(CompileMode::Body);
    let (typ, new_context) = typing(&context, &Lang::Sequence(adtm.get_body().0.clone(), HelpData::default()));
    type_printer::pretty_print(&typ);
    new_context
}

struct TypRFile<'a> {
    content: &'a str,
    name: String
}

impl<'a> TypRFile<'a> {
    fn new(content: &str, name: String) -> TypRFile {
        TypRFile {
            content: content,
            name: name
        }
    }

    fn parse(self) -> Adt  {
        parse(LocatedSpan::new_extra(self.content, self.name)).unwrap().1
    }
}


//1. 
pub fn parse_code(path: &PathBuf) -> AdtManager {
    let typr_std = include_str!("../configs/r/std.ty");
    let file = get_os_file(path.to_str().unwrap());
    let file_content = read_file(path);
    let std_file = TypRFile::new(typr_std, "std.ty".to_string());
    let base_file = TypRFile::new(&file_content, file);

    let adt_manager = AdtManager::new()
        .add_to_header(std_file.parse())
        .add_to_body(base_file.parse());

    let adt = metaprogrammation(adt_manager.body.clone());
    adt_manager.set_body(adt)
}
