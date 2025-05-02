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
use crate::TargetLanguage;
use crate::Environment;
use nom_locate::LocatedSpan;
use crate::my_io::get_os_file;

pub fn write_adt_to_typescript(adt: &Adt, cont: &Context) -> () {
    let rstd = include_str!("../configs/typescript/std.ts");
    let mut rstd_file = File::create("std.ts").unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let mut app = File::create("app.ts").unwrap();
    let ts_import = include_str!("../configs/typescript/ts_import.ts");
    let content = format!("{}\n{}\n{}", ts_import, Adt(cont.adt.clone()).to_typescript(cont), adt.to_typescript(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn write_adt_to_typescript_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf) -> () {
    let rstd = include_str!("../configs/typescript/std.ts");
    let std_path = output_dir.join("std.ts");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join("main.ts");
    let mut app = File::create(app_path).unwrap();
    let ts_import = include_str!("../configs/typescript/ts_import.ts");
    let content = format!("{}\n{}\n{}", ts_import, Adt(cont.adt.clone()).to_typescript(cont), adt.to_typescript(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn write_adt_to_assemblyscript_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf) -> () {
    let rstd = include_str!("../configs/typescript/std.ts");
    let std_path = output_dir.join("std.ts");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join("main.ts");
    let mut app = File::create(app_path).unwrap();
    let ts_import = include_str!("../configs/typescript/ts_import.ts");
    let content = format!("{}\n{}\n{}", ts_import, Adt(cont.adt.clone()).to_assemblyscript(cont), adt.to_assemblyscript(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn write_adt_to_r_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf, file_name: &str) -> () {
    let rstd = include_str!("../configs/r/std.R");
    let std_path = output_dir.join("std.R");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join(file_name);
    let mut app = File::create(app_path).unwrap();
    let content = format!("source('std.R', echo = FALSE)\n{}\n{}", Adt(cont.adt.clone()).to_r(cont), adt.to_r(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn type_check(adt: &Adt, target: TargetLanguage, environ: Environment) -> Context {
    let (typ, context) = typing(&Context::default().set_target(target), &Lang::Sequence(adt.0.clone()));
    type_printer::pretty_print(&typ);
    context
}

//1. 
pub fn parse_code(path: &PathBuf, target: TargetLanguage) -> AdtManager {
    let typr_std = match target {
        TargetLanguage::R => include_str!("../configs/r/std.ty"),
        TargetLanguage::TypeScript => include_str!("../configs/typescript/std.ty"),
        TargetLanguage::AssemblyScript => include_str!("../configs/assemblyscript/std.ty")
    };
    let file = get_os_file(path.to_str().unwrap());
    let adt_manager = AdtManager::new()
        .add_to_header(parse(LocatedSpan::new_extra(typr_std, "std.ty".to_string())).unwrap().1)
        .add_to_body(parse(LocatedSpan::new_extra(&read_file(path), file)).unwrap().1);
    let adt = metaprogrammation(adt_manager.body.clone());
    adt_manager.set_body(adt)
}
