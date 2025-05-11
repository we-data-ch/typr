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
use crate::help_data::HelpData;
use crate::context::CompileMode;

pub fn write_adt_to_typescript(adt: &Adt, cont: &Context) -> () {
    let rstd = include_str!("../configs/typescript/std.ts");
    let mut rstd_file = File::create("std.ts").unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let mut app = File::create("app.ts").unwrap();
    let ts_import = include_str!("../configs/typescript/ts_import.ts");
    let content = format!("{}\n{}\n{}", ts_import, cont.adt.get_adt().to_typescript(cont), adt.to_typescript(cont));
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
    let content = format!("{}\n{}\n{}", ts_import, cont.adt.get_adt().to_typescript(cont), adt.to_typescript(cont));
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
    let content = format!("{}\n{}\n{}", ts_import, cont.adt.get_adt().to_assemblyscript(cont), adt.to_assemblyscript(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn write_adt_to_r_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf, file_name: &str) -> () {
    let rstd = include_str!("../configs/r/std.R");
    let std_path = output_dir.join("std.R");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join(file_name);
    let mut app = File::create(app_path).unwrap();
    let content = format!("source('std.R', echo = FALSE)\n{}\n{}", cont.adt.get_adt().to_r(cont), adt.to_r(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn type_check(adtm: &AdtManager, target: TargetLanguage, _environ: Environment) -> Context {
    let base_context = Context::default()
        .set_target(target)
        .set_compile_mode(CompileMode::Header);
    let context = typing(&base_context, &Lang::Sequence(adtm.get_header().0.clone(), HelpData::default())).1.set_compile_mode(CompileMode::Body);
    let (typ, new_context) = typing(&context, &Lang::Sequence(adtm.get_body().0.clone(), HelpData::default()));
    type_printer::pretty_print(&typ);
    new_context
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
