use crate::Adt;
use crate::Context;
use std::fs::File;
use crate::execute_r;
use crate::typing;
use crate::Lang;
use crate::type_printer;
use crate::AdtManager;
use crate::parse;
use crate::read_file;
use crate::metaprogrammation;
use std::io::Write;
use std::path::PathBuf;

pub fn write_adt_to_r(adt: &Adt, cont: &Context) -> () {
    let rstd = include_str!("../configs/std.R");
    let mut rstd_file = File::create("std.R").unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let mut app = File::create("app.R").unwrap();
    let content = format!("source('std.R', echo = FALSE)\n{}\n{}", Adt(cont.adt.clone()).to_r(cont), adt.to_r(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn write_adt_to_r_with_path(adt: &Adt, cont: &Context, output_dir: &PathBuf) -> () {
    let rstd = include_str!("../configs/std.R");
    let std_path = output_dir.join("std.R");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join("main.R");
    let mut app = File::create(app_path).unwrap();
    let content = format!("source('std.R', echo = FALSE)\n{}\n{}", Adt(cont.adt.clone()).to_r(cont), adt.to_r(cont));
    app.write_all(content.as_bytes()).unwrap();
}

pub fn execute(adt: &Adt, cont: &Context) -> () {
    write_adt_to_r(&adt, cont);
    execute_r();
}

pub fn type_check(adt: &Adt) -> Context {
    let (typ, context) = typing(&Context::default(), &Lang::Sequence(adt.0.clone()));
    type_printer::pretty_print(&typ);
    context
}

pub fn parse_code(path: &PathBuf) -> AdtManager {
    let typr_std = include_str!("../configs/std.ty");
    let adt_manager = AdtManager::new()
        .add_to_body(parse(&read_file(path)).unwrap().1)
        .add_to_header(parse(typr_std).unwrap().1);
    let adt = metaprogrammation(adt_manager.body.clone());
    adt_manager.set_body(adt)
}

pub fn run_file(path: &PathBuf, execution_path: &PathBuf) -> () {
    let original_dir = std::env::current_dir().expect("Impossible d'obtenir le répertoire de travail actuel");
    std::env::set_current_dir(execution_path).expect("Échec lors du changement de répertoire");

    let adt_manager = parse_code(path);
    let context = type_check(&adt_manager.get_adt_without_header());
    execute(&adt_manager.get_adt_without_header(), &context);

    std::env::set_current_dir(original_dir).expect("Échec lors de la restauration du répertoire de travail");
}
