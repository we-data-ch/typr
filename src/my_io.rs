use std::process::Command;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use crate::context_manager::parse_prolog;
use crate::context::Context;
use crate::Adt;


fn delete_file(file_path: &str) {
    let binding = get_os_file(file_path);
    let path = Path::new(&binding);
    let _ = fs::remove_file(path);
}

fn get_os_file(file: &str) -> String {
    if cfg!(windows){
       file.replace("\\", r"\") 
    } else {
       file.to_string()
    }
}

pub fn read_file() -> String {
    let args: Vec<String> = env::args().collect();
    let file = get_os_file(&args[1]);
    fs::read_to_string(&file).unwrap()
}

pub fn read_file_from_name(name: &str) -> String {
    let file = get_os_file(&format!("{}.ty", name));
    fs::read_to_string(&file).expect(&format!("Can't Read file {}", name))
}

pub fn type_check_prolog() {
    println!("Type checking: ");
    let output = Command::new("swipl")
        .arg("-s")
        .arg(get_os_file("adt.pl"))
        .arg("-g")
        .arg("main")
        .arg("-t")
        .arg("halt")
        .output()
        .expect("Échec lors de l'exécution de la commande");

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    println!("");
}

pub fn execute_r() -> () {
    println!("Execution: ");
    let output = Command::new("Rscript")
        .arg(get_os_file("app.R"))
        .output()
        .expect("Échec lors de l'exécution de la commande");

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
}


pub fn write_adt_to_prolog(adt: &Adt) {
    let s = adt.to_string();
    let mut file = File::create(get_os_file("adt.pl")).unwrap();
    let import1 = ":- use_module(type_checker).\n";
    let import2 = ":- use_module(type_printer).\n\n";
    let function = "type_and_context(Seq, Type, Context) :- typing(context([], []), Seq, Type), eval(context([], []), Seq, Context).\n\n";
    let intro = "main :-\n";
    let begin_typing = "type_and_context(";
    let stds = include_str!("../configs/std.pl");
    let new_s = s.replacen("sequence([", &("sequence([\n".to_string() + stds + ","), 1);
    let end_typing = ", T, context(Kinds, Types)),\n";
    let printt = "type_printer:pretty_print(T),\n";
    let write = "type_printer:write_structure(Types).\n";
    file.write_all(format!("{}{}{}{}{}{}{}{}{}",
                           import1, import2, function, intro, begin_typing, new_s, end_typing, printt, write)
                   .as_bytes()).unwrap();
}

pub fn get_context() -> Context {
    let file = get_os_file("context.txt");
    let context_txt = fs::read_to_string(&file).unwrap();
    parse_prolog(&context_txt)
}
