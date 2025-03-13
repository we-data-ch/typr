use std::process::Command;
use std::env;
use std::fs;


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

pub fn execute_r() -> () {
    println!("Execution: ");
    let output = Command::new("Rscript")
        .arg(get_os_file("app.R"))
        .output()
        .expect("Échec lors de l'exécution de la commande");

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
}
