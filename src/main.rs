mod language;
mod elements;
mod parser;
mod types;
mod operators;
mod my_io;
mod var;
mod metaprogramming;
mod argument_type;
mod argument_value;
mod argument_kind;

use parser::parse;
use my_io::{read_file, write_adt, recreate_files, delete_files, execute};
use crate::types::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use std::fs::File;
use std::io::Write;


fn main() {
    recreate_files();

    let adt = parse(&read_file()).unwrap().1;
    let adt = metaprogrammation(adt.clone());
    write_adt(&adt.to_string());

    let rstd = include_str!("../configs/std.R");

    let mut app = File::create("app.R").unwrap();
    let content = format!("{}\n\n{}", rstd, adt.to_r());
    app.write_all(content.as_bytes()).unwrap();
    
    execute();

    delete_files();
}

#[cfg(test)]
mod tests {
    use crate::elements::parse_elements;

    #[test]
    fn test1(){
        let text = "true and false and true and true;";
        let value = parse_elements(text).unwrap().1;
        assert_eq!(
            value.to_string(),
            "and(true, and(false, and(true, true)))");
    }

    #[test]
    fn test2() {
        let text = "true or false;";
        let value = parse_elements(text).unwrap().1;
        assert_eq!(
            value.to_string(), 
            "or(true, false)");
    }
}
