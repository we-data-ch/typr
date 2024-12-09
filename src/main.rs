mod language;
mod elements;
mod parser;
mod types;
mod operators;
mod my_io;
mod var;
mod metaprogramming;

use parser::parse;
use my_io::{read_file, write_adt, recreate_files, delete_files};
use crate::types::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use std::fs::File;
use std::io::Write;


fn main() {
    recreate_files();
    let adt = parse(&read_file()).unwrap().1;
    write_adt(&metaprogrammation(adt.clone()).to_string());
    let serialized = serde_json::to_string(&adt).unwrap();
    let mut file = File::create("adt.json").unwrap();
    file.write_all(serialized.as_bytes()).unwrap();
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
