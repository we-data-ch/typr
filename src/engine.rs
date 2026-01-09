use crate::Lang;
use crate::parse;
use std::fs::File;
use crate::read_file;
use crate::metaprogrammation;
use std::io::Write;
use std::path::PathBuf;
use nom_locate::LocatedSpan;
use crate::my_io::get_os_file;
use crate::Environment;


pub fn write_std_for_type_checking(output_dir: &PathBuf) {
    let rstd = include_str!("../configs/std/std_R.ty");
    let std_path = output_dir.join("std.ty");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();
}


pub struct TypRFile<'a> {
    content: &'a str,
    name: String
}

impl<'a> TypRFile<'a> {

    pub fn new(content: &'a str, name: String) -> TypRFile<'a> {
        TypRFile {
            content: content,
            name: name
        }
    }

    pub fn parse(self) -> Lang  {
        parse(LocatedSpan::new_extra(self.content, self.name))
    }

}


//1. 
pub fn parse_code(path: &PathBuf, environment: Environment) -> Lang {
    let file = get_os_file(path.to_str().unwrap());
    let file_content = read_file(path).expect(&format!("Path {:?} not found", path));
    let base_file = TypRFile::new(&file_content, file);

    metaprogrammation(base_file.parse(), environment)
}
