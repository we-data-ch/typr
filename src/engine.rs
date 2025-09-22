use crate::Adt;
use std::fs::File;
use crate::AdtManager;
use crate::parse;
use crate::read_file;
use crate::metaprogrammation;
use std::io::Write;
use std::path::PathBuf;
use nom_locate::LocatedSpan;
use crate::my_io::get_os_file;


pub fn write_std_for_type_checking(output_dir: &PathBuf) {
    let rstd = include_str!("../configs/r/std.ty");
    let std_path = output_dir.join("std.ty");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();
}


struct TypRFile<'a> {
    content: &'a str,
    name: String
}

impl<'a> TypRFile<'a> {

    fn new(content: &'a str, name: String) -> TypRFile<'a> {
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
        .add_to_body(std_file.parse())
        .add_to_body(base_file.parse());

    let adt = metaprogrammation(adt_manager.body.clone());
    adt_manager.set_body(adt)
}
