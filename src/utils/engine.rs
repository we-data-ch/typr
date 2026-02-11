use crate::components::context::config::Environment;
use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::language::Lang;
use crate::processes::parsing::parse;
use crate::processes::parsing::ParseResult;
use crate::utils::metaprogramming::metaprogrammation;
use crate::utils::my_io::get_os_file;
use crate::utils::my_io::read_file;
use nom_locate::LocatedSpan;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub fn write_std_for_type_checking(output_dir: &PathBuf) {
    let rstd = include_str!("../../configs/std/std_R.ty");
    let std_path = output_dir.join("std.ty");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();
}

pub struct TypRFile<'a> {
    content: &'a str,
    name: String,
}

impl<'a> TypRFile<'a> {
    pub fn new(content: &'a str, name: String) -> TypRFile<'a> {
        TypRFile {
            content: content,
            name: name,
        }
    }

    /// Parse and return the full ParseResult with AST and collected errors
    pub fn parse_with_errors(self) -> ParseResult {
        parse(LocatedSpan::new_extra(self.content, self.name))
    }

    /// Parse and return just the AST (legacy behavior)
    pub fn parse(self) -> Lang {
        self.parse_with_errors().ast
    }
}

/// Result of parsing a code file, containing the AST and any syntax errors
pub struct ParseCodeResult {
    pub ast: Lang,
    pub errors: Vec<SyntaxError>,
}

impl ParseCodeResult {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

/// Parse code and return the AST along with any syntax errors collected
pub fn parse_code_with_errors(path: &PathBuf, environment: Environment) -> ParseCodeResult {
    let file = get_os_file(path.to_str().unwrap());
    let file_content = read_file(path).expect(&format!("Path {:?} not found", path));
    let base_file = TypRFile::new(&file_content, file);

    let parse_result = base_file.parse_with_errors();
    let ast = metaprogrammation(parse_result.ast, environment);

    ParseCodeResult {
        ast,
        errors: parse_result.errors,
    }
}

/// Parse code and return the AST (legacy behavior, ignores syntax errors)
pub fn parse_code(path: &PathBuf, environment: Environment) -> Lang {
    parse_code_with_errors(path, environment).ast
}
