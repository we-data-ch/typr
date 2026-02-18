#![allow(dead_code)]

use crate::components::context::config::Environment;
use crate::components::context::Context;
use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use crate::processes::parsing::parse;
use crate::processes::parsing::ParseResult;
use crate::processes::type_checking::typing_with_errors;
// `TypingResult` import removed — not used in this module
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

/// Complete result of compiling a TypR file (parsing + type checking)
pub struct CompileResult {
    /// The parsed and type-checked AST
    pub ast: Lang,
    /// The inferred type of the program
    pub inferred_type: Type,
    /// The final typing context
    pub context: Context,
    /// All errors (syntax + type) collected during compilation
    pub errors: Vec<TypRError>,
}

impl CompileResult {
    /// Check if compilation produced any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get only syntax errors
    pub fn syntax_errors(&self) -> Vec<&SyntaxError> {
        self.errors
            .iter()
            .filter_map(|e| match e {
                TypRError::Syntax(s) => Some(s),
                _ => None,
            })
            .collect()
    }

    /// Get only type errors
    pub fn type_errors(&self) -> Vec<&crate::components::error_message::type_error::TypeError> {
        self.errors
            .iter()
            .filter_map(|e| match e {
                TypRError::Type(t) => Some(t),
                _ => None,
            })
            .collect()
    }
}

/// Parse and type-check code, returning all errors collected
///
/// This is the main entry point for compiling TypR code with error collection.
/// It performs both parsing and type checking, collecting all errors along the way.
pub fn compile_code_with_errors(path: &PathBuf, environment: Environment) -> CompileResult {
    let file = get_os_file(path.to_str().unwrap());
    let file_content = read_file(path).expect(&format!("Path {:?} not found", path));
    let base_file = TypRFile::new(&file_content, file);

    // Parse with error collection
    let parse_result = base_file.parse_with_errors();
    let ast = metaprogrammation(parse_result.ast, environment);

    // Convert syntax errors to TypRErrors
    let mut all_errors: Vec<TypRError> = parse_result
        .errors
        .into_iter()
        .map(TypRError::Syntax)
        .collect();

    // Type check with error collection
    let context = Context::default();
    let typing_result = typing_with_errors(&context, &ast);

    // Collect type errors
    all_errors.extend(typing_result.errors);

    CompileResult {
        ast: typing_result.type_context.lang,
        inferred_type: typing_result.type_context.value,
        context: typing_result.type_context.context,
        errors: all_errors,
    }
}

/// Compile code from a string (useful for REPL and testing)
pub fn compile_string_with_errors(
    code: &str,
    file_name: &str,
    environment: Environment,
) -> CompileResult {
    let base_file = TypRFile::new(code, file_name.to_string());

    // Parse with error collection
    let parse_result = base_file.parse_with_errors();
    let ast = metaprogrammation(parse_result.ast, environment);

    // Convert syntax errors to TypRErrors
    let mut all_errors: Vec<TypRError> = parse_result
        .errors
        .into_iter()
        .map(TypRError::Syntax)
        .collect();

    // Type check with error collection
    let context = Context::default();
    let typing_result = typing_with_errors(&context, &ast);

    // Collect type errors
    all_errors.extend(typing_result.errors);

    CompileResult {
        ast: typing_result.type_context.lang,
        inferred_type: typing_result.type_context.value,
        context: typing_result.type_context.context,
        errors: all_errors,
    }
}
