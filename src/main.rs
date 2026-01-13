mod error_message;
mod type_checking;
mod interface;
mod context;
mod parsing;
mod r#type;
mod utils;
mod lang;

use crate::type_checking::type_checker::TypeChecker;
use crate::utils::package_loader::PackageManager;
use crate::type_checking::type_checker::typing;
use crate::engine::write_std_for_type_checking;
use crate::my_io::execute_r_with_path;
use crate::context::context::Context;
use crate::context::vartype::VarType;
use crate::help_message::TypeError;
use crate::lang::argument_value;
use error_message::help_message;
use crate::config::Environment;
use crate::engine::parse_code;
use error_message::help_data;
use utils::metaprogramming;
use parsing::parser::parse;
use r#type::function_type;
use crate::context::graph;
use crate::interface::cli;
use crate::lang::var::Var;
use crate::config::Config;
use crate::language::Lang;
use r#type::r#type::Type;
use utils::fluent_parser;
use parsing::type_token;
use r#type::module_type;
use parsing::lang_token;
use r#type::array_type;
use parsing::elements;
use my_io::read_file;
use context::config;
use lang::language;
use utils::builder;
use r#type::tchar;
use utils::engine;
use utils::my_io;
use r#type::tint;
use std::fs;


fn main() {
    cli::start()
}
