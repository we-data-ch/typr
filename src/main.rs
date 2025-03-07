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
mod context_manager;
mod type_comparison;
mod unification;
mod context;
mod adt;
mod module;
mod nominal_context;
mod r#type;
mod kinds;
mod kind;
mod type_checker;
mod type_context;
mod type_module;
mod type_printer;
mod tag;
mod index;

use parser::parse;
use my_io::{read_file, execute_r};
use crate::r#type::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use std::fs::File;
use std::io::Write;
use crate::adt::Adt;
use crate::type_checker::typing;
use crate::context::Context;
use crate::nominal_context::NominalContext;

fn write_adt_to_r(adt: &Adt, nominal: &NominalContext, cont: &Context) -> () {
    let rstd = include_str!("../configs/std.R");
    let mut app = File::create("app.R").unwrap();
    let content = format!("{}\n\n{}", rstd, adt.to_r(nominal, cont));
    app.write_all(content.as_bytes()).unwrap();
}

fn execute(adt: &Adt, nominal: NominalContext, cont: &Context) -> () {
    write_adt_to_r(&adt, &nominal, cont);
    execute_r();
}

fn type_check(adt: &Adt) -> (NominalContext, Context) {
    let (typ, context) = typing(&Context::default(), &Lang::Sequence(adt.0.clone()));
    type_printer::pretty_print(&typ);
    (NominalContext::from(&context), context)
}

fn parse_code() -> Adt {
    let adt = parse(&read_file()).unwrap().1;
    metaprogrammation(adt.clone())
}

fn main() {
    let adt = parse_code();

    let (nominal, context) = type_check(&adt);
    execute(&adt, nominal, &context);
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
