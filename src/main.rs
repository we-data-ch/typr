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
//mod context_manager;
//mod module;
mod type_comparison;
mod unification;
mod context;
mod adt;
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
mod adt_manager;
mod nominals;
mod subtypes;

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
use crate::adt_manager::AdtManager;
use std::collections::HashSet;

pub fn is_subset<T: Eq + std::hash::Hash>(v1: &[T], v2: &[T]) -> bool {
    let set_v2: HashSet<_> = v2.iter().collect();
    v1.iter().all(|x| set_v2.contains(x))
}

fn write_adt_to_r(adt: &Adt, cont: &Context) -> () {
    let rstd = include_str!("../configs/std.R");
    let mut app = File::create("app.R").unwrap();
    let content = format!("{}\n\n{}\n\n{}", rstd, Adt(cont.adt.clone()).to_r(cont), adt.to_r(cont));
    app.write_all(content.as_bytes()).unwrap();
}

fn execute(adt: &Adt, cont: &Context) -> () {
    write_adt_to_r(&adt, cont);
    execute_r();
}

fn type_check(adt: &Adt) -> Context {
    let (typ, context) = typing(&Context::default(), &Lang::Sequence(adt.0.clone()));
    type_printer::pretty_print(&typ);
    context
}

fn parse_code() -> AdtManager {
    let typr_std = include_str!("../configs/std.ty");
    let adt_manager = AdtManager::new()
        .add_to_body(parse(&read_file()).unwrap().1)
        .add_to_header(parse(typr_std).unwrap().1);
    let adt = metaprogrammation(adt_manager.body.clone());
    adt_manager.set_body(adt)
}

fn main() {
    let adt_manager = parse_code();

    //let context = type_check(&adt_manager.get_adt_with_header());
    let context = type_check(&adt_manager.get_adt_with_header());
    execute(&adt_manager.get_adt_without_header(), &context);
}

#[cfg(test)]
mod tests {
    use crate::elements::parse_elements;
    use crate::language::Lang;

    #[test]
    fn test1(){
        let text = "true and false and true and true;";
        let value = parse_elements(text).unwrap().1;
        assert_eq!(
            value,
            Lang::Empty);
    }

    #[test]
    fn test2() {
        let text = "true or false;";
        let value = parse_elements(text).unwrap().1;
        assert_eq!(
            value, 
            Lang::Empty);
    }
}
