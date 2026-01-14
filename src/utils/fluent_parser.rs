#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::lang::var::Var;
use crate::components::context::context::Context;
use crate::type_checking::type_checker::typing;
use crate::lang::translatable::RTranslatable;
use crate::components::r#type::r#type::Type;
use crate::components::lang::language::Lang;
use crate::parsing::parser::parse2;
use crate::graph::TypeSystem;
use rpds::Vector;
use crate::utils::builder;

#[derive(Debug, Clone)]
pub struct FluentParser {
    raw_code: Vector<String>,
    code: Vector<Lang>,
    new_code: Vector<Lang>,
    r_code: Vector<String>,
    logs: Vector<String>,
    pub context: Context,
    last_type: Type,
    pub saved_r: Vector<String>
}

impl FluentParser {

    pub fn new() -> Self {
       FluentParser {
           raw_code: Vector::new(),
           code: Vector::new(),
           new_code: Vector::new(),
           r_code: Vector::new(),
           logs: Vector::new(),
           context: Context::empty(),
           last_type: builder::empty_type(),
           saved_r: Vector::new()
       } 
    }

    pub fn push(self, code: &str) -> Self {
        Self {
            raw_code: self.raw_code.push_back(code.to_string()),
            ..self
        }
    }

    pub fn push_log(self, log: &str) -> Self {
        Self {
            logs: self.logs.push_back(log.to_string()),
            ..self
        }
    }

    pub fn push_code(self, code: Lang) -> Self {
        Self {
            code: self.code.push_back(code),
            ..self
        }
    }

    fn drop_first_raw(self) -> Self {
        Self {
            raw_code: self.raw_code.iter().skip(1).cloned().collect(),
            ..self
        }
    }

    fn next_raw_code(self) -> Option<(String, Self)> {
        match self.clone().raw_code.first() {
            Some(val) => Some((val.clone(), self.drop_first_raw())),
            _ => None
        }
    }

    // FluentParser can parse step by step
    pub fn parse_next(self) -> Self {
        match self.clone().next_raw_code() {
            Some((line, rest)) => {
                match parse2((&line[..]).into()) {
                    Ok(code) => rest.push_code(code),
                    Err(msg) => rest.push_log(&msg),
                }
            },
            _ => self.push_log("No more raw line left")
        }
    }

    pub fn clean_raw_code(self) -> Self {
        Self {
            raw_code: Vector::new(),
            ..self
        }
    }

    pub fn parse_all_lines(self) -> Self {
        self.clone().raw_code.iter()
            .fold(self, |acc, x| {
                match parse2(x[..].into()) {
                    Ok(code) => acc.push_code(code),
                    Err(msg) => acc.push_log(&msg)
                }
            }).clean_raw_code()
    }

    fn drop_first_code(self) -> Self {
        Self {
            code: self.code.iter().skip(1).cloned().collect(),
            ..self
        }
    }

    pub fn next_code(self) -> Option<(Lang, Self)> {
        match self.code.first() {
            Some(lang) 
                => Some((lang.clone(), self.drop_first_code())),
            _ => None
        }
    }

    pub fn set_context(self, context: Context) -> Self {
        Self {
            context,
            ..self
        }
    }

    fn set_last_type(self, typ: Type) -> Self {
        Self {
            last_type: typ,
            ..self
        }
    }

    pub fn push_new_code(self, code: Lang) -> Self {
        Self {
            new_code: self.new_code.push_back(code),
            ..self
        }
    }

    // Type the next parser code
    pub fn type_next(self) -> Self {
        match self.clone().next_code() {
            Some((code, rest)) => {
                let (typ, lang, new_context) =  typing(&self.context, &code);
                rest.set_context(new_context)
                    .push_new_code(lang)
                    .set_last_type(typ)
            },
            _ => self.push_log("No more Lang code left")
        }
    }

    pub fn type_all(self) -> Self {
        let (new_context, new_type) = self.clone().code.iter()
            .fold((self.clone().context, builder::empty_type()), 
                  |(cont, typ), x| {
                      let (new_type, _, new_cont) = typing(&cont, x);
                      (new_cont, new_type)
                  });
        self.set_context(new_context).set_last_type(new_type)
    }

    pub fn parse_type_next(self) -> Self {
        self.parse_next()
            .type_next()
    }

    pub fn parse_type_all(self) -> Self {
        self.parse_all_lines()
            .type_all()
    }

    pub fn type_of(&self, symbol: &str) -> Vec<Type> {
        let var = Var::from_name(symbol);
        vec![self.context.get_type_from_existing_variable(var)]
    }

    pub fn view_logs(&self) -> String {
        self.logs.iter().cloned().collect::<Vec<_>>().join("\n")
    }

    pub fn get_log(&self, id: i32) -> String {
        let id = id as usize;
        if self.logs.len() > id {
            self.logs[id].clone()
        } else {
            format!("There aren't any log at index {}", id)
        }
    }

    pub fn get_last_log(&self) -> String {
        if self.logs.len() > 0_usize {
            self.logs.iter().rev().next().unwrap().clone()
        } else {
            "The logs are empty".to_string()
        }
    }

    pub fn get_last_type(&self) -> Type {
        self.last_type.clone()
    }

    fn drop_first_new_code(self) -> Self {
        Self {
            new_code: self.new_code.iter().skip(1).cloned().collect(),
            ..self
        }
    }

    pub fn next_new_code(self) -> Option<(Lang, Self)> {
        match self.new_code.first() {
            Some(lang) 
                => Some((lang.clone(), self.drop_first_new_code())),
            _ => None
        }
    }

    pub fn push_r_code(self, r_code: String) -> Self {
        Self {
            r_code: self.r_code.push_back(r_code),
            ..self
        }
    }

    fn save_r_code(self, r_code: &str) -> Self {
        Self {
            saved_r: self.saved_r.push_back(r_code.to_string()),
            ..self
        }
    }

    pub fn get_saved_r_code(&self) -> String {
        self.saved_r.iter()
            .cloned()
            .reduce(|acc, x| format!("{}\n{}", acc, &x))
            .unwrap_or("".to_string())
    }

    fn get_let_definitions(v: Vector<Lang>, context: &Context) -> Vec<String> {
        v.iter()
         .filter(|x| x.save_in_memory())
         .map(|x| x.to_r(context).0)
         .collect()
    }

    pub fn transpile_next(self) -> Self {
        match self.clone().next_new_code() {
            Some((code, rest)) => {
                let (r_code, new_context) =  code.to_r(&self.context);
                let res = rest.set_context(new_context)
                    .push_r_code(r_code);
                Self::get_let_definitions(self.new_code, &self.context)
                    .iter()
                    .fold(res, |acc, x| acc.save_r_code(x))
            },
            _ => self.push_log("No more Lang code left")
        }
    }

    pub fn parse_type_transpile_next(self) -> Self {
        self
            .parse_next()
            .type_next()
            .transpile_next()
    }

    pub fn run(self) -> Self {
        self.parse_type_transpile_next()
    }

    fn drop_first_r_code(self) -> Self {
        Self {
            r_code: self.r_code.iter().skip(1).cloned().collect(),
            ..self
        }
    }

    pub fn next_r_code(self) -> Option<(String, Self)> {
        match self.r_code.first() {
            Some(lang) 
                => Some((lang.clone(), self.drop_first_r_code())),
            _ => None
        }
    }

    pub fn display_context(&self) -> String {
        self.context.display_typing_context()
    }

}


use std::fmt;
impl fmt::Display for FluentParser {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = format!("raw_code: {}\ncode: {}\nnew_code: {}\nr_code: {}\nlast_type: {}",
                self.raw_code.iter().cloned().collect::<Vec<_>>().join(" | "),
                self.code.iter().map(|x| x.simple_print()).collect::<Vec<_>>().join(" | "),
                self.new_code.iter().map(|x| x.simple_print()).collect::<Vec<_>>().join(" | "),
                self.r_code.iter().cloned().collect::<Vec<_>>().join(" | "),
                self.last_type.pretty());
        write!(f, "{}", res)       
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // should be able to parse a line
    // should log an error if line parsing go wrong
    // should get the type of a loaded variable

    #[test]
    fn test_fluent_parser0(){
        let typ = FluentParser::new()
            .push("8")
            .parse_type_next()
            .get_last_type();
        assert_eq!(typ, builder::integer_type(8))
    }

    #[test]
    fn test_fluent_parser1(){
        let typ = FluentParser::new()
            .push("let df <- 8;").parse_type_next()
            .push("9").parse_type_next()
            .get_last_type();
        assert_eq!(typ, builder::integer_type(8))
    }

    #[test]
    fn test_fluent_transpiler1(){
        let fp = FluentParser::new()
            .push("8")
            .run()
            ;
        assert_eq!(fp.next_r_code().unwrap().0, "8L")
    }

}
