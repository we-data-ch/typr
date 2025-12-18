#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use rpds::Vector;
use crate::Type;
use crate::Var;
use crate::Lang;
use crate::Context;
use crate::parser::parse2;
use crate::typing;
use crate::builder;

#[derive(Debug, Clone)]
struct FluentParser {
    raw_code: Vector<String>,
    code: Vector<Lang>,
    logs: Vector<String>,
    context: Context,
    last_type: Type
}

impl FluentParser {

    pub fn new() -> Self {
       FluentParser {
           raw_code: Vector::new(),
           code: Vector::new(),
           logs: Vector::new(),
           context: Context::empty(),
           last_type: builder::empty_type()
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

    fn next_code(self) -> Option<(Lang, Self)> {
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

    // Type the next parser code
    pub fn type_next(self) -> Self {
        match self.clone().next_code() {
            Some((code, rest)) => {
                let (typ, lang, new_context) =  typing(&self.context, &code);
                rest.set_context(new_context)
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

}
