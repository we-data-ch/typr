use std::fmt;
use serde::Serialize;
use crate::language::Lang;
use crate::Context;

#[derive(Debug, Serialize, Clone)]
pub struct Adt(pub Vec<Lang>);

impl fmt::Display for Adt {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cont = Context::new(vec![], vec![]);
        let res = self.0.iter().map(|x| x.disp(&cont))
            .reduce(|acc, x| format!("{}, {}", acc, x))
            .unwrap_or("".to_string());
        write!(f, "sequence([{}])", res)       
    }
}

impl From<Vec<Lang>> for Adt {
   fn from(val: Vec<Lang>) -> Self {
        Adt(val)
   } 
}

fn find_alias(name: &str, v: Vec<Lang>) -> Lang {
    v.iter().find(|x| 
        match x {
          Lang::Alias(var, _params, _typ) 
                if var.get_name() == name => true,
            _ => false
        }).unwrap().clone()
}

impl Adt {
    pub fn iter(&self) -> std::slice::Iter<Lang> {
        self.0.iter()
    }

    pub fn to_r(&self, cont: &Context) -> String {
        self.iter()
            .map(|line| line.to_r(cont))
            .fold(String::from(""), |acc, x| format!("{}\n{}", acc, x))
    }

    pub fn to_wasm(&self, cont: &Context) -> String {
        self.iter()
            .map(|line| line.to_typescript(cont))
            .fold(String::from(""), |acc, x| format!("{}\n{}", acc, x))
    }

    pub fn add(self, adt: Adt) -> Adt {
        Adt(self.0.iter().chain(adt.0.iter()).cloned().collect::<Vec<_>>())
    }
}
