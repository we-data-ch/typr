use std::fmt;
use serde::Serialize;
use crate::r#type::Type;
use crate::help_data::HelpData;
use crate::tchar::Tchar;
use crate::Context;
use crate::Var;
use crate::type_comparison::reduce_type;

#[derive(Debug, Clone, PartialEq, Serialize, Eq, Hash)] // 3 argument is for the embedding
pub struct ArgumentType(pub Type, pub Type, pub bool);

impl ArgumentType {
    pub fn to_r(&self) -> String {
        match &self.0 {
            Type::Char(c, _) => 
                match c {
                    Tchar::Val(x) => x.to_string(),
                    _ => "".to_string()
                },
            t => t.to_string()
        }
    }

    pub fn new(name: &str, type_: &Type) -> Self {
        ArgumentType(
            Type::Char(name.to_string().into(), HelpData::default()),
            type_.clone(), false)
    }

    pub fn get_type(&self) -> Type {
        self.1.clone()
    }

    pub fn get_argument(&self) -> Type {
        self.0.clone()
    }

    pub fn get_argument_str(&self) -> String {
        match self.0.clone() {
            Type::Char(l, _) => 
                match l {
                    Tchar::Val(c) => c.to_string(),
                    _ => panic!("A parameter can't be an empty value")
                }
            Type::LabelGen(l, _) => l.to_string().to_uppercase(),
            Type::Multi(t, _) 
                => ArgumentType(*t, self.1.clone(), false).get_argument_str(),
            _ => panic!("The argument wasn't a label")
        }
    }

    pub fn remove_embeddings(&self) -> ArgumentType {
        ArgumentType(self.0.clone(), self.1.clone(), false)
    }

    pub fn is_embedded(&self) -> bool {
        self.2
    }

    pub fn set_type(self, typ: Type) -> ArgumentType {
        ArgumentType(self.0, typ, self.2)
    }

    pub fn to_var(self, context: &Context) -> Var {
        let new_type = reduce_type(context, &self.get_type())
            .for_var();
        Var::from_type(self.get_argument())
            .expect("The arg_typ should have been label function")
            .set_type(new_type)
    }

}

impl fmt::Display for ArgumentType {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.0, self.1)       
    }
}

impl From<(String, Type)> for ArgumentType {
   fn from(val: (String, Type)) -> Self {
        ArgumentType(
            Type::Char(val.0.into(), val.1.clone().into()),
            val.1, false)
   } 
}
