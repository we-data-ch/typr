#![allow(dead_code)]
use crate::Type;
use crate::Lang;
use std::fmt;
use serde::Serialize;
use crate::context::Context;
use crate::type_comparison;
use crate::help_data::HelpData;

type Name = String;
type Path = String;
type IsMutableOpaque = bool;

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Eq, Hash)]
pub enum Permission {
    Private,
    Public
}

impl fmt::Display for Permission {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Permission::Private => write!(f, "private"),
            Permission::Public => write!(f, "public")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Eq, Hash)]
pub struct Var(pub Name, pub Path, pub Permission, pub IsMutableOpaque, pub Type, pub HelpData);

impl Var {
    pub fn from_language(l: Lang) -> Option<Var> {
        match l {
            Lang::Variable(name, path, perm, muta, typ, h) 
                => Some(Var(name, path, perm, muta, typ, h)),
            _ => None
        }
    }

    pub fn from_name(name: &str) -> Self {
        Var(
            name.to_string(),
            "".to_string(),
            Permission::Private,
            false,
            Type::Empty(HelpData::default()),
            HelpData::default())
    }

    pub fn to_language(self) -> Lang {
        Lang::Variable(self.0, self.1, self.2, self.3, self.4, self.5)
    }

    pub fn set_name(self, s: &str) -> Var {
       Var(s.to_string(), self.1, self.2, self.3, self.4, self.5)
    }

    pub fn set_type(self, typ: Type) -> Var {
        Var(self.0, self.1, self.2, self.3, typ, self.5)
    }

    pub fn set_permission(self, perm: bool) -> Var {
        let new_perm = if perm == true { Permission::Public } else { Permission::Private };
        Var(self.0, self.1, new_perm, self.3, self.4, self.5)
    }

    pub fn set_mutability(self, muta: bool) -> Var {
        Var(self.0, self.1, self.2, muta, self.4, self.5)
    }

    pub fn set_opacity(self, opa: bool) -> Var {
        Var(self.0, self.1, self.2, opa, self.4, self.5)
    }

    pub fn add_path(self, name: &str) -> Var {
        if self.1 == "" {
            Var(self.0, name.to_string(), self.2, self.3, self.4, self.5)
        } else {
            Var(self.0, self.1 + "/" + name, self.2, self.3, self.4, self.5)
        }
    }

    pub fn get_name(&self) -> String {
        self.0.to_string()
    }

    pub fn get_path(&self) -> String {
        self.1.to_string()
    }

    pub fn get_permission(&self) -> Permission {
        self.2
    }

    pub fn set_path(self, new_path: &str) -> Var {
        Var(self.0, new_path.to_string(), self.2, self.3, self.4, self.5)
    }

    pub fn get_type(&self) -> Type {
        self.4.clone()
    }

    pub fn match_with(&self, var: &Var, context: &Context) -> bool {
        [(self.get_name() == var.get_name()),
        (self.get_path() == var.get_path()),
        //(self.get_permission() == var.get_permission()),
        type_comparison::is_matching(context, &self.get_type(), &var.get_type())].iter().all(|&x| x)
    }
}

impl fmt::Display for Var {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.1 == "" { 
            write!(f, "var('{}', empty, {}, {}, {})",
            self.0, self.2, self.3, self.4)       
        } else {
            write!(f, "var('{}', '{}', {}, {}, {})", 
                   self.0, self.1, self.2, self.3, self.4)
        }
    }
}

impl Default for Var {
    fn default() -> Self {
        Var("".to_string(), "".to_string(), Permission::Private, false, Type::Empty(HelpData::default()), HelpData::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_var_perm1(){
        assert_eq!(
            Var("hey".to_string(), "".to_string(), Permission::Public, false, Type::Empty).set_permission(false),
            Var("hey".to_string(), "".to_string(), Permission::Private, false, Type::Empty)
            ); 
    }
}
