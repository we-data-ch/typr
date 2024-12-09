use crate::Type;
use crate::Lang;
use std::fmt;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Serialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Var(pub String, pub String, pub Permission, pub bool, pub Type);

impl Var {
    pub fn from_language(l: Lang) -> Option<Var> {
        match l {
            Lang::Variable(name, path, perm, muta, typ) 
                => Some(Var(name, path, perm, muta, typ)),
            _ => None
        }
    }

    pub fn from_name(name: &str) -> Self {
        Var(
            name.to_string(),
            "".to_string(),
            Permission::Private,
            false,
            Type::Empty)
    }

    pub fn to_language(self) -> Lang {
        Lang::Variable(self.0, self.1, self.2, self.3, self.4)
    }

    pub fn set_type(self, typ: Type) -> Var {
        Var(self.0, self.1, self.2, self.3, typ)
    }

    pub fn set_permission(self, perm: bool) -> Var {
        let new_perm = if perm == true { Permission::Public } else { Permission::Private };
        Var(self.0, self.1, new_perm, self.3, self.4)
    }

    pub fn set_mutability(self, muta: bool) -> Var {
        Var(self.0, self.1, self.2, muta, self.4)
    }

    pub fn set_opacity(self, opa: bool) -> Var {
        Var(self.0, self.1, self.2, opa, self.4)
    }

    pub fn add_path(self, name: &str) -> Var {
        if self.1 == "" {
            Var(self.0, name.to_string(), self.2, self.3, self.4)
        } else {
            Var(self.0, self.1 + "/" + name, self.2, self.3, self.4)
        }
    }

    pub fn get_name(&self) -> String {
        self.0.to_string()
    }

    pub fn get_path(&self) -> String {
        self.1.to_string()
    }

    pub fn set_path(self, new_path: &str) -> Var {
        Var(self.0, new_path.to_string(), self.2, self.3, self.4)
    }

    pub fn get_type(&self) -> Type {
        self.4.clone()
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
        Var("".to_string(), "".to_string(), Permission::Private, false, Type::Empty)
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
