#![allow(dead_code)]
use crate::Type;
use crate::Lang;
use std::fmt;
use serde::Serialize;
use crate::context::Context;
use crate::type_comparison;
use crate::help_data::HelpData;
use crate::path::Path;
use crate::translatable::RTranslatable;
use crate::function_type::FunctionType;
use crate::typing;
use crate::TypeError;
use crate::help_message::ErrorMsg;

type Name = String;
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

// main
impl Var {

    pub fn infer_var_name(&self, args: &Vec<Lang>, context: &Context) -> Var {
        if args.len() > 0 {
            let first = typing(context, &args.iter().nth(0).unwrap().clone()).0;
            self.clone().set_type(first[0].clone())
        } else {
            self.clone()
        }
    }

    pub fn get_related_function(self, args: &Vec<Lang>, context: &Context) 
        -> Option<FunctionType> {
        let var_name = self.infer_var_name(args, context);
        let fn_ty = typing(context, &var_name.to_language()).0;
        fn_ty[0].clone().to_function_type()
    }

    pub fn get_function_signature(&self, values: &Vec<Lang>, context: &Context) -> FunctionType {
        if context.is_an_untyped_function(&self.get_name()) {
            FunctionType::try_from(Type::RFunction(HelpData::default())).unwrap()
        } else {
            self.clone()
                .get_related_function(values, context)
                .expect(&TypeError::UndefinedFunction((self).clone()).display())
        }
    }

    pub fn from_language(l: Lang) -> Option<Var> {
        match l {
            Lang::Variable(name, path, perm, muta, typ, h) 
                => Some(Var(name, path, perm, muta, typ, h)),
            _ => None
        }
    }

    pub fn from_type(t: Type) -> Option<Var> {
        match t {
            Type::Alias(name, concret_types, _base_type, opacity, h) => {
                    let var = Var::from_name(&name)
                        .set_type(Type::Params(concret_types.to_vec(), concret_types.clone().into()))
                        .set_help_data(h)
                        .set_opacity(opacity);
                    Some(var)
            },
            Type::Char(val, h) => {
                let var = Var::from_name(&val.get_val())
                    .set_help_data(h);
                Some(var)
            }
            _ => None
        }
    }

    pub fn from_name(name: &str) -> Self {
        Var(
            name.to_string(),
            "".into(),
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
        let typ = if let Type::Function(_, params, _, h) = typ {
            if params.len() >= 1 {
                params[0].clone()
            } else { Type::Any(h) }
        } else { typ };
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

    pub fn add_path(self, name: Path) -> Var {
        if self.1 == Path::default() {
            Var(self.0, name.into(), self.2, self.3, self.4, self.5)
        } else {
            Var(self.0, self.1 + name, self.2, self.3, self.4, self.5)
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

    pub fn set_path(self, new_path: Path) -> Var {
        Var(self.0, new_path, self.2, self.3, self.4, self.5)
    }

    pub fn get_type(&self) -> Type {
        self.4.clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.5.clone()
    }

    pub fn match_with(&self, var: &Var, context: &Context) -> bool {
        (self.get_name() == var.get_name()) &&
        (self.get_path() == var.get_path()) &&
        type_comparison::is_matching(context, &self.get_type(), &var.get_type())
    }

    pub fn set_help_data(self, h: HelpData) -> Var {
        Var(self.0, self.1, self.2, self.3, self.4, h)
    }

    pub fn is_mutable(&self) -> bool {
        self.is_variable() && self.3.clone()
    }

    pub fn is_private(&self) -> bool {
        self.2 == Permission::Private
    }

    pub fn is_public(&self) -> bool {
        !self.is_private()
    }

    pub fn is_from_other_module(&self) -> bool {
        !self.1.is_empty()
    }

    pub fn is_natif_to_module(&self) -> bool {
        !self.is_from_other_module()
    }

    pub fn is_alias(&self) -> bool {
        match self.get_type() {
            Type::Params(_, _) => true,
            _ => false
        }
    }

    pub fn is_variable(&self) -> bool {
        !self.is_alias()
    }

    pub fn is_opaque(&self) -> bool {
        self.is_alias() && self.3
    }

    pub fn get_opacity(&self) -> bool {
        self.3
    }

    pub fn to_alias(self) -> Type  {
        Type::Alias(self.get_name(), vec![], self.get_path().into(), self.get_opacity(), self.get_help_data()) 
    }

    pub fn contains(&self, s: &str) -> bool {
        if self.get_name().len() > s.len() {
            &self.get_name()[0..s.len()] == s
        } else { false }
    }

    pub fn get_digit(&self, s: &str) -> i8 {
        self.get_name()[s.len()..].parse::<i8>().unwrap()
    }

    pub fn add_digit(self, d: i8) -> Self {
        self.clone().set_name(&(self.get_name() + &d.to_string()))
    }

    pub fn exist(&self, context: &Context) -> Option<Self> {
        context.variable_exist(self.clone())
    }

    pub fn not_generic_yet(&self, context: &Context) -> bool {
        context.not_generic_yet(self.get_name())
    }

}

impl fmt::Display for Var {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}{}<{}>",
            self.1, self.0, self.4)       
    }
}

impl Default for Var {
    fn default() -> Self {
        Var("".to_string(), "".into(), Permission::Private, false, Type::Empty(HelpData::default()), HelpData::default())
    }
}

impl RTranslatable<String> for Var {
    fn to_r(&self, _: &Context) -> String {
        format!("{}{}", self.1.clone().to_r(), self.0)
    }
}

impl TryFrom<Lang> for Var {
    type Error = ();

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Variable(name, path, perm, muta, typ, h) 
                => Ok(Var(name, path, perm, muta, typ, h)),
            _ => Err(())
        }
    }
}

impl TryFrom<Box<Lang>> for Var {
    type Error = ();

    fn try_from(value: Box<Lang>) -> Result<Self, Self::Error> {
        Var::try_from((*value).clone())
    }
}

impl From<&str> for Var {
   fn from(val: &str) -> Self {
        Var(
            val.to_string(),
            "".into(),
            Permission::Private,
            false,
            Type::Empty(HelpData::default()),
            HelpData::default())
   } 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_default_var_not_foreign(){
        let var = Var::default();
        assert!(!var.is_from_other_module())
    }

}
