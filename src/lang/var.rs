#![allow(dead_code)]
use crate::type_checking::type_checker::typing;
use crate::error_message::help_data::HelpData;
use crate::lang::translatable::RTranslatable;
use crate::function_type::FunctionType;
use crate::context::context::Context;
use serde::{Serialize, Deserialize};
use crate::elements::is_pascal_case;
use crate::graph::TypeSystem;
use crate::tchar::Tchar;
use crate::builder;
use crate::Type;
use crate::Lang;
use std::fmt;

type Name = String;
type IsPackageOpaque = bool;

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize, Eq, Hash)]
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

impl From<Permission> for bool {
   fn from(val: Permission) -> Self {
       match val {
           Permission::Public => true,
           _ => false
       }
   } 
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub struct Var(pub Name, pub Permission, pub IsPackageOpaque, pub Type, pub HelpData);

impl Var {

    pub fn alias(name: &str, params: &[Type]) -> Self {
        Var::from(name)
            .set_type(Type::Params(params.to_vec(), HelpData::default()))
    }

    pub fn infer_var_related_type(&self, values: &Vec<Lang>, context: &Context) -> Var {
        if values.len() > 0 {
            let first_arg = values.iter().nth(0).unwrap().clone();
            let first_param_type = 
                typing(context, &first_arg).0;
            self.clone().set_type(first_param_type.clone())
        } else {
            self.clone()
        }
    }

    fn keep_minimals(liste: Vec<Type>, context: &Context) -> Vec<Type> {
        let mut mins: Vec<Type> = Vec::new();
        
        for candidat in liste {
            let mut keep_candidat = true;
            let mut indices_to_delete = Vec::new();
            
            for (i, existant) in mins.iter().enumerate() {
                if candidat.is_subtype(existant, context) {
                    indices_to_delete.push(i);
                } else if existant.is_subtype(&candidat, context) {
                    keep_candidat = false;
                    break;
                } 
            }
            
            if keep_candidat {
                for &i in indices_to_delete.iter().rev() {
                    mins.remove(i);
                }
                mins.push(candidat);
            }
        }
        mins
    }

    pub fn get_related_functions(self, values: &Vec<Lang>, context: &Context) 
        -> Vec<FunctionType> {
        let typed_var = self.infer_var_related_type(values, context);
        let res = context.get_matching_functions(typed_var.clone()).unwrap();
        Self::keep_minimals(res, context)
             .iter()
             .map(|x| x.to_function_type().unwrap())
             .collect()
    }

    pub fn get_function_signatures(&self, values: &Vec<Lang>, context: &Context) -> Vec<FunctionType> {
        self.clone()
            .get_related_functions(values, context)
    }

    pub fn from_language(l: Lang) -> Option<Var> {
        match l {
            Lang::Variable(name, perm, muta, typ, h) 
                => Some(Var(name, perm, muta, typ, h)),
            _ => None
        }
    }

    pub fn from_type(t: Type) -> Option<Var> {
        match t {
            Type::Alias(name, concret_types, opacity, h) => {
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
            Permission::Public,
            false,
            builder::empty_type(),
            HelpData::default())
    }

    pub fn to_language(self) -> Lang {
        Lang::Variable(self.0, self.1, self.2, self.3, self.4)
    }

    pub fn set_name(self, s: &str) -> Var {
       Var(s.to_string(), self.1, self.2, self.3, self.4)
    }

    pub fn set_type(self, typ: Type) -> Var {
        let typ = match typ {
            Type::Function(params, _, h) => {
                if params.len() >= 1 {
                    params[0].clone()
                } else { Type::Any(h) }
            },
            _ => typ
        };
        Var(self.0, self.1, self.2, typ, self.4)
    }

    pub fn set_type_raw(self, typ: Type) -> Var {
        Var(self.0, self.1, self.2, typ, self.4)
    }

    pub fn set_permission(self, perm: bool) -> Var {
        let new_perm = if perm == true { Permission::Public } else { Permission::Private };
        Var(self.0, new_perm, self.2, self.3, self.4)
    }

    pub fn set_importability(self, muta: bool) -> Var {
        Var(self.0, self.1, muta, self.3, self.4)
    }

    pub fn set_is_imported(self) -> Var {
        Var(self.0, self.1, true, self.3, self.4)
    }

    pub fn set_is_not_imported(self) -> Var {
        Var(self.0, self.1, false, self.3, self.4)
    }

    pub fn set_opacity(self, opa: bool) -> Var {
        Var(self.0, self.1, opa, self.3, self.4)
    }

    pub fn get_name(&self) -> String {
        self.0.to_string()
    }

    pub fn get_permission(&self) -> Permission {
        self.1
    }

    pub fn get_type(&self) -> Type {
        self.3.clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.4.clone()
    }

    pub fn match_with(&self, var: &Var, context: &Context) -> bool {
        (self.get_name() == var.get_name()) &&
        self.get_type().is_subtype(&var.get_type(), context)
    }

    pub fn set_help_data(self, h: HelpData) -> Var {
        Var(self.0, self.1, self.2, self.3, h)
    }

    pub fn is_imported(&self) -> bool {
        self.is_variable() && self.2.clone()
    }

    pub fn is_private(&self) -> bool {
        self.1 == Permission::Private
    }

    pub fn is_public(&self) -> bool {
        !self.is_private()
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
        self.is_alias() && self.2
    }

    pub fn get_opacity(&self) -> bool {
        self.2
    }

    pub fn to_alias_type(self) -> Type  {
        Type::Alias(self.get_name(), vec![], self.get_opacity(), self.get_help_data()) 
    }

    pub fn to_alias_lang(self) -> Lang  {
        Lang::Alias(Box::new(self.clone().to_language()),
                vec![], 
                builder::unknown_function(),
                self.get_help_data())
    }

    pub fn to_let(self) -> Lang  {
        Lang::Let(Box::new(self.clone().to_language()),
                builder::unknown_function(),
                Box::new(builder::empty_lang()),
                self.get_help_data())
    }

    pub fn contains(&self, s: &str) -> bool {
        self.get_name().contains(s)
    }

    pub fn replace(self, old: &str, new: &str) -> Self {
        let res = self.get_name().replace(old, new);
        self.set_name(&res)
    }

    pub fn display_type(self, cont: &Context) -> Self {
        if !self.get_name().contains(".") {
            let type_str = match self.get_type() {
                Type::Empty(_) | Type::Any(_) => "".to_string(),
                ty => ".".to_string() + &cont.get_class(&ty).replace("'", "")
            };
            let new_name = if self.contains("`") {
                "`".to_string() + &self.get_name().replace("`", "") + &type_str + "`"
            } else { self.get_name() + &type_str };
            self.set_name(&new_name)
        } else { self }
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
}

impl fmt::Display for Var {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}<{}>", self.0, self.3)       
    }
}

impl Default for Var {
    fn default() -> Self {
        Var("".to_string(), Permission::Public, false, Type::Empty(HelpData::default()), HelpData::default())
    }
}

impl RTranslatable<String> for Var {
    fn to_r(&self, _: &Context) -> String {
        format!("{}", self.0)
    }
}

impl TryFrom<Lang> for Var {
    type Error = ();

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Variable(name, perm, muta, typ, h) 
                => Ok(Var(name, perm, muta, typ, h)),
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

impl TryFrom<&Box<Lang>> for Var {
    type Error = ();

    fn try_from(value: &Box<Lang>) -> Result<Self, Self::Error> {
        Var::try_from((*value).clone())
    }
}

impl From<&str> for Var {
   fn from(val: &str) -> Self {
        Var(
            val.to_string(),
            Permission::Public,
            false,
            Type::Empty(HelpData::default()),
            HelpData::default())
   } 
}

impl TryFrom<Type> for Var {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
       match value {
           Type::Char(tchar, h) => {
                match tchar {
                    Tchar::Val(name) => {
                        let var = if is_pascal_case(&name) {
                            Var::from_name(&name)
                                .set_help_data(h)
                                .set_type(builder::params_type())
                        } else {
                            Var::from_name(&name)
                                .set_help_data(h)
                        };
                        Ok(var)
                    },
                    _ => todo!()
                }
           },
           _ => Err("From type to Var, not possible".to_string())
       }
    }
}
