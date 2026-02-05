#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::locatable::Locatable;
use crate::components::language::Lang;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::parsing::elements::is_pascal_case;
use crate::processes::transpiling::translatable::RTranslatable;
use crate::processes::type_checking::typing;
use crate::utils::builder;
use serde::{Deserialize, Serialize};
use std::fmt;

type Name = String;
type IsPackageOpaque = bool;

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize, Eq, Hash)]
pub enum Permission {
    Private,
    Public,
}

impl fmt::Display for Permission {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Permission::Private => write!(f, "private"),
            Permission::Public => write!(f, "public"),
        }
    }
}

impl From<Permission> for bool {
    fn from(val: Permission) -> Self {
        match val {
            Permission::Public => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash)]
pub struct Var {
    pub name: Name,
    pub is_opaque: IsPackageOpaque,
    pub related_type: Type,
    pub help_data: HelpData,
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.is_opaque == other.is_opaque
            && self.related_type == other.related_type
    }
}

impl Eq for Var {}

impl Locatable for Var {
    fn get_help_data(&self) -> HelpData {
        self.help_data.clone()
    }
}

impl Var {
    pub fn add_backticks_if_percent(self) -> Self {
        let s = self.get_name();
        let res = if s.starts_with('%') && s.ends_with('%') {
            format!("`{}`", s)
        } else {
            s.to_string()
        };
        self.set_name(&res)
    }

    pub fn alias(name: &str, params: &[Type]) -> Self {
        Var::from(name).set_type(Type::Params(params.to_vec(), HelpData::default()))
    }

    pub fn set_var_related_type(&self, types: &Vec<Type>, context: &Context) -> Var {
        if types.len() > 0 {
            let first_arg = types.iter().nth(0).unwrap().clone();
            self.clone().set_type(first_arg.clone())
        } else {
            self.clone()
        }
    }

    fn keep_minimal(liste: Vec<Type>, context: &Context) -> Option<Type> {
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
        // get smallest type
        if mins.iter().any(|x| !x.is_interface()) {
            mins.iter().cloned().skip_while(|x| x.is_interface()).next()
        } else {
            mins.iter().cloned().next()
        }
    }

    pub fn get_functions_from_name(&self, context: &Context) -> Vec<FunctionType> {
        context
            .get_functions_from_name(&self.get_name())
            .iter()
            .flat_map(|(_, typ)| typ.clone().to_function_type())
            .collect()
    }

    pub fn from_language(l: Lang) -> Option<Var> {
        match l {
            Lang::Variable(name, muta, typ, h) => Some(Var {
                name,
                is_opaque: muta,
                related_type: typ,
                help_data: h,
            }),
            _ => None,
        }
    }

    pub fn from_type(t: Type) -> Option<Var> {
        match t {
            Type::Alias(name, concret_types, opacity, h) => {
                let var = Var::from_name(&name)
                    .set_type(Type::Params(
                        concret_types.to_vec(),
                        concret_types.clone().into(),
                    ))
                    .set_help_data(h)
                    .set_opacity(opacity);
                Some(var)
            }
            Type::Char(val, h) => {
                let var = Var::from_name(&val.get_val()).set_help_data(h);
                Some(var)
            }
            _ => None,
        }
    }

    pub fn from_name(name: &str) -> Self {
        Var {
            name: name.to_string(),
            is_opaque: false,
            related_type: builder::empty_type(),
            help_data: HelpData::default(),
        }
    }

    pub fn to_language(self) -> Lang {
        Lang::Variable(self.name, self.is_opaque, self.related_type, self.help_data)
    }

    pub fn set_name(self, s: &str) -> Var {
        Var {
            name: s.to_string(),
            is_opaque: self.is_opaque,
            related_type: self.related_type,
            help_data: self.help_data,
        }
    }

    pub fn set_type(self, typ: Type) -> Var {
        let typ = match typ {
            Type::Function(params, _, h) => {
                if params.len() >= 1 {
                    params[0].clone()
                } else {
                    Type::Any(h)
                }
            }
            _ => typ,
        };
        Var {
            name: self.name,
            is_opaque: self.is_opaque,
            related_type: typ,
            help_data: self.help_data,
        }
    }

    pub fn set_type_raw(self, typ: Type) -> Var {
        Var {
            name: self.name,
            is_opaque: self.is_opaque,
            related_type: typ,
            help_data: self.help_data,
        }
    }

    pub fn set_opacity(self, opa: bool) -> Var {
        Var {
            name: self.name,
            is_opaque: opa,
            related_type: self.related_type,
            help_data: self.help_data,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.to_string()
    }

    pub fn get_type(&self) -> Type {
        self.related_type.clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.help_data.clone()
    }

    pub fn match_with(&self, var: &Var, context: &Context) -> bool {
        (self.get_name() == var.get_name()) && self.get_type().is_subtype(&var.get_type(), context)
    }

    pub fn set_help_data(self, h: HelpData) -> Var {
        Var {
            name: self.name,
            is_opaque: self.is_opaque,
            related_type: self.related_type,
            help_data: h,
        }
    }

    pub fn is_imported(&self) -> bool {
        self.is_variable() && self.is_opaque.clone()
    }

    pub fn is_alias(&self) -> bool {
        match self.get_type() {
            Type::Params(_, _) => true,
            _ => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        !self.is_alias()
    }

    pub fn is_opaque(&self) -> bool {
        self.is_alias() && self.is_opaque
    }

    pub fn get_opacity(&self) -> bool {
        self.is_opaque
    }

    pub fn to_alias_type(self) -> Type {
        Type::Alias(
            self.get_name(),
            vec![],
            self.get_opacity(),
            self.get_help_data(),
        )
    }

    pub fn to_alias_lang(self) -> Lang {
        Lang::Alias(
            Box::new(self.clone().to_language()),
            vec![],
            builder::unknown_function_type(),
            self.get_help_data(),
        )
    }

    pub fn to_let(self) -> Lang {
        Lang::Let(
            Box::new(self.clone().to_language()),
            builder::unknown_function_type(),
            Box::new(builder::empty_lang()),
            self.get_help_data(),
        )
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
                ty => ".".to_string() + &cont.get_class(&ty).replace("'", ""),
            };
            let new_name = if self.contains("`") {
                "`".to_string() + &self.get_name().replace("`", "") + &type_str + "`"
            } else {
                self.get_name() + &type_str
            };
            self.set_name(&new_name)
        } else {
            self
        }
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
        write!(f, "{}<{}>", self.name, self.related_type)
    }
}

impl Default for Var {
    fn default() -> Self {
        Var {
            name: "".to_string(),
            is_opaque: false,
            related_type: Type::Empty(HelpData::default()),
            help_data: HelpData::default(),
        }
    }
}

impl RTranslatable<String> for Var {
    fn to_r(&self, _: &Context) -> String {
        format!("{}", self.name)
    }
}

impl TryFrom<Lang> for Var {
    type Error = ();

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Variable(name, muta, typ, h) => Ok(Var {
                name,
                is_opaque: muta,
                related_type: typ,
                help_data: h,
            }),
            _ => Err(()),
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
        Var {
            name: val.to_string(),
            is_opaque: false,
            related_type: Type::Empty(HelpData::default()),
            help_data: HelpData::default(),
        }
    }
}

impl TryFrom<Type> for Var {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Char(tchar, h) => match tchar {
                Tchar::Val(name) => {
                    let var = if is_pascal_case(&name) {
                        Var::from_name(&name)
                            .set_help_data(h)
                            .set_type(builder::params_type())
                    } else {
                        Var::from_name(&name).set_help_data(h)
                    };
                    Ok(var)
                }
                _ => todo!(),
            },
            _ => Err("From type to Var, not possible".to_string()),
        }
    }
}
