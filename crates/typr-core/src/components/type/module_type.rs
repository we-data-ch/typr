#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::error_message::help_data::HelpData;
use crate::components::language::var::Var;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;
use crate::processes::parsing::elements::is_pascal_case;
use crate::utils::builder;

#[derive(Debug)]
pub struct ModuleType {
    pub_members: Vec<ArgumentType>,
    priv_members: Vec<ArgumentType>,
    help_data: HelpData,
}

impl ModuleType {
    pub fn new(pub_members: Vec<ArgumentType>, priv_members: Vec<ArgumentType>, help_data: HelpData) -> Self {
        ModuleType {
            pub_members,
            priv_members,
            help_data,
        }
    }

    pub fn get_type_from_name(&self, name: &str) -> Result<Type, String> {
        self.pub_members
            .iter()
            .find(|arg_typ| arg_typ.get_argument_str() == name)
            .map(|arg_typ| arg_typ.get_type())
            .ok_or("Alias not available in the module".to_string())
    }

    pub fn get_public_members(&self) -> &Vec<ArgumentType> {
        &self.pub_members
    }

    pub fn get_private_members(&self) -> &Vec<ArgumentType> {
        &self.priv_members
    }

    pub fn is_public_member(&self, name: &str) -> bool {
        self.pub_members
            .iter()
            .any(|arg_typ| arg_typ.get_argument_str() == name)
    }

    pub fn has_private_member(&self, name: &str) -> bool {
        self.priv_members
            .iter()
            .any(|arg_typ| arg_typ.get_argument_str() == name)
    }

    pub fn get_aliases(&self) -> Vec<(Var, Type)> {
        self.pub_members
            .iter()
            .filter(|arg_typ| is_pascal_case(&arg_typ.get_argument_str()))
            .map(|arg_typ| {
                (
                    Var::from_name(&arg_typ.get_argument_str()).set_type(builder::params_type()),
                    arg_typ.get_type(),
                )
            })
            .collect()
    }
}

impl From<(Vec<ArgumentType>, Vec<String>, HelpData)> for ModuleType {
    fn from(val: (Vec<ArgumentType>, Vec<String>, HelpData)) -> Self {
        let priv_members = val
            .1
            .into_iter()
            .map(|name| ArgumentType::new(&name, &crate::utils::builder::empty_type()))
            .collect();
        ModuleType {
            pub_members: val.0,
            priv_members,
            help_data: val.2,
        }
    }
}
