#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::error_message::help_data::HelpData;
use crate::processes::parsing::elements::is_pascal_case;
use crate::components::r#type::r#type::Type;
use crate::components::lang::var::Var;
use crate::utils::builder;


#[derive(Debug)]
pub struct ModuleType {
   args: Vec<ArgumentType>,
   help_data: HelpData
}

impl ModuleType {
    pub fn get_type_from_name(&self, name: &str) -> Result<Type, String> {
        self.args.iter()
            .find(|arg_typ| arg_typ.get_argument_str() == name)
            .map(|arg_typ| arg_typ.get_type())
            .ok_or("Alias not available in the module".to_string())
    }

    pub fn get_aliases(&self) -> Vec<(Var, Type)> {
       self.args.iter()
           .filter(|arg_typ| is_pascal_case(&arg_typ.get_argument_str()))
           .map(|arg_typ| 
                (Var::from_name(&arg_typ.get_argument_str()).set_type(builder::params_type()),
                arg_typ.get_type()))
           .collect()
    }

}

impl From<(Vec<ArgumentType>, HelpData)> for  ModuleType {
   fn from(val: (Vec<ArgumentType>, HelpData)) -> Self {
       ModuleType {
           args: val.0,
           help_data: val.1
       }
   } 
}

