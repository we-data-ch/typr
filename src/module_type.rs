#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use std::collections::HashSet;
use crate::help_data::HelpData;
use crate::argument_type::ArgumentType;
use crate::Type;

pub struct ModuleType {
   args: HashSet<ArgumentType>,
   help_data: HelpData
}

impl ModuleType {
    pub fn get_type_from_name(&self, name: &str) -> Result<Type, String> {
        self.args.iter()
            .find(|arg_typ| arg_typ.get_argument_str() == name)
            .map(|arg_typ| arg_typ.get_type())
            .ok_or("Alias not available in the module".to_string())
    }
}

impl From<(HashSet<ArgumentType>, HelpData)> for  ModuleType {
   fn from(val: (HashSet<ArgumentType>, HelpData)) -> Self {
       ModuleType {
           args: val.0,
           help_data: val.1
       }
   } 
}

