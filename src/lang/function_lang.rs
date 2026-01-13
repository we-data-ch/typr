#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::Type;
use crate::Lang;
use crate::help_data::HelpData;
use crate::r#type::argument_type::ArgumentType;

pub struct Function {
    arg_types: Vec<ArgumentType>,
    return_type: Type,
    body: Box<Lang>,
    help_data: HelpData
}

impl Function {
    pub fn new(arg_types: Vec<ArgumentType>, return_type: Type, body: Box<Lang>, help_data: HelpData) -> Function {
        Function { arg_types, return_type, body, help_data }
    }

    pub fn get_arg_types(&self) -> Vec<ArgumentType> {
        self.arg_types.clone()
    }

    pub fn get_return_type(&self) -> Type {
        self.return_type.clone()
    }

    pub fn get_body(&self) -> Lang {
        (*self.body).clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.help_data.clone()
    }

}

impl TryFrom<Lang> for Function {
    type Error = ();

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Function(arg_types, return_type, body, help_data) 
                => Ok(Function::new(arg_types, return_type, body, help_data)),
            _ => Err(())
        }
    }
}

