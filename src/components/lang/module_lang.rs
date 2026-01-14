#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::error_message::help_data::HelpData;
use crate::components::lang::language::ModulePosition;
use crate::components::context::context::Context;
use crate::components::context::config::Config;
use crate::components::lang::language::Lang;
use crate::components::r#type::r#type::Type;

pub struct ModuleLang {
    name: String,
    members: Vec<Lang>,
    position: ModulePosition,
    config: Config,
    help_data: HelpData
}

impl ModuleLang {
    pub fn to_record(self, type_module: &Type, context: &Context) -> Lang {
        let new_args = self.members.iter()
            .flat_map(|arg| arg.clone().to_arg_value(type_module, context))
            .flatten().collect::<Vec<_>>();
        Lang::Record(new_args, self.get_help_data())
    }

    pub fn get_help_data(&self) -> HelpData {
        self.help_data.clone()
    }


}

impl TryFrom<Lang> for ModuleLang {
    type Error = String;

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Module(name, args, position, config, h) 
                => Ok(ModuleLang { 
                        name: name,
                        members: args,
                        position: position,
                        config: config,
                        help_data: h }),
            _ => Err(format!("{} can't be converted to struct ModuleLang", value.simple_print()))
        }
    }
}
