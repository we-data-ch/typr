use crate::processes::transpiling::translatable::RTranslatable;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::context::config::Environment;
use crate::processes::type_checking::typing;
use crate::components::context::Context;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use crate::utils::builder;
use rpds::Vector;

#[derive(Debug, Clone)]
pub struct TypeChecker {
    context: Context,
    code: Vector<Lang>,
    types: Vector<Type>,
    last_type: Type
}

impl TypeChecker {
    pub fn new(context: Context) -> Self {
        Self {
            context: context,
            code: Vector::new(),
            types: Vector::new(),
            last_type: builder::unknown_function()
        }
    }

    pub fn typing(self, exp: &Lang) -> Self {
        match exp {
            Lang::Lines(exps, _) => {
                let type_checker = exps.iter()
                    .fold(self.clone(), |acc, lang| acc.typing_helper(lang));
                println!("Typing:\n{}\n", type_checker.last_type.pretty());
                type_checker
            },
            _ => self.clone().typing_helper(exp)
        }
    }

    fn typing_helper(self, exp: &Lang) -> Self {
        let (typ, lang, context) = typing(&self.context, exp).to_tuple();
        Self {
            context: context,
            code: self.code.push_back(lang),
            types: self.types.push_back(typ.clone()),
            last_type: typ
        }
    }

    pub fn get_context(&self) -> Context {
        self.context.clone()
    }

    pub fn transpile(self) -> String {
        let code = self.code.iter()
            .zip(self.types.iter())
            .map(|(lang, _)| lang.to_r(&self.context).0)
            .collect::<Vec<_>>().join("\n");
        let import = match self.get_environment() {
            Environment::Project | Environment::Repl => "",
            Environment::StandAlone => "source('a_std.R', echo = FALSE)"
        };

        format!("{}\n\n{}", import, code)
    }

    fn get_environment(&self) -> Environment {
        self.context.get_environment()
    }

}
