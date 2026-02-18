use crate::components::context::config::Environment;
use crate::components::context::Context;
use crate::components::language::Lang;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::transpiling::translatable::RTranslatable;
use crate::processes::type_checking::typing;
use crate::processes::type_checking::TypRError;
use crate::utils::builder;
use rpds::Vector;

#[derive(Debug, Clone)]
pub struct TypeChecker {
    context: Context,
    code: Vector<Lang>,
    types: Vector<Type>,
    last_type: Type,
    errors: Vec<TypRError>,
}

impl TypeChecker {
    pub fn new(context: Context) -> Self {
        Self {
            context: context,
            code: Vector::new(),
            types: Vector::new(),
            last_type: builder::unknown_function_type(),
            errors: vec![],
        }
    }

    pub fn has_errors(&self) -> bool {
        self.errors.len() > 0
    }

    pub fn show_errors(&self) {
        self.errors
            .iter()
            .for_each(|error| println!("{}", error.clone().display()))
    }

    pub fn typing(self, exp: &Lang) -> Self {
        let res = match exp {
            Lang::Lines(exps, _) => {
                let type_checker = exps
                    .iter()
                    .fold(self.clone(), |acc, lang| acc.typing_helper(lang));
                println!("Typing:\n{}\n", type_checker.last_type.pretty());
                type_checker
            }
            _ => self.clone().typing_helper(exp),
        };
        res.has_errors().then(|| {
            res.show_errors();
            panic!("");
        });
        res
    }

    fn typing_helper(self, exp: &Lang) -> Self {
        let (typ, lang, context, errors) = typing(&self.context, exp).to_tuple_with_errors();
        Self {
            context: context,
            code: self.code.push_back(lang),
            types: self.types.push_back(typ.clone()),
            last_type: typ,
            errors: self.errors.iter().chain(errors.iter()).cloned().collect(),
        }
    }

    pub fn get_context(&self) -> Context {
        self.context.clone()
    }

    pub fn transpile(self) -> String {
        let code = self
            .code
            .iter()
            .zip(self.types.iter())
            .map(|(lang, _)| lang.to_r(&self.context).0)
            .collect::<Vec<_>>()
            .join("\n");
        let import = match self.get_environment() {
            Environment::Project | Environment::Repl => "",
            Environment::StandAlone => "source('a_std.R', echo = FALSE)",
        };

        format!("{}\n\n{}", import, code)
    }

    fn get_environment(&self) -> Environment {
        self.context.get_environment()
    }
}
