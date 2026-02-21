#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::context::Context;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use crate::utils::builder;

#[derive(Debug, Clone, PartialEq)]
pub struct Typer {
    context: Context,
    memory: Vec<(Lang, Type)>,
    var: Var,
    typ: Type,
}

impl Typer {
    pub fn typing(self, lang: Lang) -> Self {
        let (typ, lang, cont) = lang.typing(&self.context).to_tuple();
        Self {
            context: cont,
            memory: self
                .memory
                .iter()
                .chain([(lang, typ)].iter())
                .cloned()
                .collect(),
            ..self
        }
    }

    pub fn set_var(self, var: Var) -> Self {
        Self { var, ..self }
    }

    pub fn set_type(self, typ: Type) -> Self {
        Self { typ, ..self }
    }

    pub fn push_var_type(self) -> Self {
        Self {
            context: self.context.clone().push_var_type(
                self.var.clone(),
                self.typ.clone(),
                &self.context,
            ),
            memory: self.memory,
            ..Typer::default()
        }
    }

    pub fn get_context(&self) -> Context {
        self.context.clone()
    }
}

impl Default for Typer {
    fn default() -> Typer {
        Typer {
            context: Context::default(),
            memory: vec![],
            var: Var::default(),
            typ: builder::unknown_function_type(),
        }
    }
}

impl From<Context> for Typer {
    fn from(val: Context) -> Self {
        Typer {
            context: val,
            ..Typer::default()
        }
    }
}
