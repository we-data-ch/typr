use crate::Context;
use crate::Lang;
use crate::Type;
use crate::builder;
use crate::Var;

#[derive(Debug, Clone, PartialEq)]
pub struct Typer {
    context: Context,
    memory: Vec<(Lang, Type)>,
    var: Var,
    typ: Type
}

impl Typer {
    pub fn typing(self, lang: Lang) -> Self {
        let (typ, cont) = lang.typing(&self.context);
        Self {
            context: cont,
            memory: self.memory.iter().chain([(lang, typ)].iter()).cloned().collect(),
            ..self
        }
    }

    pub fn set_var(self, var: Var) -> Self {
        Self {
            var,
            ..self
        }
    }

    pub fn set_type(self, typ: Type) -> Self {
        Self {
            typ,
            ..self
        }
    }

    pub fn push_var_type(self) -> Self {
        Self {
            context: self.context.clone()
                .push_var_type(self.var.clone(), self.typ.clone(), &self.context),
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
           typ: builder::empty_type()
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
