use crate::processes::type_checking::Context;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::Var;

#[derive(Debug, Clone)]
pub struct TypeContext {
    pub value: Type,
    pub lang: Lang,
    pub context: Context,
}

impl TypeContext {
    pub fn with_lang(self, expr: &Lang) -> Self {
        (self.value, expr.clone(), self.context).into()
    }

    pub fn get_covariant_type(self, typ: &Type) -> Self {
        let typ = self.value.get_covariant_type(typ, &self.context);
        (typ, self.lang, self.context).into()
    }

    pub fn add_to_context(self, var: Var) -> Self {
        let (typ, context) = self.value.add_to_context(var, self.context);
        (typ, self.lang, context).into()
    }

    pub fn get_expr(&self) -> Lang {
        self.lang.clone()
    }

    pub fn to_tuple(self) -> (Type, Lang, Context) {
        (self.value, self.lang, self.context)
    }
}

impl From<(Type, Lang, Context)> for TypeContext {
    fn from(val: (Type, Lang, Context)) -> Self {
        Self {
            value: val.0,
            lang: val.1,
            context: val.2,
        }
    }
}
