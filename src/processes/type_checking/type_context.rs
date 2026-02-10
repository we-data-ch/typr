use crate::components::error_message::typr_error::TypRError;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::Var;

#[derive(Debug, Clone)]
pub struct TypeContext {
    pub value: Type,
    pub lang: Lang,
    pub context: Context,
    pub errors: Vec<TypRError>,
}

impl TypeContext {
    pub fn new(value: Type, lang: Lang, context: Context) -> Self {
        Self {
            value,
            lang,
            context,
            errors: Vec::new(),
        }
    }

    pub fn with_errors(mut self, errors: Vec<TypRError>) -> Self {
        self.errors.extend(errors);
        self
    }

    pub fn push_error(&mut self, error: TypRError) {
        self.errors.push(error);
    }

    pub fn extend_errors(&mut self, errors: Vec<TypRError>) {
        self.errors.extend(errors);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn get_errors(&self) -> &Vec<TypRError> {
        &self.errors
    }

    pub fn into_errors(self) -> Vec<TypRError> {
        self.errors
    }

    pub fn with_lang(self, expr: &Lang) -> Self {
        Self {
            value: self.value,
            lang: expr.clone(),
            context: self.context,
            errors: self.errors,
        }
    }

    pub fn get_covariant_type(self, typ: &Type) -> Self {
        let typ = self.value.get_covariant_type(typ, &self.context);
        Self {
            value: typ,
            lang: self.lang,
            context: self.context,
            errors: self.errors,
        }
    }

    pub fn add_to_context(self, var: Var) -> Self {
        let (typ, context) = self.value.add_to_context(var, self.context);
        Self {
            value: typ,
            lang: self.lang,
            context,
            errors: self.errors,
        }
    }

    pub fn get_expr(&self) -> Lang {
        self.lang.clone()
    }

    pub fn to_tuple(self) -> (Type, Lang, Context) {
        (self.value, self.lang, self.context)
    }

    pub fn to_tuple_with_errors(self) -> (Type, Lang, Context, Vec<TypRError>) {
        (self.value, self.lang, self.context, self.errors)
    }
}

impl From<(Type, Lang, Context)> for TypeContext {
    fn from(val: (Type, Lang, Context)) -> Self {
        Self {
            value: val.0,
            lang: val.1,
            context: val.2,
            errors: Vec::new(),
        }
    }
}

impl From<(Type, Lang, Context, Vec<TypRError>)> for TypeContext {
    fn from(val: (Type, Lang, Context, Vec<TypRError>)) -> Self {
        Self {
            value: val.0,
            lang: val.1,
            context: val.2,
            errors: val.3,
        }
    }
}
