use serde::Serialize;
use crate::Lang;
use std::fmt;
use crate::Context;
use crate::translatable::RTranslatable;
use crate::Type;
use crate::builder;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ArgumentValue(pub String, pub Lang);

impl ArgumentValue {
    pub fn get_argument(&self) -> String {
        self.0.clone()
    }

    pub fn get_value(&self) -> Lang {
        self.1.clone()
    }
}

impl fmt::Display for ArgumentValue {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let empty = builder::empty_type();
        let cont = Context::new(vec![], vec![]);
        write!(f, "[var('{}'),{}]", self.0, self.1.to_r(empty, &cont).0)       
    }
}

impl RTranslatable<String> for ArgumentValue {
    fn to_r(&self, _: Type, cont: &Context) -> String {
        let empty = builder::empty_type();
        format!("{} = {}", self.0, self.1.to_r(empty, cont).0)
    }

}

