#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::language::Lang;
use crate::components::language::HelpData;

pub struct ArrayLang(Vec<Lang>, HelpData);

impl ArrayLang {
    pub fn get_first_argument(&self) -> Option<Lang> {
        if self.0.len() > 0 {
            Some(self.0[0].clone())
        } else {
            None
        }
    }
}

impl TryFrom<Lang> for ArrayLang {
    type Error = String;

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Array(arr, h) => Ok(ArrayLang(arr, h)),
            _ => Err(format!("{} can't be an array", value.simple_print()))
        }
    }
}

impl TryFrom<&Box<Lang>> for ArrayLang {
    type Error = String;

    fn try_from(value: &Box<Lang>) -> Result<Self, Self::Error> {
        match (**value).clone() {
            Lang::Array(arr, h) => Ok(ArrayLang(arr, h)),
            _ => Err(format!("{} can't be an array", value.simple_print()))
        }
    }
}
