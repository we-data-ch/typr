use crate::components::context::Context;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::Lang;
use std::ops::Add;

pub trait TranslateAppendable {
    fn to_translatable(self) -> Translatable;
}

pub trait RTranslatable<T: TranslateAppendable> {
    fn to_r(&self, context: &Context) -> T;
}

pub struct Translatable {
    context: Context,
    code: String,
}

impl Translatable {
    pub fn to_r<T: TranslateAppendable>(self, lang: &impl RTranslatable<T>) -> Translatable {
        let res = lang.to_r(&self.context);
        self.append(res)
    }

    pub fn to_r_safe<T: TranslateAppendable>(self, lang: &impl RTranslatable<T>) -> Translatable {
        let res = lang.to_r(&self.context);
        self.append_safe(res)
    }

    pub fn reset_context(self) -> Translatable {
        Translatable {
            context: Context::default(),
            ..self
        }
    }

    pub fn to_r_arg_val(self, arg_val: &ArgumentValue, joint: &str) -> Translatable {
        let res = arg_val.to_r(&self.context);
        self.add(&res).add(joint)
    }

    pub fn add(self, s: &str) -> Translatable {
        Translatable {
            code: self.code + s,
            ..self
        }
    }

    pub fn append(self, val: impl TranslateAppendable) -> Translatable {
        self + val.to_translatable()
    }

    pub fn append_safe(self, val: impl TranslateAppendable) -> Translatable {
        self + val.to_translatable().reset_context()
    }

    pub fn get_code(&self) -> String {
        self.code.clone()
    }

    pub fn join(self, vals: &[Lang], joint: &str) -> Translatable {
        if vals.len() > 0 {
            vals.into_iter()
                .fold(self, |trans, val| trans.to_r(val).add(joint))
                .sub(joint.len())
        } else {
            self
        }
    }

    pub fn join_arg_val(self, vals: &[ArgumentValue], joint: &str) -> Translatable {
        vals.into_iter()
            .fold(self, |trans, val| trans.to_r_arg_val(val, joint))
            .sub(joint.len())
    }

    pub fn sub(self, len: usize) -> Translatable {
        let new_code = if self.code.len() > len {
            &self.code[0..(self.code.len() - len)]
        } else {
            &self.code
        };
        Translatable {
            code: new_code.to_string(),
            ..self
        }
    }
}

impl Add for Translatable {
    type Output = Translatable;

    fn add(self, other: Self) -> Self::Output {
        let new_context = (other.context == Context::default())
            .then_some(self.context)
            .unwrap_or(other.context);
        Translatable {
            context: new_context,
            code: self.code + &other.code,
        }
    }
}

impl From<Context> for Translatable {
    fn from(val: Context) -> Self {
        Translatable {
            context: val,
            code: "".to_string(),
        }
    }
}

impl From<Translatable> for (String, Context) {
    fn from(val: Translatable) -> Self {
        (val.code, val.context)
    }
}

impl TranslateAppendable for (String, Context) {
    fn to_translatable(self) -> Translatable {
        Translatable {
            context: self.1,
            code: self.0,
        }
    }
}

impl TranslateAppendable for String {
    fn to_translatable(self) -> Translatable {
        Translatable {
            context: Context::default(),
            code: self,
        }
    }
}

impl RTranslatable<(String, Context)> for Box<Lang> {
    fn to_r(&self, context: &Context) -> (String, Context) {
        (**self).to_r(context)
    }
}
