use crate::components::r#type::HelpData;
use crate::components::r#type::Type;
use rand::prelude::*;

pub struct Alias {
    name: String,
    params: Vec<Type>,
    opacity: bool,
    help_data: HelpData,
}

impl Alias {
    pub fn new(name: String, params: Vec<Type>, opacity: bool, help_data: HelpData) -> Self {
        Self {
            name,
            params,
            opacity,
            help_data,
        }
    }

    pub fn set_opacity(self, val: bool) -> Self {
        Self {
            opacity: val,
            ..self
        }
    }

    pub fn to_type(self) -> Type {
        Type::Alias(self.name, self.params, self.opacity, self.help_data)
    }
}

impl Default for Alias {
    fn default() -> Self {
        let mut rng = rand::rng();

        // Version la plus lisible et la plus utilisée
        //// Generate and shuffle a sequence:
        let nums: Vec<i32> = (1..=1000).collect();
        let nombre = nums.choose(&mut rng).unwrap();
        let name = format!("Opaque{nombre}");
        Alias {
            name,
            params: vec![],
            opacity: false,
            help_data: HelpData::default(),
        }
    }
}
