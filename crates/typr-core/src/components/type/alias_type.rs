use crate::components::r#type::HelpData;
use crate::components::r#type::Type;
use std::sync::atomic::{AtomicU32, Ordering};

static INTERFACE_COUNTER: AtomicU32 = AtomicU32::new(0);

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
        let counter = INTERFACE_COUNTER.fetch_add(1, Ordering::SeqCst);
        let name = format!("Interface{counter}");
        Alias {
            name,
            params: vec![],
            opacity: false,
            help_data: HelpData::default(),
        }
    }
}
