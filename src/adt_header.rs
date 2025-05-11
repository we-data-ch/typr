use crate::Lang;
use crate::Adt;

#[derive(Debug, Default, Clone)]
pub struct AdtHeader {
   pub generic_methods: Vec<Lang>,
   pub modules: Vec<Lang>
}

impl AdtHeader {
    pub fn get_adt(&self) -> Adt {
        self.modules.iter()
            .chain(self.generic_methods.iter())
            .cloned()
            .collect::<Vec<_>>().into()
    }

    pub fn set_generic_methods(&self, vec: Vec<Lang>) -> AdtHeader {
        AdtHeader {
            generic_methods: vec,
            ..self.clone()
        }
    }

    pub fn set_module(&self, vec: Vec<Lang>) -> AdtHeader {
        AdtHeader {
            modules: vec,
            ..self.clone()
        }
    }
}


