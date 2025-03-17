use crate::nominal_context::TypeNominal;
use crate::Type;

#[derive(Debug, Clone)]
pub struct Nominals {
    type_class: TypeNominal,
}

impl Nominals {
    pub fn new() -> Nominals {
        Nominals {
            type_class: TypeNominal::new(),
        }
    }

    pub fn push_type(self, t: Type) -> Nominals {
        let new_type_class = self.type_class.register_type(t);
        Nominals {
            type_class: new_type_class,
            ..self
        }
    }

    pub fn get_class(&self, t: &Type) -> String {
        let vec = self.type_class.body.clone();
        vec.iter().find(|(typ, _nomi)| typ == t).unwrap().1.0.clone()
    }

}
