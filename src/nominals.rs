use crate::nominal_context::get_subtype_relation;
use crate::nominal_context::TypeNominal;
use crate::Type;
use crate::Context;
use crate::nominal_context::TypeCategory;

#[derive(Debug, Clone)]
struct SuperNominals(Vec<Vec<String>>);

impl SuperNominals {
    fn new() -> SuperNominals {
        SuperNominals(vec![vec![]])
    }
}

#[derive(Debug, Clone)]
pub struct Nominals {
    type_class: TypeNominal,
    supertypes: SuperNominals
}

impl Nominals {
    pub fn new() -> Nominals {
        Nominals {
            type_class: TypeNominal::new(),
            supertypes: SuperNominals::new() 
        }
    }

    pub fn push_type(self, t: Type) -> Nominals {
        let new_type_class = self.type_class.register_type(t);
        Nominals {
            type_class: new_type_class,
            ..self
        }
    }

    pub fn update_super_types(self, con: &Context) -> Nominals {
        Nominals {
            supertypes: SuperNominals(get_subtype_relation(self.type_class.get_types(), con, &self.type_class)),
            ..self
        }
    }

    pub fn get_class(&self, t: &Type) -> String {
        let vec = self.type_class.body.clone();
        vec.iter().find(|(typ, _nomi)| typ == t).unwrap().1.0.clone()
    }

    pub fn get_classes(&self, t: &Type) -> String {
        let index = self.type_class.get_index(t.clone());
        let vec = self.supertypes.0[index].iter().map(|x| format!("'{}'", x)).collect::<Vec<_>>();
        vec.join(", ")
    }

}
