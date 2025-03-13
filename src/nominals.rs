use crate::nominal_context::get_subtype_relation;
use crate::nominal_context::TypeNominal;
use crate::NominalContext;
use crate::Type;
use crate::Context;

#[derive(Debug, Clone)]
struct SuperNominals(Vec<Vec<String>>);

impl SuperNominals {
    fn new() -> SuperNominals {
        SuperNominals(vec![vec![]])
    }
}

#[derive(Debug, Clone)]
pub struct Nominals {
    manager: TypeNominal,
    context: SuperNominals
}

impl Nominals {
    pub fn new() -> Nominals {
        Nominals {
            manager: TypeNominal::new(),
            context: SuperNominals::new() 
        }
    }

    pub fn push_type(self, t: Type) -> Nominals {
        let new_manager = self.manager.register_type(t);
        Nominals {
            manager: new_manager,
            ..self
        }
    }

    pub fn update_super_types(self, con: &Context) -> Nominals {
        Nominals {
            context: SuperNominals(get_subtype_relation(self.manager.get_types(), con, &self.manager)),
            ..self
        }
    }

}


//impl From<&Context> for NominalContext {
   //fn from(con: &Context) -> Self {
       //let types = con.get_types();
       //let nominals: TypeNominal = get_nominal(types.clone(), con);
       //let super_nominal = get_subtype_relation(types.clone(), con, &nominals);
       //NominalContext(types.iter().cloned()
           //.zip(nominals.body.iter().map(|(_, nom)| nom.0.clone()))
           //.zip(super_nominal.iter().cloned())
           //.map(|((typ, nom), sup)| (typ, nom, sup))
           //.collect::<Vec<_>>())
   //} 
//}
