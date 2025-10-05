use crate::Adt;
use crate::Lang;
use crate::Context;
use crate::typing;
use crate::help_data::HelpData;
use crate::graph::TypeSystem;
use crate::type_checker::TypeChecker;

#[derive(Debug)]
pub struct AdtManager {
    pub body: Adt,
    pub header: Adt
}

//main
impl AdtManager {
    pub fn new() -> AdtManager {
        AdtManager {
            body: Adt(vec![]),
            header: Adt(vec![])
        }
    }

    pub fn add_to_body(self, adt: Adt) -> AdtManager {
        AdtManager {
            body: self.body.add(adt),
            ..self
        }
    }

    pub fn add_to_header(self, adt: Adt) -> AdtManager {
        let new_adt = adt.iter()
            .flat_map(|x| if let Lang::Let(_ , _, body, _) = x {Some((x.clone(), body))} else {None})
            .collect::<Vec<_>>();
        let header = new_adt.iter()
            .filter(|(_, body)| body.is_undefined())
            .map(|(x, _)| x)
            .cloned().collect::<Vec<_>>();
        let body = new_adt.iter()
            .filter(|(_, body)| !body.is_undefined())
            .map(|(x, _)| x)
            .cloned().collect::<Vec<_>>();
        AdtManager {
            body: self.body.add(Adt(body)),
            header: self.header.add(Adt(header))
        }
    }

    pub fn get_body(&self) -> Adt {
        self.body.clone()
    }

    pub fn get_header(&self) -> Adt {
        self.header.clone()
    }

    pub fn set_body(self, adt: Adt) -> AdtManager {
        AdtManager {
            body: adt,
            ..self
        }
    }

    pub fn type_check(&self) -> TypeChecker {
        let base_context = Context::default();
        let lang = Lang::Lines(self.get_body().0.clone(), HelpData::default());
        let type_checker = TypeChecker::new(base_context).typing(&lang);
        println!("Type checking:\n{}\n", type_checker.get_type().pretty());
        type_checker
        //let context = typing(&base_context, &Lang::Lines(self.get_header().0.clone(), HelpData::default())).2;
        //let (typ, _langs, new_context) = typing(&base_context, &Lang::Lines(self.get_body().0.clone(), HelpData::default()));
        //new_context
    }

}
