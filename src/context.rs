use crate::r#type::Type;
use crate::language::Lang;
use crate::var::Var;
use std::collections::HashSet;
use crate::kind::Kind;
use crate::NominalContext;
use crate::nominals::Nominals;

#[derive(Debug, Clone)]
pub struct Context {
   types: Vec<(Var, Type)>,
   kinds: Vec<(Type, Kind)>,
   nominals: Nominals
}

impl Default for Context {
    fn default() -> Self {
        Context { types: vec![], kinds: vec![], nominals: Nominals::new() }
    }
}

impl From<Vec<(Lang, Type)>> for  Context {
   fn from(val: Vec<(Lang, Type)>) -> Self {
       let val2: Vec<(Var, Type)> = val.iter()
           .map(|(lan, typ)| { 
                (Var::from_language(lan.clone()).unwrap(), typ.clone())})
           .collect();
        Context { types: val2, kinds: vec![], nominals: Nominals::new() }
   } 
}

fn type_extraction(t: &Type) -> Vec<Type> {
    match t {
        Type::Function(_, args, ret)
            => {
                let mut sol = args.clone();
                sol.push((**ret).clone());
                sol.push(t.clone()); sol
            }
        Type::Union(tags) => {
           let mut sol = tags.iter().map(|tag| tag.to_type()).collect::<Vec<_>>();
           sol.push(t.clone()); sol
        },
        typ => vec![typ.clone()]
    }
}

impl Context {
    pub fn new(types: Vec<(Var, Type)>, kinds: Vec<(Type, Kind)>, nominals: Nominals) -> Context {
        Context {
            types: types,
            kinds: kinds,
            nominals: nominals
        }
    }

    pub fn get(&self, var: &Var) -> Option<Type> {
        self.iter().map(|(var2, type_)| {
            match var2 {
                Var(name, path, perm, bo, typ)
                    if Lang::Variable(name.clone(), path.clone(), *perm, *bo, typ.clone()) == var.clone().to_language()
                        => Some(type_.clone()),
                    _ => None
            }
        }).flatten().next()
    }

    pub fn iter(&self) -> std::slice::Iter<(Var, Type)> {
        self.types.iter()
    }

    pub fn get_types(&self) -> Vec<Type> {
        let res = self.iter().flat_map(|(_lang, type_)| {
            type_extraction(type_)
        }).collect::<Vec<_>>();
        let mut seen = HashSet::new();
        let unique: Vec<_> = res.into_iter()
            .filter(|x| seen.insert((*x).clone()))
            .collect();
        unique
        }

    pub fn push_type(self, lang: Var, typ: Type, cont: &Context) -> Context {
        Context {
            types: self.types.iter().chain([(lang, typ.clone())].iter()).cloned().collect(),
            nominals: self.nominals.push_type(typ).update_super_types(cont),
            ..self
        }
    }

    pub fn get_type_from_variable(&self, var: Var) -> Type {
        if var.get_name() == "f" {
            dbg!(&self);
        }
        self.types.iter()
           .find(|(v, _)| var.match_with(v, self))
           .map(|(_, ty)| ty)
           .expect(&format!("The variable {}, wasn't found in the context", var))
           .clone()
    }

    pub fn get_type_map(&self) -> Vec<(Var, Type)> {
        self.types.clone()
    }

    pub fn get_kind_map(&self) -> Vec<(Type, Kind)> {
        self.kinds.clone()
    }

    pub fn get_class(&self, t: &Type) -> String {
        self.nominals.get_class(t)
    }

    pub fn get_classes(&self, t: &Type) -> String {
        self.nominals.get_classes(t)
    }
    
}
