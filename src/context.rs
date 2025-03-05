use crate::r#type::Type;
use crate::language::Lang;
use crate::var::Var;
use std::collections::HashSet;
use crate::kind::Kind;

#[derive(Debug, Clone)]
pub struct Context {
   types: Vec<(Var, Type)>,
   kinds: Vec<(Type, Kind)>
}

impl Default for Context {
    fn default() -> Self {
        Context { types: vec![], kinds: vec![] }
    }
}

impl From<Vec<(Lang, Type)>> for  Context {
   fn from(val: Vec<(Lang, Type)>) -> Self {
       let val2: Vec<(Var, Type)> = val.iter()
           .map(|(lan, typ)| { 
                (Var::from_language(lan.clone()).unwrap(), typ.clone())})
           .collect();
        Context { types: val2, kinds: vec![] }
   } 
}

impl Context {
    pub fn new(types: Vec<(Var, Type)>, kinds: Vec<(Type, Kind)>) -> Context {
        Context {
            types: types,
            kinds: kinds
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
            match type_ {
                Type::Function(_, args, ret)
                    => {
                        let mut sol = args.clone();
                        sol.push((**ret).clone()); sol
                    }
                typ => vec![typ.clone()]
            }
        }).collect::<Vec<_>>();
        let mut seen = HashSet::new();
        let unique: Vec<_> = res.into_iter()
            .filter(|x| seen.insert((*x).clone()))
            .collect();
        unique
        }

    pub fn push_type(self, lang: Var, typ: Type) -> Context {
        Context {
            types: self.types.iter().chain([(lang, typ)].iter()).cloned().collect(),
            ..self
        }
    }

    pub fn get_type_from_variable(&self, var: Var) -> Type {
       self.types.iter()
           .find_map(|(v, ty)| var.match_with(v, self).then(|| ty))
           .unwrap().clone()
    }

    pub fn get_type_map(&self) -> Vec<(Var, Type)> {
        self.types.clone()
    }

    pub fn get_kind_map(&self) -> Vec<(Type, Kind)> {
        self.kinds.clone()
    }
    
}
