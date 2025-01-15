use crate::types::Type;
use crate::language::Lang;
use crate::var::Var;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Context(Vec<(Lang, Type)>);

impl Default for Context {
    fn default() -> Self {
        Context(vec![])
    }
}

impl From<Vec<(Lang, Type)>> for  Context {
   fn from(val: Vec<(Lang, Type)>) -> Self {
        Context(val)
   } 
}

impl Context {
    pub fn get(&self, var: &Var) -> Option<Type> {
        self.iter().map(|(lang, type_)| {
            match lang {
                Lang::Variable(name, path, perm, bo, typ)
                    if Lang::Variable(name.clone(), path.clone(), *perm, *bo, typ.clone()) == var.clone().to_language()
                        => Some(type_.clone()),
                    _ => None
            }
        }).flatten().next()
    }

    pub fn iter(&self) -> std::slice::Iter<(Lang, Type)> {
        self.0.iter()
    }

    pub fn get_types(&self) -> Vec<Type> {
        let res = self.iter().flat_map(|(_lang, type_)| {
            match type_ {
                Type::Function(args, ret)
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
}
