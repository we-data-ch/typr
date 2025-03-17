use crate::r#type::Type;
use crate::language::Lang;
use crate::var::Var;
use std::collections::HashSet;
use crate::kind::Kind;
use crate::subtypes::Subtypes;
use crate::nominal_context::TypeNominal;

#[derive(Debug, Clone)]
pub struct VarType(Vec<(Var, Type)>);

impl VarType {
    fn new() -> VarType {
        VarType(vec![])
    }

    fn iter(&self) -> std::slice::Iter<(Var, Type)> {
        self.0.iter()
    }

    fn get_functions(&self, t: &Type) -> Vec<(Var, Type)> {
        self.0.iter().filter(|(var, typ)| var.get_type() == *t).cloned().collect()
    }

    pub fn get_types(&self) -> HashSet<Type> {
        self.0.iter().flat_map(|(_var, typ)| typ.clone().type_extraction()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct Context {
   pub types: VarType,
   kinds: Vec<(Type, Kind)>,
   nominals: TypeNominal,
   pub subtypes: Subtypes
}

impl Default for Context {
    fn default() -> Self {
        Context { 
            types: VarType::new(),
            kinds: vec![],
            nominals: TypeNominal::new(),
            subtypes: Subtypes::new()
        }
    }
}

impl From<Vec<(Lang, Type)>> for  Context {
   fn from(val: Vec<(Lang, Type)>) -> Self {
       let val2: Vec<(Var, Type)> = val.iter()
           .map(|(lan, typ)| { 
                (Var::from_language(lan.clone()).unwrap(), typ.clone())})
           .collect();
        Context { 
            types: VarType(val2),
            kinds: vec![],
            nominals: TypeNominal::new(),
            subtypes: Subtypes::new()
        }
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
    pub fn new(types: Vec<(Var, Type)>, kinds: Vec<(Type, Kind)>) -> Context {
        Context {
            types: VarType(types),
            kinds: kinds,
            nominals: TypeNominal::new(),
            subtypes: Subtypes::new()
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
        self.types.0.iter()
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

    pub fn push_type(self, lang: Var, typ: Type, context: &Context) -> Context {
        let types = typ.type_extraction();
        let res = VarType(self.types.iter().chain([(lang, typ.clone())].iter()).cloned().collect());
        let type_list: Vec<_> = res.get_types().iter().cloned().collect();
        let new_subtypes = self.subtypes.update(&type_list, context);
        Context {
            types: res, 
            nominals: types.iter().fold(self.nominals, |nom, typ_| nom.push_type(typ_.clone())),
            subtypes: new_subtypes,
            ..self
        }
    }

    pub fn get_type_from_variable(&self, var: Var) -> Type {
        self.types.iter()
           .find(|(v, _)| var.match_with(v, self))
           .map(|(_, ty)| ty)
           .expect(&format!("The variable {}, wasn't found in the context", var))
           .clone()
    }

    pub fn get_type_map(&self) -> Vec<(Var, Type)> {
        self.types.0.clone()
    }

    pub fn get_kind_map(&self) -> Vec<(Type, Kind)> {
        self.kinds.clone()
    }

    pub fn get_class(&self, t: &Type) -> String {
        self.nominals.get_class(t)
    }

    pub fn get_classes(&self, t: &Type) -> Option<String> {
        self.subtypes.get_supertypes(t)
            .into_iter().map(|typ| self.nominals.get_class(&typ))
            .reduce(|acc, x| format!("'{}', '{}'", acc, x))
    }

    pub fn get_supertypes(&self, t: &Type) -> Vec<Type> {
        self.subtypes.get_supertypes(t)
    }

    pub fn get_functions(&self, t: &Type) -> Vec<(Var, Type)> {
        self.get_supertypes(t).iter()
            .chain([t.clone()].iter()).flat_map(|typ| self.types.get_functions(typ))
            .collect()
    }

    pub fn update_subtypes(&self) -> Context {
        let type_list: Vec<_> = self.types.get_types().iter().cloned().collect();
        let new_subtypes = self.subtypes.clone().update(&type_list, self);
        Context {
            subtypes: new_subtypes,
            ..self.clone()
        }
    }
    
}
