use crate::r#type::Type;
use crate::language::Lang;
use crate::var::Var;
use std::collections::HashSet;
use crate::kind::Kind;
use crate::subtypes::Subtypes;
use crate::nominal_context::TypeNominal;
use crate::argument_type::ArgumentType;

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
   pub subtypes: Subtypes,
   pub adt: Vec<Lang>
}

impl Default for Context {
    fn default() -> Self {
        Context { 
            types: VarType::new(),
            kinds: vec![],
            nominals: TypeNominal::new(),
            subtypes: Subtypes::new(),
            adt: vec![]
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
            subtypes: Subtypes::new(),
            adt: vec![]
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
            subtypes: Subtypes::new(),
            adt: vec![]
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
        let new_subtypes = self.subtypes.clone().update(&type_list, context);
        let nominals = types.iter()
            .fold(self.nominals.clone(), |nom, typ_| nom.push_type(typ_.clone()));
        Context {
            types: res, 
            nominals: nominals.clone(),
            subtypes: new_subtypes,
            adt: self.clone().add_to_adt(&wasm_types(&types, &nominals)).adt,
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
            .map(|x| format!("'{}'", x))
            .reduce(|acc, x| format!("{}, {}", acc, x))
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

    pub fn get_embeddings(&self, t: &Type) -> (Vec<(Var, Type)>, Context) {
        match t {
            Type::Record(arg_typs) => {
                let new_t = t.clone().without_embeddings();
                let type_functions = arg_typs.iter()
                    .filter(|arg_typ| arg_typ.is_embedded())
                    .map(|arg_typ| arg_typ.remove_embeddings())
                    .map(|arg_typ| (arg_typ.get_argument(), arg_typ.get_type()))
                    .flat_map(|(arg, ty)| {
                        let funcs = self.get_functions(&ty);
                        funcs.iter().map(|(var, fun)| (arg.clone(), var.clone(), fun.clone())).collect::<Vec<_>>()
                    })
                    .map(|(arg, var, fun): (String, Var, Type)| 
                         (arg, var.clone().set_type(new_t.clone()),
                         fun.clone().replace_function_types(var.get_type(), new_t.clone())))
                    .collect::<Vec<_>>();
                let new_cont = 
                    type_functions.iter()
                    .fold(self.clone(), |ctx, tf| ctx.clone().push_type(tf.1.clone(), tf.2.clone(), &ctx));
                let new_cont2 = new_cont.clone().add_to_adt(&self.build_concret_functions(&type_functions));
                (type_functions.iter().map(|(arg, var, fun)| (var.clone(), fun.clone())).collect(),
                new_cont2)
            },
            _ => (vec![], Context::default())
        }
    }

    pub fn add_to_adt(self, data: &[Lang]) -> Context {
        Context {
            adt: data.iter().fold(self.adt, |adt, lang| add_if_absent(adt, lang.clone())),
            ..self
        }
    }

    fn build_concret_functions(&self, var_typ: &[(String, Var, Type)]) -> Vec<Lang> {
        var_typ.iter().map(|(par, var, typ)| {
            let t = var.get_type();
            match typ {
                Type::Function(kinds, args, t2) => {
                   let manips = args.iter().enumerate()
                       .map(|(i, argtyp)| manip(&generate_arg(i), argtyp.clone(), t.clone(), par))
                       .collect::<Vec<_>>();
                   let t_end = (**t2).clone();
                   let manip1 = if t_end == t { Manip::Set("a".to_string(), par.to_string()) } else {Manip::Same("a".to_string())};
                   let new_args = args.iter()
                       .map(|ty| if *ty == t {typ.clone()} else { ty.clone() } )
                       .enumerate()
                       .map(|(i, typ)| ArgumentType::new(&generate_arg(i), &typ.clone()))
                       .collect::<Vec<_>>();
                   let new_t2 = if t_end == t { typ.clone() } else {t_end.clone()};
                   Lang::Let(
                       var.clone(),
                        Type::Empty,
                        Box::new(
                           Lang::Function(kinds.to_vec(), new_args, new_t2,
                                          Box::new(build_concret_function(&manips, manip1, var.clone())))
                                )
                            )
                },
                _ => todo!()
            }
        }).collect()
    }

    pub fn get_type_from_class(&self, class: &str) -> Type {
        self.nominals.get_type_from_class(class)
    }
}

fn build_concret_function(m: &[Manip], end: Manip, name: Var) -> Lang {
    let args = m.iter()
        .map(|x| x.to_lang())
        .collect::<Vec<_>>();
    let first = args.iter().nth(0).unwrap().clone();
    match end {
        Manip::Set(_, param) => {
            Lang::FunctionApp(
                Box::new(Var::from_name("set").to_language()),
                vec![
                    Var::from_name("a").to_language(),
                    Lang::Char(param.to_string()),
                    Lang::FunctionApp(Box::new(name.to_language()), args)
                ]
            )
        },
        _ => {
            Lang::FunctionApp(Box::new(name.to_language()), args)
        }
    }
}


fn manip(arg: &str, t1: Type, t2: Type, par: &str) -> Manip {
   if t1 == t2 {
       Manip::Get(arg.to_string(), par.to_string())
   } else {
        Manip::Same(arg.to_string())
   }
}

type ArgName = String;
type Field = String;

enum Manip {
    Get(ArgName, Field),
    Set(ArgName, Field),
    Same(ArgName),
    Empty
}

impl Manip {
    fn to_lang(&self) -> Lang {
        match self {
            Manip::Same(argname) => Var::from_name(&argname).to_language(),
            Manip::Get(argname, field) => {
                Lang::FunctionApp(
                    Box::new(Var::from_name("get").to_language()),
                    vec![
                        Var::from_name(&argname).to_language(),
                        Lang::Char(field.to_string())
                    ])
            },
            _ => todo!()
        }
    }
}

fn generate_arg(mut num: usize) -> String {
    match num {
        0 => "a",
        1 => "b",
        2 => "c",
        3 => "d",
        4 => "e",
        5 => "f",
        6 => "g",
        7 => "h",
        8 => "i",
        9 => "j",
        10 => "k",
        _ => "overflow"
    }.to_string()
}

fn add_if_absent(mut vec: Vec<Lang>, val: Lang) -> Vec<Lang> {
    if !vec.contains(&val) {
        vec.push(val);
    }
    vec // Retourne le nouveau vecteur
}

fn wasm_types(types: &[Type], nominals: &TypeNominal) -> Vec<Lang> {
    vec![]
}
