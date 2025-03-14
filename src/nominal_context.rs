use std::collections::HashMap;
use crate::context::Context;
use crate::r#type::Type;
use crate::type_comparison::is_subtype;

#[derive(Debug, Clone)]
pub struct NominalContext(Vec<(Type, String, Vec<String>)>);

impl NominalContext {
    pub fn new() -> NominalContext {
        NominalContext(vec![])
    }

    pub fn get_classes(&self, t: &Type) -> String {
        let vec: Vec<_> = self.0.iter().find(|(ty, _, _)| t == ty)
            .map(|(_, _, sup)| sup.clone())
            .unwrap();
        let vec = vec.iter()
            .filter(|elem| *elem != "").collect::<Vec<_>>();
        vec.iter().map(|x| format!("'{}'", x)).collect::<Vec<_>>().join(",")
    }

    pub fn get_class(&self, t: &Type) -> String {
        self.0.iter().find(|(ty, _, _)| t == ty)
            .map(|(_, name, _)| name.clone())
            .unwrap()
    }

    pub fn push_type(self, t: Type) -> NominalContext {
        todo!();
    }

    pub fn update_supertypes(self) -> NominalContext {
        todo!();
    }
}

impl From<Vec<(Type, String, Vec<String>)>> for  NominalContext {
   fn from(val: Vec<(Type, String, Vec<String>)>) -> Self {
        NominalContext(val.clone())
   } 
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TypeCategory {
    Array,
    Function,
    Record,
    Index,
    Alias,
    Tag,
    Union,
    Interface,
    Boolean,
    Integer,
    Number,
    Char,
    Generic,
    Rest
}

impl TypeCategory {
    fn from_type(t: Type) -> TypeCategory {
        match t {
            Type::Array(_, _) => TypeCategory::Array,
            Type::Function(_, _, _) => TypeCategory::Function,
            Type::Record(_) => TypeCategory::Record,
            Type::Index(_) => TypeCategory::Index,
            Type::Alias(_, _, _) => TypeCategory::Alias,
            Type::Tag(_, _) => TypeCategory::Tag,
            Type::Union(_) => TypeCategory::Union,
            Type::Interface(_) => TypeCategory::Interface,
            Type::Boolean => TypeCategory::Boolean,
            Type::Integer => TypeCategory::Integer,
            Type::Number => TypeCategory::Number,
            Type::Char => TypeCategory::Char,
            Type::Generic(_) => TypeCategory::Generic,
            Type::IndexGen(_) => TypeCategory::Generic,
            _ => TypeCategory::Rest
        }
    }
}

use std::fmt;
impl fmt::Display for TypeCategory {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            TypeCategory::Array => "Array",
            TypeCategory::Function => "Function",
            TypeCategory::Record => "Record",
            TypeCategory::Index => "Index",
            TypeCategory::Tag => "Tag",
            TypeCategory::Alias => "Alias",
            TypeCategory::Union => "Union",
            TypeCategory::Interface => "Interface",
            TypeCategory::Boolean => "bool",
            TypeCategory::Integer => "int",
            TypeCategory::Number => "num",
            TypeCategory::Char => "char",
            TypeCategory::Generic => "Generic",
            TypeCategory::Rest => "Rest"
        };
        write!(f, "{}", res)       
    }
}

#[derive(Debug, Clone)]
struct Categories(HashMap<TypeCategory, usize>);

impl Categories {
    fn register_type(self, t: Type) -> Categories {
        let mut map = self.0;
        t.type_extraction().iter()
            .map(|x| TypeCategory::from_type(x.clone()))
            .for_each(|category| {
                map.entry(category).and_modify(|val| *val += 1);
            });
        Categories(map)
    }

    fn get_nominal(&self, t: &Type) -> Nominal {
        let category = TypeCategory::from_type(t.clone());
        let index = self.0.get(&category).unwrap();
        Nominal(format!("{}_{}", category, index))
    }
}


#[derive(Debug, Clone)]
pub struct TypeNominal {
    pub body: Vec<(Type, Nominal)>,
    categories: Categories
}

impl TypeNominal {
    pub fn new() -> Self {
        let mut categories = HashMap::new();
        categories.insert(TypeCategory::Array, 0 as usize);
        categories.insert(TypeCategory::Function, 0 as usize);
        categories.insert(TypeCategory::Record, 0 as usize);
        categories.insert(TypeCategory::Index, 0 as usize);
        categories.insert(TypeCategory::Alias, 0 as usize);
        categories.insert(TypeCategory::Tag, 0 as usize);
        categories.insert(TypeCategory::Union, 0 as usize);
        categories.insert(TypeCategory::Interface, 0 as usize);
        categories.insert(TypeCategory::Boolean, 0 as usize);
        categories.insert(TypeCategory::Integer, 0 as usize);
        categories.insert(TypeCategory::Number, 0 as usize);
        categories.insert(TypeCategory::Char, 0 as usize);
        categories.insert(TypeCategory::Generic, 0 as usize);
        TypeNominal {
            body: vec![],
            categories: Categories(categories)
        }
    }

    pub fn register_type(self, t: Type) -> TypeNominal {
        let types = t.type_extraction();
        let new_categories = types.iter().fold(self.categories, |cat, typ| {
            cat.register_type(typ.clone())
        });
        let typ_nom = types.iter().map(|typ| {
            (typ.clone(), new_categories.get_nominal(&typ))
        }).collect::<Vec<_>>();

        TypeNominal {
            body: self.body.iter().chain(typ_nom.iter()).cloned().collect::<Vec<_>>(),
            categories: new_categories
        }
    }

    pub fn get_types(&self) -> Vec<Type> {
        self.body.iter().map(|(typ, _nomi)| typ.clone()).collect()
    }

    pub fn get_index(&self, t: Type) -> usize {
        self.body.iter().enumerate().find(|(i, (typ, _))| *typ == t).unwrap().0 as usize
    }

    fn corresponding_nominal(&self, type_: &Type) -> Option<Nominal> {
        self.body.iter().find(|(typ, _)| type_ == typ)
            .map(|(_, nominal)| nominal.clone())
    }

   fn get_nth(&mut self, type_: Type) -> usize {
      match type_ {
          Type::Array(_, _) => self.get_nth_helper(TypeCategory::Array),
          Type::Function(_, _, _) => self.get_nth_helper(TypeCategory::Function),
          Type::Record(_) => self.get_nth_helper(TypeCategory::Record),
          Type::Number | Type::Integer | Type::Char | Type::Boolean => 0 as usize,
          Type::Embedded(_) | Type::Generic(_) | Type::IndexGen(_) => 0 as usize,
          Type::Index(_) => self.get_nth_helper(TypeCategory::Index),
          Type::Alias(_, _, _) => self.get_nth_helper(TypeCategory::Alias),
          Type::Tag(_, _) => self.get_nth_helper(TypeCategory::Tag),
          Type::Union(_) => self.get_nth_helper(TypeCategory::Union),
          Type::Interface(_) => self.get_nth_helper(TypeCategory::Interface),
          Type::Failed(_) | Type::Params(_) | Type::Empty => 0 as usize,
          Type::Add(_, _) | Type::Mul(_, _) | Type::Div(_, _) | Type::Minus(_, _) => 0 as usize,
          _ => self.get_nth_helper(TypeCategory::Rest)
      }
   } 

   fn get_nth_helper(&mut self, category: TypeCategory) -> usize {
       self.incr(category); self.get(category)
   }

   fn incr(&mut self, category: TypeCategory) -> () {
       self.categories.0.insert(category, *self.categories.0.get(&category).unwrap() + 1);
   }

   fn get(&self, category: TypeCategory) -> usize {
       *self.categories.0.get(&category).unwrap()
   }

   fn push(&mut self, type_nominal: (Type, Nominal)) {
       self.body.push(type_nominal)
   }

   fn new_nominal(&mut self, type_: Type) -> Nominal {
        Nominal::from((type_.clone(), self.get_nth(type_)))
   }

}

#[derive(Debug, PartialEq, Clone)]
pub struct Nominal(pub String);

impl From<(Type, usize)> for Nominal {
   fn from(val: (Type, usize)) -> Self {
       let category = TypeCategory::from_type(val.0);
       match category {
           TypeCategory::Rest => Nominal("".to_string()),
           _ => Nominal(format!("{}_{}", category, val.1))
       }
   } 
}

fn get_nominal(types: Vec<Type>, _con: &Context) -> TypeNominal {
    types.iter().fold(TypeNominal::new(), |mut acc, type_| {
        let res = match acc.corresponding_nominal(type_) {
            Some(nominal) => (type_.clone(), nominal),
            None => (type_.clone(), acc.new_nominal(type_.clone()))
        };
        acc.push(res); acc
    })
}

pub fn get_subtype_relation(types: Vec<Type>, con: &Context, nominals: &TypeNominal) -> Vec<Vec<String>> {
    let types_ref = types.clone();
    types.iter().map(|typ| {
        types_ref.iter()
            .filter(|typ2| is_subtype(con, typ, typ2))
            .map(|typ3| nominals.corresponding_nominal(typ3).unwrap().0)
            .collect::<Vec<_>>()
    }).collect::<Vec<_>>()
}

impl From<&Context> for NominalContext {
   fn from(con: &Context) -> Self {
       let types = con.get_types();
       let nominals: TypeNominal = get_nominal(types.clone(), con);
       let super_nominal = get_subtype_relation(types.clone(), con, &nominals);
       NominalContext(types.iter().cloned()
           .zip(nominals.body.iter().map(|(_, nom)| nom.0.clone()))
           .zip(super_nominal.iter().cloned())
           .map(|((typ, nom), sup)| (typ, nom, sup))
           .collect::<Vec<_>>())
   } 
}
