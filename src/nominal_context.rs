use std::collections::HashMap;
use crate::r#type::Type;

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
            Type::LabelGen(_) => TypeCategory::Generic,
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
            TypeCategory::Boolean => "logical",
            TypeCategory::Integer => "integer",
            TypeCategory::Number => "numeric",
            TypeCategory::Char => "character",
            TypeCategory::Generic => "Generic",
            TypeCategory::Rest => "Rest"
        };
        write!(f, "{}", res)       
    }
}

#[derive(Debug, Clone)]
struct Categories(HashMap<TypeCategory, usize>);

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
        categories.insert(TypeCategory::Rest, 0 as usize);
        TypeNominal {
            body: vec![],
            categories: Categories(categories)
        }
    }

    fn get_nth(&self, type_: Type) -> (Categories, usize) {
      match type_ {
          Type::Array(_, _) => self.get_nth_helper(TypeCategory::Array),
          Type::Function(_, _, _) => self.get_nth_helper(TypeCategory::Function),
          Type::Record(_) => self.get_nth_helper(TypeCategory::Record),
          Type::Number | Type::Integer | Type::Char | Type::Boolean => (self.categories.clone(), 100 as usize),
          Type::Embedded(_) | Type::Generic(_) | Type::IndexGen(_) => (self.categories.clone(), 0 as usize),
          Type::Index(_) => self.get_nth_helper(TypeCategory::Index),
          Type::Alias(_, _, _) => self.get_nth_helper(TypeCategory::Alias),
          Type::Tag(_, _) => self.get_nth_helper(TypeCategory::Tag),
          Type::Union(_) => self.get_nth_helper(TypeCategory::Union),
          Type::Interface(_) => self.get_nth_helper(TypeCategory::Interface),
          Type::Failed(_) | Type::Params(_) | Type::Empty => (self.categories.clone(), 0 as usize),
          Type::Add(_, _) | Type::Mul(_, _) | Type::Div(_, _) | Type::Minus(_, _) => (self.categories.clone(), 0 as usize),
          _ => self.get_nth_helper(TypeCategory::Rest)
      }
   } 

   fn get_nth_helper(&self, category: TypeCategory) -> (Categories, usize) {
       (self.incr(category), self.get(category))
   }

   fn incr(&self, category: TypeCategory) -> Categories {
       let mut mut_cate = self.categories.clone();
       mut_cate.0.insert(category, *self.categories.0.get(&category).unwrap() + 1);
       mut_cate.clone()
   }

   fn get(&self, category: TypeCategory) -> usize {
       *self.categories.0.get(&category).unwrap()
   }

   fn new_nominal(&self, type_: Type) -> (Categories, Nominal) {
       let res = self.get_nth(type_.clone());
        (res.0, Nominal::from((type_, res.1)))
   }

   fn contains(&self, typ: &Type) -> bool {
       self.body.iter().find(|(typ_, _nom)| typ == typ_).is_some()
   }

   pub fn push_type(&self, typ: Type) -> TypeNominal {
       if self.contains(&typ) {
            self.clone()
       } else {
           let cat_nom = self.new_nominal(typ.clone());
           TypeNominal {
               body: self.body.iter().chain([(typ, cat_nom.1)].iter()).cloned().collect::<Vec<_>>(),
               categories: cat_nom.0
           }
       }
   }

   pub fn get_class(&self, typ: &Type) -> String {
       match typ {
           Type::Empty => "Empty".to_string(),
           Type::Any => "Empty".to_string(),
           _ => {
               self.body.iter()
                   .find(|(typ_, _nominal)| typ_ == typ)
                   .unwrap().1.0.clone()
           }
       }
   }

   pub fn get_type_from_class(&self, class: &str) -> Type {
       self.body.iter()
           .find(|(_type, nominal)| class == nominal.0)
           .unwrap().0.clone()
   }

}

#[derive(Debug, PartialEq, Clone)]
pub struct Nominal(pub String);

impl From<(Type, usize)> for Nominal {
   fn from(val: (Type, usize)) -> Self {
       let category = TypeCategory::from_type(val.0);
       if val.1 != 100 { // if it's not a primitive like int or bool
           match category {
               TypeCategory::Rest => Nominal("".to_string()),
               _ => Nominal(format!("{}_{}", category, val.1))
           }
       } else {
           Nominal(format!("{}", category))
       }
   } 
}
