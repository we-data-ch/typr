use std::collections::HashMap;
use crate::context::Context;
use crate::types::Type;

pub struct NominalContext(Vec<(Type, String, Vec<String>)>);

impl From<Vec<(Type, String, Vec<String>)>> for  NominalContext {
   fn from(val: Vec<(Type, String, Vec<String>)>) -> Self {
        NominalContext(val.clone())
   } 
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum TypeCategory {
    Array,
    Function,
    Record,
    Index,
    Alias,
    Tag,
    Union,
    Interface
}

struct TypeNominal {
    body: Vec<(Type, Nominal)>,
    categories: HashMap<TypeCategory, usize>
}

impl TypeNominal {
    fn new() -> Self {
        let mut categories = HashMap::new();
        categories.insert(TypeCategory::Array, 0 as usize);
        categories.insert(TypeCategory::Function, 0 as usize);
        categories.insert(TypeCategory::Record, 0 as usize);
        categories.insert(TypeCategory::Index, 0 as usize);
        categories.insert(TypeCategory::Alias, 0 as usize);
        categories.insert(TypeCategory::Tag, 0 as usize);
        categories.insert(TypeCategory::Union, 0 as usize);
        categories.insert(TypeCategory::Interface, 0 as usize);
        TypeNominal {
            body: vec![],
            categories: categories 
        }
    }

    fn corresponding_nominal(&self, type_: &Type) -> Option<Nominal> {
        todo!();
    }

   fn get_nth(&mut self, type_: Type) -> usize {
      let res = match type_ {
          Type::Array(_, _) => self.get_nth_helper(TypeCategory::Array),
          Type::Function(_, _) => self.get_nth_helper(TypeCategory::Function),
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

      };
      todo!();
   } 

   fn get_nth_helper(&mut self, category: TypeCategory) -> usize {
       self.incr(category); self.get(category)
   }

   fn incr(&mut self, category: TypeCategory) -> () {
       self.categories.insert(category, *self.categories.get(&category).unwrap() + 1);
   }

   fn get(&self, category: TypeCategory) -> usize {
       *self.categories.get(&category).unwrap()
   }

   fn push(&mut self, type_nominal: (Type, Nominal)) {
       todo!();
   }

   fn new_nominal(&mut self, type_: Type) -> Nominal {
        Nominal::from((type_.clone(), self.get_nth(type_)))
   }

}

#[derive(Debug, PartialEq)]
struct Nominal(String);

impl From<(Type, usize)> for Nominal {
   fn from(val: (Type, usize)) -> Self {
       todo!();
   } 
}

fn get_nominal(types: Vec<Type>, con: Context) -> TypeNominal {
    types.iter().fold(TypeNominal::new(), |mut acc, type_| {
        let res = match acc.corresponding_nominal(type_) {
            Some(nominal) => (type_.clone(), nominal),
            None => (type_.clone(), acc.new_nominal(type_.clone()))
        };
        acc.push(res); acc
    })
}

impl From<Context> for NominalContext {
   fn from(con: Context) -> Self {
       let types = con.get_types();
       let nominals = get_nominal(types.clone(), con);
       dbg!(&types);
       vec![].into()
   } 
}
