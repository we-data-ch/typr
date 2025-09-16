#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use std::collections::HashSet;

struct Context;

impl Default for Context {
    fn default() -> Context {
        Context
    }
}

#[derive(Debug, Clone, Eq, Hash)]
enum Type {
    Generic,
    Number,
    Integer,
    Sub
}


impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Generic, Type::Generic) => true,
            (Type::Integer, Type::Integer) => true,
            (Type::Sub, Type::Sub) => true,
            (Type::Number, Type::Number) => true,
            _ => false
        }
    }
}

impl Type {
    pub fn is_subtype(&self, other: &Type, context: &Context) -> bool {
        match (self, other) {
            (_, Type::Generic) => true,
            (Type::Integer, Type::Number) => true,
            (Type::Sub, Type::Integer) => true,
            (Type::Sub, Type::Number) => true,
            (_, _) => false
        }
    }

    pub fn pretty(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Graph {
   value: Type,
   subtypes: Vec<Graph>
}

impl From<Type> for Graph {
   fn from(val: Type) -> Self {
       Graph {
            value: val,
            subtypes: vec![]
       } 
   } 
}

//main
impl Graph {
    pub fn new() -> Self {
        Graph {
            value: Type::Generic,
            subtypes: vec![]
        }
    }

    pub fn update(self, types: &[Type]) -> Self {
        todo!();
    }

    pub fn deep_clone(&self) -> Self {
        todo!();
    }


    pub fn no_child_as_supertype_of(&self, typ: Type) -> bool {
        self.subtypes.iter().cloned()
                    .all(|x| !typ.is_subtype(&x.value, &Context::default()))
    }

    pub fn propagate(self, typ: Type) -> Self {
        if self.no_child_as_supertype_of(typ.clone()) {
            self.add_subtype(typ)
        } else {
            Graph {
                value: self.value,
                subtypes: self.subtypes.iter().cloned()
                                .map(|x| x.add_type(typ.clone()))
                                .collect()
            }
        }
    }

    pub fn propagate_trace(self, typ: Type) -> Self {
        if self.no_child_as_supertype_of(typ.clone()) {
            println!("{:?} is not a subtype of {:?}'s subtypes: {}", typ, self.value, self.show_subtypes());
            self.add_subtype(typ)
        } else {
            println!("add {:?} to one of the children of {:?}", typ, self.value);
            Graph {
                value: self.value,
                subtypes: self.subtypes.iter().cloned()
                                .map(|x| x.add_type(typ.clone()))
                                .collect()
            }
        }
    }

    pub fn show_subtypes(&self) -> String {
        "[".to_string() + &self.subtypes.iter()
            .map(|typ| format!("{:?}", typ.value))
            .collect::<Vec<_>>().join(",") + "]"
    }

    pub fn add_subtype(self, typ: Type) -> Self {
        Graph {
            value: self.value,
            subtypes: self.subtypes.iter()
                        .chain([Graph::from(typ)].iter())
                        .cloned().collect()
        }
    }

    pub fn set_subtypes(self, subtypes: Vec<Graph>) -> Self {
        Graph {
            value: self.value,
            subtypes
        }
    }

    fn switch_if_reverse_subtype(self, typ: Type) -> Self {
        if self.value.is_subtype(&typ, &Context::default()) {
            Graph {
                value: typ,
                subtypes: vec![Graph::from(self.value).set_subtypes(self.subtypes)]
            }
        } else { self }
    }

    fn switch_if_reverse_subtype_trace(self, typ: Type) -> Self {
        if self.value.is_subtype(&typ, &Context::default()) {
            println!("{:?} is a subtype of the entry {:?}", self.value, typ);
            Graph {
                value: typ,
                subtypes: vec![Graph::from(self.value).set_subtypes(self.subtypes)]
            }
        } else { 
            println!("{:?} is not a subtype of {:?} abort this branch", typ, self.value);
            self }
    }

    pub fn add_type(self, typ: Type) -> Self {
        match (typ.is_subtype(&self.value, &Context::default()), self.subtypes.len()) {
            (true, 0) => self.add_subtype(typ),
            (true, n) => self.propagate(typ),
            _ => self.switch_if_reverse_subtype(typ) 
        }
    }

    pub fn add_type_trace(self, typ: Type) -> Self {
        match (typ.is_subtype(&self.value, &Context::default()), self.subtypes.len()) {
            (true, 0) => {
                println!("{:?} is a subtype of the leaf {:?}", typ, self.value);
                self.add_subtype(typ)},
            (true, n) => {
                println!("{:?} is a subtype of the node {:?}", typ, self.value);
                self.propagate_trace(typ)},
            _ => {
                self.switch_if_reverse_subtype_trace(typ)}
        }
    }

    pub fn print_structure(&self) {
        todo!();
    }

    pub fn get_supertypes(&self, target_type: &Type) -> Vec<Type> {
        if target_type.is_subtype(&self.value, &Context::default()) {
           self.subtypes.iter() 
               .flat_map(|x| x.get_supertypes(target_type))
               .chain([self.value.clone()].iter().cloned())
               .rev().collect::<HashSet<Type>>()
               .iter().cloned().collect::<Vec<Type>>()
        } else {
            vec![]
        }
    }

    pub fn get_hierarchy(&self) -> String {
        self.get_hierarchy_helper(0)
    }

    fn tabulation_from_level(level: i32) -> String {
        (0..level).into_iter().map(|_| "  ")
            .collect::<Vec<_>>().join("")
    }

    pub fn get_hierarchy_helper(&self, level: i32) -> String {
        let tab = Graph::tabulation_from_level(level);
        let children = self.subtypes.iter()
            .map(|x| x.get_hierarchy_helper(level + 1))
            .collect::<Vec<_>>().join("\n");
        tab + &self.value.pretty() + "\n" + &children
    }

}

use std::fmt;
impl fmt::Display for Graph {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_hierarchy())       
    }
}

fn main() {
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init(){
        let g = Graph::new();
        let res = g.get_supertypes(&Type::Integer);
        assert_eq!(vec![Type::Generic], res);
    }

    #[test]
    fn test_add_type0(){
        let g = Graph::new()
            .add_type(Type::Integer);
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type1(){
        let g = Graph::new()
            .add_type(Type::Number)
            .add_type(Type::Integer);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type2(){
        let g = Graph::new()
            .add_type(Type::Integer)
            .add_type_trace(Type::Sub);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type3(){
        let g = Graph::new()
            .add_type(Type::Integer)
            .add_type_trace(Type::Number)
            .add_type(Type::Sub);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

}
