#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use std::collections::HashSet;
use std::cmp::Eq;
use std::hash::Hash;
use std::cmp::Ordering;
use std::fmt::Debug;

pub trait TypeSystem: PartialOrd + Debug + Eq + Hash + Clone + Default {
    fn pretty(&self) -> String;

    fn is_subtype(&self, other: &Self) -> bool {
        self.le(other)
    }


    fn prettys(v: &[Self]) -> String {
        "[".to_string() + &v.iter().map(|x| x.pretty())
            .collect::<Vec<_>>().join(", ") + "]"
    }
}

impl TypeSystem for Type {
    fn pretty(&self) -> String {
        format!("{:?}", self)
    }
}

struct Context;

impl Default for Context {
    fn default() -> Context {
        Context
    }
}

#[derive(Debug, Clone, Eq, Hash, Default)]
enum Type {
    #[default]
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

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let convert = |typ: &Type| match typ {
            Type::Generic => 4,
            Type::Number => 3,
            Type::Integer => 2,
            Type::Sub => 1
        };
        convert(self).partial_cmp(&convert(other))
    }
}



//main
#[derive(Debug, Clone, PartialEq)]
pub struct Graph<T: TypeSystem> {
    memory: HashSet<T>,
    root: Node<T>
}

impl<T: TypeSystem> Graph<T> {
    pub fn new() -> Self {
        Graph {
            memory: HashSet::new(),
            root: Node::new()
        }
    }

    pub fn add_type(self, typ: T) -> Self {
        if self.memory.contains(&typ) {
            self
        } else {
            Graph {
                memory: self.memory.iter().chain([typ.clone()].iter()).cloned().collect(),
                root: self.root.add_type(typ)
            }
        }
    }

    pub fn add_type_trace(self, typ: T) -> Self {
        if self.memory.contains(&typ) {
            self
        } else {
            Graph {
                memory: self.memory.iter().chain([typ.clone()].iter()).cloned().collect(),
                root: self.root.add_type_trace(typ)
            }
        }
    }

    pub fn get_hierarchy(&self) -> String {
        self.root.get_hierarchy()
    }

    pub fn get_type_list(&self) -> String {
        format!("{}", T::prettys(&self.memory.iter().cloned().collect::<Vec<_>>()))
    }

    pub fn print_hierarchy(&self) {
        println!("{}", self.get_hierarchy());
    }

    pub fn get_supertypes(&self, typ: &T) -> Vec<T> {
        self.root.get_supertypes(typ)
            .iter().cloned().collect::<HashSet<_>>()
            .iter().cloned().collect::<Vec<_>>()
    }

    pub fn get_supertypes_trace(&self, typ: &T) -> Vec<T> {
        self.root.get_supertypes_trace(typ)
            .iter().cloned().collect::<HashSet<_>>()
            .iter().cloned().collect::<Vec<_>>()
    }

    pub fn add_types(self, typs: &[T]) -> Self {
        typs.iter()
            .cloned()
            .fold(self, |acc, x| acc.add_type(x))
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct Node<T: TypeSystem> {
   value: T,
   subtypes: Vec<Node<T>>
}

impl<T: TypeSystem> From<T> for Node<T> {
   fn from(val: T) -> Self {
       Node {
            value: val,
            subtypes: vec![]
       } 
   } 
}

//main
impl<T: TypeSystem> Node<T> {
    pub fn new() -> Self {
        Node {
            value: T::default(),
            subtypes: vec![]
        }
    }

    pub fn update(self, types: &[T]) -> Self {
        todo!();
    }

    pub fn deep_clone(&self) -> Self {
        todo!();
    }

    pub fn propagate(self, typ: T) -> Self {
        let graph = Node {
            value: self.value.clone(),
            subtypes: self.subtypes.iter().cloned()
                            .map(|x| x.add_type(typ.clone()))
                            .collect()
        };
        if graph == self {
            self.add_subtype(typ)
        } else {
            graph
        }
    }

    pub fn propagate_trace(self, typ: T) -> Self {
        let graph = Node {
            value: self.value.clone(),
            subtypes: self.subtypes.iter().cloned()
                            .map(|x| x.add_type(typ.clone()))
                            .collect()
        };
        if graph == self {
            println!("add {} to one of the children of {}", 
                     typ.pretty(), self.value.pretty());
            self.add_subtype(typ)
        } else {
            println!("{} is not a subtype of {}'s subtypes: {}", 
                     typ.pretty(), self.value.pretty(), self.show_subtypes());
            graph
        }
    }

    pub fn show_subtypes(&self) -> String {
        "[".to_string() + &self.subtypes.iter()
            .map(|typ| format!("{}", typ.value.pretty()))
            .collect::<Vec<_>>().join(",") + "]"
    }

    pub fn add_subtype(self, typ: T) -> Self {
        Node {
            value: self.value,
            subtypes: self.subtypes.iter()
                        .chain([Node::from(typ)].iter())
                        .cloned().collect()
        }
    }

    pub fn set_subtypes(self, subtypes: Vec<Node<T>>) -> Self {
        Node {
            value: self.value,
            subtypes
        }
    }

    fn switch_if_reverse_subtype(self, typ: T) -> Self {
        if self.value.is_subtype(&typ) {
            Node {
                value: typ,
                subtypes: vec![Node::from(self.value).set_subtypes(self.subtypes)]
            }
        } else { self }
    }

    fn switch_if_reverse_subtype_trace(self, typ: T) -> Self {
        if self.value.is_subtype(&typ) {
            println!("{} is a subtype of the entry {}",
                     self.value.pretty(), typ.pretty());
            Node {
                value: typ,
                subtypes: vec![Node::from(self.value).set_subtypes(self.subtypes)]
            }
        } else { 
            println!("{} is not a subtype of {} abort this branch", 
                     typ.pretty(), self.value.pretty());
            self }
    }

    pub fn add_type(self, typ: T) -> Self {
        match (typ.is_subtype(&self.value), self.subtypes.len()) {
            (true, 0) => self.add_subtype(typ),
            (true, n) => self.propagate(typ),
            _ => self.switch_if_reverse_subtype(typ) 
        }
    }

    pub fn add_type_trace(self, typ: T) -> Self {
        match (typ.is_subtype(&self.value), self.subtypes.len()) {
            (true, 0) => {
                println!("{} is a subtype of the leaf {}",
                         typ.pretty(), self.value.pretty());
                self.add_subtype(typ)},
            (true, n) => {
                println!("{} is a subtype of the node {}",
                         typ.pretty(), self.value.pretty());
                self.propagate_trace(typ)},
            _ => {
                self.switch_if_reverse_subtype_trace(typ)}
        }
    }

    pub fn print_structure(&self) {
        todo!();
    }

    pub fn get_supertypes(&self, target_type: &T) -> Vec<T> {
        if target_type == &self.value {
            vec![]
        } else if target_type.is_subtype(&self.value) {
           self.subtypes.iter() 
               .flat_map(|x| x.get_supertypes(target_type))
               .chain([self.value.clone()].iter().cloned())
               .collect::<Vec<T>>()
        } else {
            vec![]
        }
    }

    pub fn get_supertypes_trace(&self, target_type: &T) -> Vec<T> {
        if target_type == &self.value {
           println!("found the root of {} we backtrack", target_type.pretty());
            vec![]
        } else if target_type.is_subtype(&self.value) {
           println!("{} is subtype of {} we check the subtypes", 
                    target_type.pretty(), self.value.pretty());
           self.subtypes.iter() 
               .flat_map(|x| x.get_supertypes_trace(target_type))
               .chain([self.value.clone()].iter().cloned())
               .collect::<Vec<T>>()
        } else {
           println!("{} is not subtype of {} ABORT this branch", 
                    target_type.pretty(), self.value.pretty());
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
        let tab = Node::<T>::tabulation_from_level(level);
        let children = self.subtypes.iter()
            .map(|x| x.get_hierarchy_helper(level + 1))
            .collect::<Vec<_>>().join("\n");
        tab + &self.value.pretty() + "\n" + &children
    }

}

use std::fmt;
impl<T: TypeSystem> fmt::Display for Node<T> {
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
        let g = Node::new();
        let res = g.get_supertypes(&Type::Integer);
        assert_eq!(vec![Type::Generic], res);
    }

    #[test]
    fn test_add_type0(){
        let g = Node::new()
            .add_type(Type::Integer);
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type1(){
        let g = Node::new()
            .add_type(Type::Number)
            .add_type(Type::Integer);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type2(){
        let g = Node::new()
            .add_type(Type::Integer)
            .add_type_trace(Type::Sub);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type3(){
        let g = Node::new()
            .add_type(Type::Integer)
            .add_type_trace(Type::Number)
            .add_type(Type::Sub);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Sub);
        assert_eq!(vec![Type::Integer, Type::Generic], res);
    }

    #[test]
    fn test_add_type4(){
        let g = Node::new()
            .add_type(Type::Integer)
            .add_type_trace(Type::Number)
            .add_type(Type::Sub);
        println!("g.get_hierarchy():\n {}", g.get_hierarchy());
        let res = g.get_supertypes(&Type::Generic);
        assert_eq!(vec![Type::Generic], res);
    }

    #[test]
    fn test_graph_eq(){
        let g1 = Node::new()
            .add_type(Type::Integer);
        let g2 = Node::new();
        assert_eq!(g1, g2);
    }

    #[test]
    fn test_add_same_type() {
        let g = Graph::new()
            .add_type(Type::Sub)
            .add_type(Type::Sub);
        assert_eq!("Generic\n  Sub\n", g.get_hierarchy());
    }

}
