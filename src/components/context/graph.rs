use crate::components::context::context::Context;
use std::collections::HashSet;
use std::hash::Hash;
use std::fmt::Debug;
use std::ops::Add;
use std::cmp::Eq;

pub trait TypeSystem: PartialOrd + Debug + Eq + Hash + Clone + Default {
    fn pretty(&self) -> String;

    fn is_subtype(&self, other: &Self, context: &Context) -> bool;

    fn prettys(v: &[Self]) -> String {
        "[".to_string() + &v.iter().map(|x| x.pretty())
            .collect::<Vec<_>>().join(", ") + "]"
    }
}

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

    pub fn add_type(self, typ: T, context: &Context) -> Self {
        if self.memory.contains(&typ) {
            self
        } else {
            let new_memory = self.memory.iter().chain([typ.clone()].iter()).cloned().collect();
            let new_root = self.root.add_type(typ.clone(), context);
            Graph {
                memory: new_memory, 
                root: new_root 
            }
        }
    }

    pub fn add_type_trace(self, typ: T, context: &Context) -> Self {
        if self.memory.contains(&typ) {
            self
        } else {
            Graph {
                memory: self.memory.iter().chain([typ.clone()].iter()).cloned().collect(),
                root: self.root.add_type_trace(typ, context)
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

    pub fn get_supertypes(&self, typ: &T, context: &Context) -> Vec<T> {
        self.root.get_supertypes(typ, context)
            .iter().cloned().collect::<HashSet<_>>()
            .iter().cloned().collect::<Vec<_>>()
    }

    pub fn get_supertypes_trace(&self, typ: &T, context: &Context) -> Vec<T> {
        self.root.get_supertypes_trace(typ, context)
            .iter().cloned().collect::<HashSet<_>>()
            .iter().cloned().collect::<Vec<_>>()
    }

    pub fn add_types(self, typs: &[T], context: &Context) -> Self {
        typs.iter()
            .cloned()
            .fold(self, |acc, x| acc.add_type(x, context))
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

impl<T: TypeSystem> Node<T> {
    pub fn new() -> Self {
        Node {
            value: T::default(),
            subtypes: vec![]
        }
    }

    pub fn propagate(self, typ: T, context: &Context) -> Self {
        let graph = Node {
            value: self.value.clone(),
            subtypes: self.subtypes.iter().cloned()
                            .map(|x| x.add_type(typ.clone(), context))
                            .collect()
        };
        if graph == self {
            self.add_subtype(typ)
        } else {
            graph
        }
    }

    pub fn propagate_trace(self, typ: T, context: &Context) -> Self {
        let graph = Node {
            value: self.value.clone(),
            subtypes: self.subtypes.iter().cloned()
                            .map(|x| x.add_type(typ.clone(), context))
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

    fn switch_if_reverse_subtype(self, typ: T, context: &Context) -> Self {
        if self.value.is_subtype(&typ, context) {
            Node {
                value: typ,
                subtypes: vec![Node::from(self.value).set_subtypes(self.subtypes)]
            }
        } else { self }
    }

    fn switch_if_reverse_subtype_trace(self, typ: T, context: &Context) -> Self {
        if self.value.is_subtype(&typ, context) {
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

    pub fn add_type(self, typ: T, context: &Context) -> Self {
        if self.value == typ {
            self
        } else {
            match (typ.is_subtype(&self.value, context), self.subtypes.len()) {
                (true, 0) =>  self.add_subtype(typ),
                (true, _) => self.propagate(typ, context),
                _ => self.switch_if_reverse_subtype(typ, context)
            }
        }
    }

    pub fn add_type_trace(self, typ: T, context: &Context) -> Self {
        match (typ.is_subtype(&self.value, context), self.subtypes.len()) {
            (true, 0) => {
                println!("{} is a subtype of the leaf {}",
                         typ.pretty(), self.value.pretty());
                self.add_subtype(typ)},
            (true, _) => {
                println!("{} is a subtype of the node {}",
                         typ.pretty(), self.value.pretty());
                self.propagate_trace(typ, context)},
            _ => {
                self.switch_if_reverse_subtype_trace(typ, context)}
        }
    }

    pub fn get_supertypes(&self, target_type: &T, context: &Context) -> Vec<T> {
        if target_type == &self.value {
            vec![]
        } else if target_type.is_subtype(&self.value, context) {
           self.subtypes.iter() 
               .flat_map(|x| x.get_supertypes(target_type, context))
               .chain([self.value.clone()].iter().cloned())
               .collect::<Vec<T>>()
        } else {
            vec![]
        }
    }

    pub fn get_supertypes_trace(&self, target_type: &T, context: &Context) -> Vec<T> {
        if target_type == &self.value {
           println!("found the root of {} we backtrack", target_type.pretty());
            vec![]
        } else if target_type.is_subtype(&self.value, context) {
           println!("{} is subtype of {} we check the subtypes", 
                    target_type.pretty(), self.value.pretty());
           self.subtypes.iter() 
               .flat_map(|x| x.get_supertypes_trace(target_type, context))
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

impl<T: TypeSystem> Add for Graph<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let context = Context::default(); // Or apply a parameter if necessary
        other.memory.iter()
            .cloned()
            .fold(self, |acc, typ| acc.add_type(typ, &context))
    }
}
