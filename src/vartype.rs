use std::collections::HashSet;
use std::iter::Rev;
use crate::var::Var;
use crate::Type;

#[derive(Debug, Clone)]
pub struct VarType(pub Vec<(Var, Type)>);

impl VarType {
    pub fn new() -> VarType {
        VarType(vec![])
    }

    pub fn iter(&self) -> Rev<std::slice::Iter<(Var, Type)>> {
        self.0.iter().rev()
    }

    pub fn get_functions(&self, t: &Type) -> Vec<(Var, Type)> {
        self.0.iter().rev().filter(|(var, _typ)| var.get_type() == *t).cloned().collect()
    }

    pub fn get_types(&self) -> HashSet<Type> {
        self.0.iter().rev().flat_map(|(_var, typ)| typ.clone().type_extraction()).collect()
    }
}
