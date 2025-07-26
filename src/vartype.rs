use std::collections::HashSet;
use std::iter::Rev;
use crate::var::Var;
use crate::Type;

//#[derive(Debug, Clone)]
//pub struct VarType(pub Vec<(Var, Type)>);

#[derive(Debug, Clone)]
pub struct VarType {
   pub variables: Vec<(Var, Type)>,
   pub aliases: Vec<(Var, Type)>
}

impl VarType {
    pub fn new() -> VarType {
        VarType {variables: vec![], aliases: vec![]}
    }

    pub fn variables(&self) -> Rev<std::slice::Iter<'_, (Var, Type)>> {
        self.variables.iter().rev()
    }

    pub fn aliases(&self) -> Rev<std::slice::Iter<'_, (Var, Type)>> {
        self.aliases.iter().rev()
    }

    pub fn get_functions(&self, t: &Type) -> Vec<(Var, Type)> {
        self.variables.iter().rev().filter(|(var, _typ)| var.get_type() == *t).cloned().collect()
    }

    pub fn get_types(&self) -> HashSet<Type> {
        self.variables.iter().rev()
            .chain(self.aliases.iter().rev())
            .flat_map(|(_var, typ)| typ.clone().type_extraction()).collect()
    }

    pub fn push_var_type(self, vt: Vec<(Var, Type)>) -> Self {
        let (var, ali) = Self::separate_variables_aliases(vt);
        self.push_variables(var).push_aliases(ali)
    }
    
    pub fn separate_variables_aliases(val: Vec<(Var, Type)>) 
        -> (Vec<(Var, Type)>, Vec<(Var, Type)>) {
       let variables = val.iter()
           .filter(|(var, _)| var.is_variable())
           .cloned()
           .collect::<Vec<(Var, Type)>>();
       let aliases = val.iter()
           .filter(|(var, _)| var.is_alias())
           .cloned()
           .collect::<Vec<(Var, Type)>>();
       (variables, aliases)
    }

    fn push_variables(self, vt: Vec<(Var, Type)>) -> Self {
        VarType {
            variables: self.variables.iter().chain(vt.iter()).cloned().collect(),
            ..self
        }
    }

    fn push_aliases(self, vt: Vec<(Var, Type)>) -> Self {
        VarType {
            aliases: self.aliases.iter().chain(vt.iter()).cloned().collect(),
            ..self
        }
    }

}


impl From<Vec<(Var, Type)>> for  VarType {
   fn from(val: Vec<(Var, Type)>) -> Self {
       let (variables, aliases) = VarType::separate_variables_aliases(val);
       VarType {
           variables: variables,
           aliases: aliases
       }
   } 
}
