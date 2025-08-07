use std::collections::HashSet;
use std::iter::Rev;
use crate::var::Var;
use crate::Type;
use crate::builder;


#[derive(Debug, Clone, PartialEq)]
pub struct VarType {
   pub variables: Vec<(Var, Type)>,
   pub aliases: Vec<(Var, Type)>
}

//main
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

    pub fn get_types(&self) -> HashSet<Type> {
        self.variables.iter().rev()
            .chain(self.aliases.iter().rev())
            .flat_map(|(_var, typ)| typ.clone().extract_types()).collect()
    }

    pub fn push_var_type(self, vt: &[(Var, Type)]) -> Self {
        let (var, ali) = Self::separate_variables_aliases(vt.to_vec());
        self.push_variables(var).push_aliases(&ali)
    }

    pub fn push_alias_increment(self, vt: (Var, Type)) -> Self {
        let name = vt.0.get_name();
        let var = self.aliases.iter()
            .find(|(var, _)| var.contains(&name))
            .map(|(var, _)| var.get_digit(&name) + 1)
            .map(|x| vt.0.clone().add_digit(x))
            .unwrap_or(vt.0.add_digit(0));
        self.push_aliases(&[(var, vt.1)])
    }

    pub fn exists(&self, typ: &Type) -> bool {
        self.aliases.iter()
            .find(|(var, typ2)| typ == typ2)
            .is_some()
    }

    fn push_type_if_not_exists(self, typ: Type) -> Self {
        (!self.exists(&typ))
            .then_some(self.clone()
                       .push_alias_increment((typ.to_category().to_variable(), typ)))
            .unwrap_or(self)
    }

    pub fn push_types(self, types: &[Type]) -> Self {
        types.iter()
            .fold(self,
                  |vartyp, typ| vartyp.push_type_if_not_exists(typ.clone()))
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

    fn push_aliases(self, vt: &[(Var, Type)]) -> Self {
        VarType {
            aliases: self.aliases.iter().chain(vt.iter()).cloned().collect(),
            ..self
        }
    }

    pub fn get_class(&self, t: &Type) -> String {
        self.aliases.iter()
            .find(|(_, typ)| typ == t)
            .map(|(var, _)| var.get_name())
            .expect(&format!("{} was not found with a corresponding alias.", t))
    }

    pub fn get_type_from_class(&self, class: &str) -> Type {
        self.aliases.iter()
            .find(|(var, _)| var.get_name() == class)
            .map(|(_, typ)| typ)
            .expect(&format!("{} isn't an existing Alias name (don't know where it come from)", class))
            .clone()
    }

    pub fn push_alias(self, alias_name: String, typ: Type) -> Self {
        let var = Var::from_name(&alias_name)
                    .set_type(builder::params_type());
        Self {
            aliases: self.aliases.iter().chain([(var, typ)].iter()).cloned().collect(),
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
