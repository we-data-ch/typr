#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use std::collections::HashSet;
use std::iter::Rev;
use crate::var::Var;
use crate::Type;
use crate::builder;
use crate::Context;
use crate::graph::TypeSystem;
use crate::config::TargetLanguage;

#[derive(Debug, Clone, PartialEq)]
pub struct VarType {
   pub variables: Vec<(Var, Type)>,
   pub aliases: Vec<(Var, Type)>
}

//main
impl VarType {
    pub fn new() -> VarType {
        let var = Var::from("Generic").set_type(builder::params_type());
        let typ = builder::generic_type();
        VarType {
            variables: vec![],
            aliases: vec![(var, typ)]
        }
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
        match &name[..] {
            "Generic" | "character" | "integer" | "Alias" | "Any" | "Rfunction" => self.clone(),
            _ => {
                let var = self.aliases.iter().rev()
                    .find(|(var, _)| var.contains(&name))
                    .map(|(var, _)| var.get_digit(&name) + 1)
                    .map(|x| vt.0.clone().add_digit(x))
                    .unwrap_or(vt.0.add_digit(0));
                self.push_aliases(&[(var, vt.1)])
            }
            
        }
    }

    pub fn exists(&self, typ: &Type) -> bool {
        self.aliases.iter()
            .find(|(_, typ2)| typ == typ2)
            .is_some() || typ.is_primitive()
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
        //vt.iter().map(|var| )
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
        let res = match t {
            Type::Integer(_, _) => "integer".to_string(),
            Type::Char(_, _) => "character".to_string(),
            Type::Boolean(_) => "logical".to_string(),
            Type::Number(_) => "numeric".to_string(),
            _ => self.aliases.iter()
                .find(|(_, typ)| typ == t)
                .map(|(var, _)| var.get_name())
                .unwrap_or(t.pretty())
        };
        "'".to_string() + &res + "'"
    }

    pub fn get_type_anotation(&self, t: &Type) -> String {
        let res = match t {
            Type::Boolean(_) => "Boolean".to_string(),
            Type::Integer(_, _) => "Integer".to_string(),
            Type::Number(_) => "Number".to_string(),
            Type::Char(_, _) => "Character".to_string(),
            Type::Vector(_, _, _) => "".to_string(),
            Type::Alias(name, _, _, _, _) => name.to_string(),
            _ => {
                self.aliases.iter()
                    .find(|(_, typ)| typ == t)
                    .map(|(var, _)| var.get_name())
                    .unwrap_or("Generic".to_string())
            }
        };
        format!("{}()", res)
    }

    pub fn get_type_anotation_no_parentheses(&self, t: &Type) -> String {
        match t {
            Type::Boolean(_) => "logical".to_string(),
            Type::Integer(_, _) => "integer".to_string(),
            Type::Number(_) => "number".to_string(),
            Type::Char(_, _) => "character".to_string(),
            Type::Alias(name, _, _, _, _) => name.to_string(),
            _ => self.aliases.iter()
                    .find(|(_, typ)| typ == t)
                    .map(|(var, _)| var.get_name())
                    .unwrap_or("Generic".to_string())
        }
    }

    pub fn get_class_unquoted(&self, t: &Type) -> String {
        match t {
            Type::Integer(_, _) => "integer".to_string(),
            Type::Char(_, _) => "character".to_string(),
            Type::Boolean(_) => "logical".to_string(),
            Type::Number(_) => "numeric".to_string(),
            _ => self.aliases.iter()
                .find(|(_, typ)| typ == t)
                .map(|(var, _)| var.get_name())
                .unwrap_or(t.pretty())
        }
    }

    pub fn get_type_from_class(&self, class: &str) -> Type {
        self.aliases.iter()
            .find(|(var, _)| var.get_name() == class)
            .map(|(_, typ)| typ)
            .expect(&format!("{} isn't an existing Alias name (don't know where it come from)", class))
            .clone()
    }

    fn in_aliases(&self, alias_name: &str) -> bool {
        self.aliases.iter()
            .find(|(var, _)| var.get_name() == alias_name)
            .is_some()
    }

    pub fn push_alias(self, alias_name: String, typ: Type) -> Self {
        let var = Var::from_name(&alias_name)
                    .set_type(builder::params_type());
        let new_aliases = if !self.in_aliases(&alias_name) {
            self.aliases.iter().chain([(var, typ)].iter()).cloned().collect::<Vec<_>>()
        } else { self.aliases.clone() };
        let res = Self {
            aliases: new_aliases,
            ..self
        };
        res
    }

    pub fn push_alias2(self, var: Var, typ: Type) -> Self {
        let new_aliases = if !self.in_aliases(&var.get_name()) {
            self.aliases.iter().chain([(var, typ)].iter()).cloned().collect::<Vec<_>>()
        } else { self.aliases.clone() };
        let res = Self {
            aliases: new_aliases,
            ..self
        };
        res
    }

    pub fn get_aliases(&self) -> String {
        self.aliases.iter()
            .map(|(var, typ)| format!("{} = {}", var.get_name(), typ.pretty()))
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn print_aliases(&self) {
        println!("{}", self.get_aliases());
    }

    pub fn variable_exist(&self, var: Var) -> Option<Var> {
        self.variables.iter()
            .find(|(v, _)| { v.match_with(&var, &Context::default()) })
            .map(|(v, _)| v.clone())
    }

    pub fn update_variable(self, var: Var) -> Self {
        let id = self.variables.iter().enumerate()
            .find(|(_, (v, _))| v.get_name() == var.get_name())
            .map(|(i, (_, _))| i)
            .expect("Variable not found");


        let mut vec = self.variables.clone();
        vec[id] = (var.clone(), var.get_type()); //TODO: latter check if vec[id].1 is a function

        Self {
            variables: vec,
            ..self
        }
    }

    pub fn name_exists(&self, name: &str) -> bool {
        self.variables.iter()
            .find(|(var, _)| var.get_name() == name)
            .is_some()
    }

    pub fn is_untyped_custom_function(&self, name: &str) -> bool {
        self.variables.iter()
            .find(|(var, typ)| (var.get_name() == name) && typ.is_r_function())
            .is_some()
    }

    pub fn remove_vars(self, vars: &[Var]) -> Self {
        vars.iter()
            .fold(self, |acc, x| acc.remove_var(x))
    }

    pub fn remove_var(self, var: &Var) -> Self {
        Self {
            variables: self.variables.iter()
                .filter(|(var2, _)|  var != var2)
                .cloned().collect(),
            ..self
        }
    }

    pub fn set_default_var_types(self) -> Self {
        self.push_variables(vec![
             (Var::from("add"), "(T, T) -> T".parse::<Type>().unwrap()),
             (Var::from("minus"), "(T, T) -> T".parse::<Type>().unwrap()),
             (Var::from("mul"), "(T, T) -> T".parse::<Type>().unwrap()),
             (Var::from("div"), "(T, T) -> T".parse::<Type>().unwrap()),
        ])
    }

    pub fn get_related_functions(&self, typ: &Type) -> Vec<Var> {
        todo!();
    }

    pub fn set_js_var_types(self) -> Self {
        self.set_default_var_types()
            .push_variables(vec![
                (Var::alias("Document", &[]), builder::opaque_type("Doc"))
            ])
    }

    pub fn set_r_var_types(self) -> Self {
        self.set_default_var_types()
    }

    pub fn source(self, target_language: TargetLanguage) -> Self {
       match target_language {
           TargetLanguage::JS => self.set_js_var_types(),
           TargetLanguage::R => self.set_r_var_types()
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
