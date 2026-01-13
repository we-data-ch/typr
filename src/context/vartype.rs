#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use serde::{Serialize, Deserialize};
use crate::config::TargetLanguage;
use std::collections::HashSet;
use crate::graph::TypeSystem;
use std::iter::Rev;
use crate::builder;
use crate::Context;
use std::io::Write;
use std::fs::File;
use std::io::Read;
use std::ops::Add;
use crate::Config;
use crate::Type;
use crate::Var;

pub fn same_var_type(element1: &(Var, Type), element2: &(Var, Type)) -> bool {
    (element1.0.get_name() == element2.0.get_name()) &&
    (element1.0.get_type() == element2.0.get_type())
}

pub fn merge_variables(set1: HashSet<(Var, Type)>, set2: HashSet<(Var, Type)>,) 
    -> HashSet<(Var, Type)> {
    let mut result = HashSet::new();
    
    for elem2 in &set2 {
        let mut replaced = false;
        
        for elem1 in &set1 {
            if same_var_type(elem1, elem2) {
                result.insert(elem2.clone());
                replaced = true;
                break;
            }
        }
        
        if !replaced {
            result.insert(elem2.clone());
        }
    }
    
    for elem1 in &set1 {
        let mut should_keep = true;
        
        for elem2 in &set2 {
            if same_var_type(elem1, elem2) {
                should_keep = false;
                break;
            }
        }
        
        if should_keep {
            result.insert(elem1.clone());
        }
    }
    
    result
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VarType {
   pub variables: HashSet<(Var, Type)>,
   pub aliases: HashSet<(Var, Type)>,
   pub std: HashSet<(Var, Type)>
}

//main
impl VarType {
    pub fn new() -> VarType {
        let var = Var::from("Generic").set_type(builder::params_type());
        let typ = builder::generic_type();
        let mut aliases = HashSet::new();
        aliases.insert((var, typ));
        VarType {
            variables: HashSet::new(),
            aliases,
            std: HashSet::new()
        }
    }

    pub fn from_config(config: Config) -> VarType {
        let vartype = VarType::new();
        match config.target_language {
            TargetLanguage::R => vartype.load_r().unwrap().load_typed_r().unwrap(),
            TargetLanguage::JS => vartype.load_js().unwrap().load_typed_js().unwrap(),
        }
    }

    pub fn variables(&self) -> Rev<std::vec::IntoIter<&(Var, Type)>> {
        self.variables.iter().collect::<Vec<_>>().into_iter().rev()
    }

    pub fn aliases(&self) -> Rev<std::vec::IntoIter<&(Var, Type)>> {
        self.aliases.iter().collect::<Vec<_>>().into_iter().rev()
    }

    pub fn get_types(&self) -> HashSet<Type> {
        self.variables.iter()
            .chain(self.aliases.iter())
            .flat_map(|(_var, typ)| typ.clone().extract_types()).collect()
    }

    pub fn push_var_type(self, vt: &[(Var, Type)]) -> Self {
        let (var, ali) = Self::separate_variables_aliases(vt.to_vec());
        let ali = ali.iter().cloned().collect::<Vec<_>>();
        self.push_variables(var).push_aliases(&ali)
    }

    pub fn replace_or_push_var_type(self, vt: &[(Var, Type)]) -> Self {
        let (var, ali) = Self::separate_variables_aliases(vt.to_vec());
        let ali = ali.iter().cloned().collect::<Vec<_>>();
        self.replace_or_push_variables(var).push_aliases(&ali)
    }

    pub fn push_alias_increment(self, vt: (Var, Type)) -> Self {
        let name = vt.0.get_name();
        match &name[..] {
            "Generic" | "character" | "integer" | "Alias" | "Any" | "Rfunction" => self.clone(),
            _ => {
                let var = self.aliases.iter()
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
        -> (HashSet<(Var, Type)>, HashSet<(Var, Type)>) {
       let variables = val.iter()
           .filter(|(var, _)| var.is_variable())
           .cloned()
           .collect::<HashSet<(Var, Type)>>();
       let aliases = val.iter()
           .filter(|(var, _)| var.is_alias())
           .cloned()
           .collect::<HashSet<(Var, Type)>>();
       (variables, aliases)
    }

    fn push_variables(self, vt: HashSet<(Var, Type)>) -> Self {
        VarType {
            variables: self.variables.union(&vt).cloned().collect(),
            ..self
        }
    }

    fn replace_or_push_variables(self, vt: HashSet<(Var, Type)>) -> Self {
        let res = merge_variables(self.variables, vt);
        VarType {
            variables: res,
            ..self
        }
    }

    fn push_aliases(self, vt: &[(Var, Type)]) -> Self {
        let vt_set: HashSet<(Var, Type)> = vt.iter().cloned().collect();
        VarType {
            aliases: self.aliases.union(&vt_set).cloned().collect(),
            ..self
        }
    }

    fn replace_aliases(self, vt: &[(Var, Type)]) -> Self {
        let vt_set: HashSet<(Var, Type)> = vt.iter().cloned().collect();
        let res = merge_variables(self.variables.clone(), vt_set);
        VarType {
            aliases: res,
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
            Type::Alias(name, _, _, _) => name.to_string(),
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
            Type::Alias(name, _, _, _) => name.to_string(),
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
        let mut new_aliases = self.aliases.clone();
        if !self.in_aliases(&alias_name) {
            new_aliases.insert((var, typ));
        }
        Self {
            aliases: new_aliases,
            ..self
        }
    }

    pub fn push_alias2(self, var: Var, typ: Type) -> Self {
        let mut new_aliases = self.aliases.clone();
        if !self.in_aliases(&var.get_name()) {
            new_aliases.insert((var, typ));
        }
        Self {
            aliases: new_aliases,
            ..self
        }
    }

    pub fn get_aliases(&self) -> String {
        let mut aliases_vec: Vec<_> = self.aliases.iter().collect();
        aliases_vec.sort_by_key(|(var, _)| var.get_name());
        aliases_vec.iter()
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
        let old_var = self.variables.iter()
            .find(|(v, _)| v.get_name() == var.get_name())
            .expect("Variable not found")
            .clone();

        let mut new_variables = self.variables.clone();
        new_variables.remove(&old_var);
        new_variables.insert((var.clone(), var.get_type()));

        Self {
            variables: new_variables,
            ..self
        }
    }

    pub fn name_exists_outside_of_std(&self, name: &str) -> bool {
        self.variables.iter()
            .filter(|(var, _)| var.get_name() == name)
            .filter(|(var, typ)| !(var.get_type().is_any() && typ.is_unknown_function()))
            .collect::<Vec<_>>().len() > 0
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
        let mut vars = HashSet::new();
        vars.insert((Var::from("add"), "(T, T) -> T".parse::<Type>().unwrap()));
        vars.insert((Var::from("minus"), "(T, T) -> T".parse::<Type>().unwrap()));
        vars.insert((Var::from("mul"), "(T, T) -> T".parse::<Type>().unwrap()));
        vars.insert((Var::from("div"), "(T, T) -> T".parse::<Type>().unwrap()));
        self.push_variables(vars)
    }

    pub fn get_related_functions(&self, typ: &Type) -> Vec<Var> {
        todo!();
    }

    pub fn set_js_var_types(self) -> Self {
        let mut vars = HashSet::new();
        vars.insert((Var::alias("Document", &[]), builder::opaque_type("Doc")));
        self.set_default_var_types()
            .push_variables(vars)
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
    
    pub fn set_std(self, v: Vec<(Var, Type)>) -> Self {
        Self {
            std: v.into_iter().collect(),
            ..self
        }
    }

    pub fn save(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let binary_data = bincode::serialize(self)?;
        let mut file = File::create(path)?;
        file.write_all(&binary_data)?;
        Ok(())
    }

    pub fn load(self, path: &str) -> Result<VarType, Box<dyn std::error::Error>> {
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        let var_type: VarType = bincode::deserialize(&buffer)?;
        Ok(self + var_type)
    }

    pub fn load_r(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../configs/bin/.std_r.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    pub fn load_typed_r(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../configs/bin/.std_r_typed.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    pub fn load_js(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../configs/bin/.std_js.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    pub fn load_typed_js(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../configs/bin/.std_js_typed.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    pub fn from_file(path: &str) -> Result<VarType, Box<dyn std::error::Error>> {
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        let var_type: VarType = bincode::deserialize(&buffer)?;
        Ok(var_type)
    }

    pub fn standard_library(&self) -> Vec<(Var, Type)> {
        self.std.iter().cloned().collect()
    }
    
}

impl Default for VarType {
    fn default() -> Self {
        VarType::new()
            .load_r()
            .unwrap()
    }
}


impl From<Vec<(Var, Type)>> for  VarType {
   fn from(val: Vec<(Var, Type)>) -> Self {
       let (variables, aliases) = VarType::separate_variables_aliases(val);
       VarType {
           variables,
           aliases,
           std: HashSet::new()
       }
   } 
}

impl Add for VarType {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            variables: self.variables.union(&other.variables).cloned().collect(),
            aliases: self.aliases.union(&other.aliases).cloned().collect(),
            std: self.std.union(&other.std).cloned().collect(),
        }
    }
}
