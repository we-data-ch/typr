pub mod config;
pub mod vartype;
pub mod graph;

use crate::processes::type_checking::unification_map::get_unification_map_for_vectorizable_function;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::components::context::unification_map::UnificationMap;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::type_error::TypeError;
use crate::processes::type_checking::match_types_to_generic;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::language::var_function::VarFunction;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::context::config::TargetLanguage;
use crate::utils::standard_library::not_in_blacklist;
use crate::processes::type_checking::unification_map;
use crate::components::context::config::Environment;
use crate::components::context::vartype::VarType;
use crate::components::context::config::Config;
use crate::components::context::graph::Graph;
use crate::processes::type_checking::typing;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use std::collections::HashSet;
use crate::utils::builder;
use std::iter::Rev;
use std::ops::Add;

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
   pub typing_context: VarType,
   pub subtypes: Graph<Type>,
   config: Config,
}

impl Default for Context {
    fn default() -> Self {
        let config = Config::default();
        Context { 
            config: config.clone(),
            typing_context: VarType::from_config(config),
            subtypes: Graph::new(),
        }
    }
}

impl From<Vec<(Lang, Type)>> for  Context {
   fn from(val: Vec<(Lang, Type)>) -> Self {
       let val2: Vec<(Var, Type)> = val.iter()
           .map(|(lan, typ)| { 
                (Var::from_language(lan.clone()).unwrap(), typ.clone())})
           .collect();
        Context { 
            typing_context: val2.into(),
            ..Context::default()
        }
   } 
}

impl Context {
    pub fn new(types: Vec<(Var, Type)>) -> Context {
        Context {
            typing_context: types.into(),
            ..Context::default()
        }
    }

    pub fn empty() -> Self {
        Context { 
            config: Config::default(),
            typing_context: VarType::new(),
            subtypes: Graph::new(),
        }
    }

    pub fn set_config(self, config: Config) -> Self {
        Self {
            config: config,
            ..self
        }
    }

    pub fn set_as_module_context(self) -> Context {
        Self {
           config: self.config.set_as_module(),
           ..self
        }
    }

    pub fn get_members(&self) -> Vec<(Var, Type)> {
        self.typing_context
            .variables()
            .chain(self.aliases())
            .cloned()
            .collect::<Vec<_>>()
    }

    pub fn print_hierarchy(&self) {
        self.subtypes.print_hierarchy();
    }

    pub fn variable_exist(&self, var: Var) -> Option<Var> {
        self.typing_context.variable_exist(var)
    }

    pub fn get_type_from_variable(&self, var: &Var) -> Result<Type, String> {
        let res = self.variables().flat_map(|(var2, typ)| {
            let Var(name1, _, bo1, typ1, _h1) = var;
            let Var(name2, _, bo2, typ2, _h2) = var2;
            let conditions = (name1 == name2) &&
                (bo1 == bo2) && typ1.is_subtype(typ2, self);
            if conditions { Some(typ.clone()) } else { None }
        })
        .reduce(|acc, x| if x.is_subtype(&acc, self) { x } else { acc });
        match res {
            Some(typ) => Ok(typ),
            _ => Err(format!("Didn't find {} in the context: {}", var.get_name(), self.display_typing_context()))
        }
    }

    pub fn get_types_from_name(&self, name: &str) -> Vec<Type> {
        self.variables()
            .filter(|(var, _)| var.get_name() == name )
            .map(|(_, typ)| typ.clone())
            .collect()
    }

    pub fn get_type_from_aliases(&self, var: &Var) -> Option<Type> {
        self.aliases().flat_map(|(var2, type_)| {
            let Var(name1, perm1, bo1, typ1, _h1) = var;
            let Var(name2, perm2, bo2, typ2, _h2) = var2;
            let conditions = (name1 == name2) &&
                (perm1 == perm2) &&
                (bo1 == bo2) && typ1.is_subtype(typ2, self);
            if conditions { Some(type_.clone()) } else { None }
        }).next()
    }

    fn is_matching_alias(&self, var1: &Var, var2: &Var) -> bool {
        let Var(name1, perm1, _bo1, _, _h1) = var1;
        let Var(name2, perm2, _bo2, _, _h2) = var2;
        (name1 == name2) && (perm1 == perm2)
    }

    pub fn get_matching_alias_signature(&self, var: &Var) -> Option<(Type, Vec<Type>)> {
        self.aliases().find(|(var2, _)| self.is_matching_alias(var, var2))
            .map(|(var2, target_type)| {
                if var2.is_opaque() {
                    (var2.clone().to_alias_type(), vec![])
                } else {
                    if let Type::Params(types, _) = var2.get_type() {
                        (target_type.clone(), types.clone())
                    } else { panic!("The related type is not Params([...])"); }
                }
            })
    }

    pub fn variables(&self) -> Rev<std::vec::IntoIter<&(Var, Type)>> {
        self.typing_context.variables()
    }

    pub fn aliases(&self) -> Rev<std::vec::IntoIter<&(Var, Type)>> {
        self.typing_context.aliases()
    }

    pub fn push_var_type(self, lang: Var, typ: Type, context: &Context) -> Context {
        let types = typ.reduce(context).extract_types();
        let var_type = self.typing_context.clone()
            .push_var_type(&[(lang.clone(), typ.clone())])
            .push_types(&types);
        let new_subtypes = self.subtypes.add_types(&types, context);
        Context {
            typing_context: var_type, 
            subtypes: new_subtypes,
            ..self
        }
    }

    pub fn replace_or_push_var_type(self, lang: Var, typ: Type, context: &Context) -> Context {
        let types = typ.reduce(context).extract_types();
        let var_type = self.typing_context.clone()
            .replace_or_push_var_type(&[(lang.clone(), typ.clone())])
            .push_types(&types);
        let new_subtypes = self.subtypes.add_types(&types, context);
        Context {
            typing_context: var_type, 
            subtypes: new_subtypes,
            ..self
        }
    }

    // Remove variables from the context
    // For removing added variables for evaluating a function's body
    pub fn remove_vars(self, vars: &[Var]) -> Context {
        Context {
            typing_context: self.typing_context.remove_vars(vars), 
            ..self
        }
    }

    pub fn push_types(self, types: &[Type]) -> Self {
        Self {
            typing_context: self.typing_context.push_types(types),
            ..self
        }
    }

    pub fn get_type_from_existing_variable(&self, var: Var) -> Type {
        if let Type::UnknownFunction(_) = var.get_type() {
            var.get_type()
        } else {
            self.typing_context.variables()
               .find(|(v, _)| var.match_with(v, self))
               .map(|(_, ty)| ty)
               .expect(&TypeError::UndefinedVariable(var.to_language()).display())
               .clone()
        }
    }

    pub fn get_true_variable(&self, var: &Var) -> Var {
        let res = self.typing_context.variables()
           .find(|(v, _)| var.match_with(v, self))
           .map(|(v, _)| v);
        match res {
            Some(vari) => vari.clone(),
            _ => self.is_an_untyped_function(&var.get_name()) 
                .then(|| var.clone().set_type(Type::UnknownFunction(var.get_help_data())))
                .expect(&format!("The variable {} was not found:\n {}", var, self.display_typing_context()))
        }
    }

    fn is_a_standard_function(&self, name: &str) -> bool {
        !self.typing_context.name_exists_outside_of_std(name)
    }

    pub fn is_an_untyped_function(&self, name: &str) -> bool {
        self.is_a_standard_function(name)
    }

    pub fn get_class(&self, t: &Type) -> String {
        self.typing_context.get_class(t)
    }

    pub fn get_class_unquoted(&self, t: &Type) -> String {
        self.typing_context.get_class_unquoted(t)
    }

    pub fn module_aliases(&self) -> Vec<(Var, Type)> {
        self.variables()
            .flat_map(|(_, typ)| typ.clone().to_module_type())
            .flat_map(|module| module.get_aliases())
            .collect()
    }

    pub fn get_type_anotations(&self) -> String {
        self.aliases()
            .chain([(Var::from_name("Integer"), builder::integer_type_default()),
                    (Var::from_name("Character"), builder::character_type_default()),
                    (Var::from_name("Number"), builder::number_type()),
                    (Var::from_name("Boolean"), builder::boolean_type())].iter())
            .cloned()
            .chain(self.module_aliases())
            .filter(|(_, typ)| typ.clone().to_module_type().is_err())
            .map(|(var, typ)| (typ, var.get_name()))
            .map(|(typ, name)| 
                 format!("{} <- function(x) x |> struct(c('{}', {}, {}))", 
                         name, name, self.get_class(&typ), self.get_classes(&typ).unwrap()))
            .collect::<Vec<_>>().join("\n")
    }

    pub fn get_type_anotation(&self, t: &Type) -> String {
        self.typing_context.get_type_anotation(t)
    }

    pub fn get_type_anotation_no_parentheses(&self, t: &Type) -> String {
        self.typing_context.get_type_anotation_no_parentheses(t)
    }

    pub fn get_classes(&self, t: &Type) -> Option<String> {
        let res = self.subtypes.get_supertypes(t, self)
            .iter()
            .filter(|typ| (*typ).clone().to_module_type().is_err())
            .filter(|typ| !typ.is_empty())
            .map(|typ| self.get_class(typ))
            .collect::<Vec<_>>().join(", ");
        if res == "" {
            Some("'None'".to_string())
        } else {
            Some(res)
        }
    }

    pub fn get_functions(&self, var1: Var) -> Vec<(Var, Type)> {
        self.typing_context.variables()
            .filter(|(var2, typ)| {
                let reduced_type1 = var1.get_type().reduce(self);
                let reduced_type2 = var2.get_type().reduce(self);
                var1.get_name() == var2.get_name()
                    && typ.is_function()
                    && reduced_type1.is_subtype(&reduced_type2, self)
            }).cloned().collect()
    }

    pub fn get_all_generic_functions(&self) -> Vec<(Var, Type)> {
        let res = self.typing_context.variables()
            .filter(|(_, typ)| typ.is_function())
            .filter(|(var, _)| not_in_blacklist(&var.get_name()))
            .filter(|(var, _)| var.is_public())
            .filter(|(var, _)| !var.get_type().is_any())
            .collect::<HashSet<_>>();
        res.iter()
           .map(|(var, typ)| (var.clone().add_backticks_if_percent(), typ.clone()))
           .collect()
    }

    pub fn get_first_matching_function(&self, var1: Var) -> Type {
        let res = self.typing_context.variables()
            .find(|(var2, typ)| {
                let reduced_type1 = var1.get_type().reduce(self);
                let reduced_type2 = var2.get_type().reduce(self);
                var1.get_name() == var2.get_name()
                    && typ.is_function()
                    && (reduced_type1.is_subtype(&reduced_type2, self) || reduced_type1.is_upperrank_of(&reduced_type2))
            });
        if res.is_none() {
            self.typing_context
                .standard_library()
                .iter()
                .find(|(var2, _)| var2.get_name() == var1.get_name())
                .expect(&format!("Can't find var {} in the context:\n {}", 
                       var1.to_string(), self.display_typing_context()))
                .clone()
        } else { 
            res.unwrap().clone()
        }.1
    }

    pub fn get_matching_functions(&self, var1: Var) -> Result<Vec<Type>, String> {
        let res = self.typing_context.variables()
            .filter(|(var2, typ)| {
                let reduced_type1 = var1.get_type().reduce(self);
                let reduced_type2 = var2.get_type().reduce(self);
                var1.get_name() == var2.get_name()
                    && typ.is_function()
                    && (reduced_type1.is_subtype(&reduced_type2, self) || reduced_type1.is_upperrank_of(&reduced_type2))
            }).map(|(_, typ)| typ.clone()).collect::<Vec<_>>();
        if res.len() == 0 {
            let name1 = var1.get_name();
            let std_lib = self.typing_context
                .standard_library();
            let res = std_lib.iter()
                .find(|(var2, _)| var2.get_name() == name1)
                .map(|(_, typ)| typ);
            match res {
                Some(val) => Ok(vec![val.clone()]),
                _ => Err(format!("Can't find var {} in the context:\n {}", var1.to_string(), self.display_typing_context()))
            }
        } else { 
            Ok(res)
        }
    }

    pub fn get_type_from_class(&self, class: &str) -> Type {
        self.typing_context.get_type_from_class(class)
    }


    pub fn add_arg_types(&self, params: &[ArgumentType]) -> Context {
        let param_types = params.iter()
            .map(|arg_typ| reduce_type(self, &arg_typ.get_type()).for_var())
            .map(|typ| match typ.to_owned() {
                Type::Function(typs, _, _) => {
                    if typs.len() > 0 {
                        typs[0].clone()
                    } else { typ }
                },
                t => t
            })
            .collect::<Vec<_>>();
        params.into_iter()
            .zip(param_types.clone().into_iter())
            .map(|(arg_typ, par_typ)| 
                 (Var::from_name(&arg_typ.get_argument_str())
                    .set_type(reduce_type(self, &par_typ)), reduce_type(self, &arg_typ.get_type())))
            .fold(self.clone(), |cont, (var, typ)| cont.clone().push_var_type(var, typ, &cont))
    }

    pub fn set_environment(&self, e: Environment) -> Context {
        Context {
            config: self.config.set_environment(e),
            ..self.clone()
        }
    }

    pub fn display_typing_context(&self) -> String {
       let res = self.variables()
            .chain(self.aliases())
            .map(|(var, typ)| format!("{} ==> {}", var.to_string(), typ.to_string()))
            .collect::<Vec<_>>()
            .join("\n");
        format!("CONTEXT:\n{}", res)
    }

    pub fn error(&self, msg: String) -> String {
        format!("{}{}", msg, self.display_typing_context())
    }

    pub fn push_alias(self, alias_name: String, typ: Type) -> Self {
        Context {
            typing_context: self.typing_context.push_alias(alias_name, typ),
            ..self
        }
    }

    pub fn push_alias2(self, alias_var: Var, typ: Type) -> Self {
        Context {
            typing_context: self.typing_context.push_alias2(alias_var, typ),
            ..self
        }
    }

    pub fn in_a_project(&self) -> bool {
        self.config.environment == Environment::Project
    }


    pub fn get_unification_map(&self, values: &[Lang], param_types: &[Type], name: &str) 
        -> Option<UnificationMap> {
        let entered_types = values.iter()
            .map(|val| typing(self, val).0).collect::<Vec<_>>();

        let unification_map = get_unification_map_for_vectorizable_function(entered_types.clone(), name);
        let res = entered_types.iter()
            .zip(param_types.iter())
            .flat_map(|(val_typ, par_typ)| match_types_to_generic(self, &val_typ.clone(), par_typ))
            .flatten()
            .collect::<Vec<_>>();
        match (unification_map, res.len() > 0) {
            (Some(um), true) => Some(um.append(UnificationMap::new(res))),
            (None, true) => Some(UnificationMap::new(res)),
            (Some(um), false) => Some(um),
            (None, false) => None
        }
    }

    fn s3_type_definition(&self, var: &Var, typ: &Type) -> String {
        let first_part = format!("{} <- function(x) x |> ", var.get_name());
        match typ {
            Type::RClass(v, _) 
                => format!("{} struct(c({}))", first_part, v.iter().cloned()
                           .collect::<Vec<_>>().join(", ")),
            _ => {
                let class = if typ.is_primitive() {
                    format!("'{}'", var.get_name())
                } else { self.get_class(typ) };
                format!("{} struct(c({}))", first_part, class) 
            }
        }
    }

    fn get_primitive_type_definition(&self) -> Vec<String> {
        let primitives =[
            ("Integer", builder::integer_type_default()),
            ("Character", builder::character_type_default()),
            ("Number", builder::number_type()),
            ("Boolean", builder::boolean_type())
        ];
        let new_context = self.clone()
            .push_types(&primitives.iter().map(|(_, typ)| typ).cloned().collect::<Vec<_>>());
        primitives.iter()
            .map(|(name, prim)| 
                 (name,
                  new_context.get_classes(prim).unwrap(),
                  new_context.get_class(prim)))
            .map(|(name, cls, cl)| 
                 format!("{} <- function(x) x |> struct(c({}, {}))", name, cls, cl))
            .collect::<Vec<_>>()
    }

    pub fn get_related_functions(&self, typ: &Type, functions: &VarFunction) -> Vec<Lang> {
        let names = self.typing_context.get_related_functions(typ);
        functions.get_bodies(&names)
    }

    pub fn get_functions_from_type(&self, typ: &Type) -> Vec<(Var, Type)> {
        self.variables()
            .cloned()
            .filter(|(var, typ2)| {
                typ2.is_function() && &var.get_type() == typ
            }).collect()
    }

    pub fn get_type_definition(&self, _functions: &VarFunction) -> String {
        match self.get_target_language() {
            TargetLanguage::R => {
                self.typing_context.aliases.iter()
                    .map(|(var, typ)| self.s3_type_definition(var, typ))
                    .chain(self.get_primitive_type_definition().iter().cloned())
                    .collect::<Vec<_>>().join("\n")
            },
            TargetLanguage::JS => {
                todo!();
            }
        }
    }


    pub fn update_variable(self, var: Var) -> Self {
        Self {
            typing_context: self.typing_context.update_variable(var),
            ..self
        }
    }

    pub fn set_target_language(self, language: TargetLanguage) -> Self {
        Self {
            config: self.config.set_target_language(language),
            typing_context: self.typing_context.source(language),
            ..self
        }
    }

    pub fn set_default_var_types(self) -> Self {
        Self {
           typing_context: self.typing_context.set_default_var_types(),
           ..self
        }
    }

    pub fn get_target_language(&self) -> TargetLanguage {
        self.config.get_target_language()
    }

    pub fn set_new_aliase_signature(self, alias: &str, related_type: Type) -> Self {
        let alias = Var::from_type(alias.parse::<Type>().unwrap()).unwrap();
        self.clone().push_alias2(alias, related_type)
    }

    pub fn extract_module_as_vartype(&self, module_name: &str) -> Self {
        let typ = self.get_type_from_variable(&Var::from_name(module_name))
            .expect("The module name was not found");
        let empty_context = Context::default();
        let new_context = match typ.clone() {
            Type::Module(args, _) => {
                args.iter()
                    .rev()
                    .map(|arg_type| 
                         (Var::try_from(arg_type.0.clone()).unwrap(),
                          arg_type.1.clone())) //TODO: Differenciate between pushing variable and
                                               //aliases
                    .fold(empty_context.clone(), |acc, (var, typ)| acc.clone().push_var_type(var, typ, &acc))
            },
            _ => panic!("{} is not a module", module_name) 
        };
        new_context.clone().push_var_type(Var::from_name(module_name), typ, &new_context)
    }

    pub fn get_vartype(&self) -> VarType {
        self.clone().typing_context
    }

    pub fn get_environment(&self) -> Environment {
        self.config.environment
    }
    pub fn extend_typing_context(self, var_types: VarType) -> Self {
        Self {
            typing_context: self.typing_context + var_types,
            ..self
        }
    }

}

impl Add for Context {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Context {
            typing_context: self.typing_context + other.typing_context,
            subtypes: self.subtypes + other.subtypes,
            config: self.config
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_context1(){
        let context = Context::default();
        println!("{}", context.display_typing_context());
        assert!(true)
    }
}
