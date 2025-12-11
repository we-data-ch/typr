#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::r#type::Type;
use crate::language::Lang;
use crate::var::Var;
use crate::argument_type::ArgumentType;
use crate::vartype::VarType;
use crate::type_comparison;
use crate::Environment;
use crate::help_data::HelpData;
use crate::typing;
use crate::type_checker::match_types_to_generic;
use crate::graph::Graph;
use crate::type_comparison::reduce_type;
use crate::TypeError;
use crate::help_message::ErrorMsg;
use std::iter::Rev;
use crate::config::Config;
use crate::builder;
use crate::unification_map::UnificationMap;
use crate::function_type::FunctionType;
use crate::config::TargetLanguage;
use rpds::Vector;
use crate::var_function::VarFunction;
use crate::graph::TypeSystem;
use std::collections::HashSet;

const BLACKLIST: [&str; 2] = ["test_that", "expect_true"];

pub fn not_in_blacklist(name: &str) -> bool {
    let hs = BLACKLIST.iter().cloned().collect::<HashSet<&str>>();
    !hs.contains(name)
}

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

//main
impl Context {
    pub fn new(types: Vec<(Var, Type)>) -> Context {
        Context {
            typing_context: types.into(),
            ..Context::default()
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
        self.variables().flat_map(|(var2, type_)| {
            let Var(name1, perm1, bo1, typ1, _h1) = var;
            let Var(name2, perm2, bo2, typ2, _h2) = var2;
            let conditions = (name1 == name2) &&
                (perm1 == perm2) &&
                (bo1 == bo2) && typ1.is_subtype(typ2, self);
            if conditions { Some(type_.clone()) } else { None }
        }).next()
        .ok_or(format!("Didn't find {} in the context: {}", var.get_name(), self.display_typing_context()))
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

    fn is_matching(&self, var1: &Var, var2: &Var) -> bool {
        let Var(name1, perm1, _bo1, params1, _h1) = var1;
        let Var(name2, perm2, _bo2, params2, _h2) = var2;
        (name1 == name2) &&
            (perm1 == perm2) && 
            params1.is_subtype(params2, self)
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

    pub fn variables(&self) -> Rev<std::slice::Iter<'_, (Var, Type)>> {
        self.typing_context.variables()
    }

    pub fn aliases(&self) -> Rev<std::slice::Iter<'_, (Var, Type)>> {
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
        !self.typing_context.name_exists(name)
    }

    pub fn is_an_untyped_function(&self, name: &str) -> bool {
        self.typing_context.is_untyped_custom_function(name) || 
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
            .flat_map(|(var, typ)| typ.clone().to_module_type())
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

    pub fn get_all_functions(&self) -> Vec<(Var, Type)> {
        self.typing_context.variables()
            .filter(|(_, typ)| typ.is_function())
            .filter(|(var, _)| not_in_blacklist(&var.get_name()))
            .cloned().collect()
    }

    pub fn get_first_matching_function(&self, var1: Var) -> Type {
        let res = self.typing_context.variables()
            .find(|(var2, typ)| {
                let reduced_type1 = var1.get_type().reduce(self);
                let reduced_type2 = var2.get_type().reduce(self);
                var1.get_name() == var2.get_name()
                    && typ.is_function()
                    && reduced_type1.is_subtype(&reduced_type2, self)
            });
        if res.is_none() {
            self.typing_context
                .standard_library()
                .iter()
                .find(|(var2, typ)| var2.get_name() == var1.get_name())
                .expect(&format!("Can't find var {} in the context:\n {}", 
                       var1.to_string(), self.display_typing_context()))
                .clone()
        } else { 
            res.unwrap().clone()
        }.1
    }

    fn build_concret_functions(&self, var_typ: &[(String, Var, Type)]) -> Vec<Lang> {
        var_typ.iter().map(|(par, var, typ)| {
            let t = var.get_type();
            match typ {
                Type::Function(args, t2, h) => {
                   let manips = args.iter().enumerate()
                       .map(|(i, argtyp)| manip(&generate_arg(i), argtyp.clone(), t.clone(), par))
                       .collect::<Vec<_>>();
                   let t_end = (**t2).clone();
                   let manip1 = if t_end == t { Manip::Set(par.to_string()) } else {Manip::Same("a".to_string())};
                   let new_args = args.iter()
                       .map(|ty| if *ty == t {typ.clone()} else { ty.clone() } )
                       .enumerate()
                       .map(|(i, typ)| ArgumentType::new(&generate_arg(i), &typ.clone()))
                       .collect::<Vec<_>>();
                   let new_t2 = if t_end == t { typ.clone() } else {t_end.clone()};
                   Lang::Let(
                       Box::new(var.clone().into()),
                        Type::Empty(HelpData::default()),
                        Box::new(
                           Lang::Function(new_args, new_t2,
                                          Box::new(build_concret_function(&manips, manip1, var.clone())), h.clone())
                                ),
                            h.clone())
                },
                _ => builder::empty_lang()
            }
        }).collect()
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

    pub fn get_unification_map(&self, values: &[Lang], param_types: &[Type]) 
        -> Option<UnificationMap> {
        let res = values.iter()
            .map(|val| typing(self, val).0)
            .zip(param_types.iter())
            .flat_map(|(val_typ, par_typ)| match_types_to_generic(self, &val_typ.clone(), par_typ))
            .flatten()
            .collect::<Vec<_>>();
        (res.len() > 0).then(|| UnificationMap::new(res))
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

    fn js_constructor(typ: &Type) -> String {
        todo!();
    }

    fn js_function(typ: &Lang) -> String {
        todo!();
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

    fn js_class_definition(&self, var: &Var, typ: &Type, functions: &VarFunction) -> Option<String> {
        let super_typs = self.subtypes.get_supertypes(typ, self);
        let functions = super_typs.iter().chain([typ.clone()].iter())
            .flat_map(|super_typ| self.get_related_functions(super_typ, functions))
            .map(|function| Self::js_function(&function))
            .collect::<Vec<_>>().join("\n");
        if functions != "" {
            let constructor = Self::js_constructor(typ);
            Some(format!("class {} {{ {}\n\n{} }} ", var.get_name(), constructor, functions))
        } else { None }
    }

    pub fn get_type_definition(&self, functions: &VarFunction) -> String {
        match self.get_target_language() {
            TargetLanguage::R => {
                self.typing_context.aliases.iter()
                    .map(|(var, typ)| self.s3_type_definition(var, typ))
                    .chain(self.get_primitive_type_definition().iter().cloned())
                    .collect::<Vec<_>>().join("\n")
            },
            TargetLanguage::JS => {
                self.typing_context.aliases.iter()
                    .flat_map(|(var, typ)| self.js_class_definition(var, typ, functions))
                    .collect::<Vec<_>>().join("\n")
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

fn build_concret_function(m: &[Manip], end: Manip, name: Var) -> Lang {
    let args = m.iter()
        .map(|x| x.to_lang())
        .collect::<Vec<_>>();
    match end {
        Manip::Set(param) => {
            Lang::FunctionApp(
                Box::new(Var::from_name("set").to_language()),
                vec![
                    Var::from_name("a").to_language(),
                    Lang::Char(param.to_string(), HelpData::default()),
                    Lang::FunctionApp(Box::new(name.to_language()), args.clone(), builder::empty_type(), args.clone().into()),
                ],
                builder::empty_type(),
                args.into()
            )
        },
        _ => {
            Lang::FunctionApp(Box::new(name.to_language()), args.clone(), builder::empty_type(), args.into())
        }
    }
}


fn manip(arg: &str, t1: Type, t2: Type, par: &str) -> Manip {
   if t1 == t2 {
       Manip::Get(arg.to_string(), par.to_string())
   } else {
        Manip::Same(arg.to_string())
   }
}

type ArgName = String;
type Field = String;

enum Manip {
    Get(ArgName, Field),
    Set(Field),
    Same(ArgName)
}

impl Manip {
    fn to_lang(&self) -> Lang {
        match self {
            Manip::Same(argname) => Var::from_name(&argname).to_language(),
            Manip::Get(argname, field) => {
                Lang::FunctionApp(
                    Box::new(Var::from_name("get").to_language()),
                    vec![
                        Var::from_name(&argname).to_language(),
                        Lang::Char(field.to_string(), HelpData::default())
                    ],
                    builder::empty_type(),
                    HelpData::default())
            },
            _ => builder::empty_lang()
        }
    }
}

pub fn generate_arg(num: usize) -> String {
    match num {
        0 => "a",
        1 => "b",
        2 => "c",
        3 => "d",
        4 => "e",
        5 => "f",
        6 => "g",
        7 => "h",
        8 => "i",
        9 => "j",
        10 => "k",
        _ => "overflow"
    }.to_string()
}

fn add_if_absent(mut vec: Vec<Lang>, val: Lang) -> Vec<Lang> {
    if !vec.contains(&val) {
        vec.push(val);
    }
    vec // Retourne le nouveau vecteur
}
