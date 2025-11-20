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
use crate::type_checker::match_types;
use crate::graph::Graph;
use crate::type_comparison::reduce_type;
use crate::TypeError;
use crate::help_message::ErrorMsg;
use std::iter::Rev;
use crate::config::CompileMode;
use crate::header::Header;
use crate::config::Config;
use crate::Adt;
use crate::builder;
use crate::unification_map::UnificationMap;
use crate::function_type::FunctionType;
use crate::config::TargetLanguage;
use rpds::Vector;
use crate::var_function::VarFunction;
use crate::graph::TypeSystem;

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
   pub typing_context: VarType,
   pub subtypes: Graph<Type>,
   header: Header,
   config: Config,
   js_subcontexts: Vector<Context>,
}

impl Default for Context {
    fn default() -> Self {
        Context { 
            header: Header::default(),
            config: Config::default(),
            typing_context: VarType::new(),
            subtypes: Graph::new(),
            js_subcontexts: Vector::new(),
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

    pub fn print_hierarchy(&self) {
        self.subtypes.print_hierarchy();
    }

    pub fn variable_exist(&self, var: Var) -> Option<Var> {
        self.typing_context.variable_exist(var)
    }

    pub fn get_type_from_variable(&self, var: &Var) -> Option<Type> {
        self.variables().flat_map(|(var2, type_)| {
            let Var(name1, path1, perm1, bo1, typ1, _h1) = var;
            let Var(name2, path2, perm2, bo2, typ2, _h2) = var2;
            let conditions = (name1 == name2) &&
                (path1 == path2) && (perm1 == perm2) &&
                (bo1 == bo2) && typ1.is_subtype(typ2, self);
            if conditions { Some(type_.clone()) } else { None }
        }).next()
    }

    pub fn get_type_from_aliases(&self, var: &Var) -> Option<Type> {
        self.aliases().flat_map(|(var2, type_)| {
            let Var(name1, path1, perm1, bo1, typ1, _h1) = var;
            let Var(name2, path2, perm2, bo2, typ2, _h2) = var2;
            let conditions = (name1 == name2) &&
                (path1 == path2) && (perm1 == perm2) &&
                (bo1 == bo2) && typ1.is_subtype(typ2, self);
            if conditions { Some(type_.clone()) } else { None }
        }).next()
    }

    fn is_matching(&self, var1: &Var, var2: &Var) -> bool {
        let Var(name1, path1, perm1, _bo1, params1, _h1) = var1;
        let Var(name2, path2, perm2, _bo2, params2, _h2) = var2;
        (name1 == name2) &&
            (path1 == path2) && (perm1 == perm2) && 
            params1.is_subtype(params2, self)
    }

    fn is_matching_alias(&self, var1: &Var, var2: &Var) -> bool {
        let Var(name1, path1, perm1, _bo1, _, _h1) = var1;
        let Var(name2, path2, perm2, _bo2, _, _h2) = var2;
        (name1 == name2) &&
            (path1 == path2) && (perm1 == perm2)
    }

    pub fn get_matching_alias_signature(&self, var: &Var) -> Option<(Type, Vec<Type>)> {
        self.aliases().find(|(var2, _)| self.is_matching_alias(var, var2))
            .map(|(var2, target_type)| {
                if var2.is_opaque() {
                    (var2.clone().to_alias(), vec![])
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
        if let Type::RFunction(_) = var.get_type() {
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
                .then(|| var.clone().set_type(Type::RFunction(var.get_help_data())))
                .expect(&format!("The variable {} was not found:\n {}", var, self.display_typing_context()))
        }
    }

    fn is_a_standard_function(&self, name: &str) -> bool {
        !self.typing_context.name_exists(name) && self.header.exist_in_standard_lib(name)
    }

    pub fn is_an_untyped_function(&self, name: &str) -> bool {
        self.typing_context.is_untyped_custom_function(name) || 
        self.is_a_standard_function(name) || 
        self.header.is_generic(name.to_string())
    }

    pub fn get_class(&self, t: &Type) -> String {
        self.typing_context.get_class(t)
    }

    pub fn get_class_unquoted(&self, t: &Type) -> String {
        self.typing_context.get_class_unquoted(t)
    }

    pub fn get_type_anotation(&self, t: &Type) -> String {
        self.typing_context.get_type_anotation(t)
    }

    pub fn get_type_anotation_no_parentheses(&self, t: &Type) -> String {
        self.typing_context.get_type_anotation_no_parentheses(t)
    }

    pub fn get_classes(&self, t: &Type) -> Option<String> {
        let res = self.subtypes.get_supertypes(t, self)
            .iter().map(|typ| self.get_class(typ))
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
            }).cloned()
            .collect()
    }

    pub fn add_module_declarations(self, data: &[Lang]) -> Context {
        Context {
            header: self.header.add_module_declarations(data),
            ..self
        }
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
                       var.clone(),
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

    pub fn set_compile_mode(self, cm: CompileMode) -> Context {
        Context {
            config: self.config.set_compile_mode(cm),
            ..self
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

    pub fn is_in_header_mode(&self) -> bool {
        self.config.compile_mode == CompileMode::Header 
    }

    pub fn append_function_list(&self, t: &str) -> Context {
        Context {
            header: self.header.clone().add_function_list(t),
            ..self.clone()
        } 
    }

    pub fn add_lang_to_header(self, langs: &[Lang]) -> Context {
        Context {
            header: self.header.add_lang(langs),
            ..self
        }
    }

    pub fn get_adt(&self) -> Adt {
        self.header.metadata.get_adt()
    }

    pub fn in_a_project(&self) -> bool {
        self.config.environment == Environment::Project
    }

    pub fn we_check_mutability(&self) -> bool {
        self.config.immutability 
    }

    pub fn get_unification_map(&self, values: &[Lang], param_types: &[Type]) 
        -> Option<UnificationMap> {
        let res = values.iter()
            .map(|val| typing(self, val).0)
            .zip(param_types.iter())
            .flat_map(|(val_typ, par_typ)| match_types(self, &val_typ.clone(), par_typ))
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
            _ => format!("{} struct(c({}))", first_part, self.get_class(typ)) 
        }
    }

    fn get_primitive_type_definition(&self) -> Vec<String> {
        let int_super_classes = self.get_classes(&builder::integer_type_default()).unwrap();
        vec![format!("Integer <- function(x) x |> struct(c({}))", int_super_classes)]
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

    pub fn push(self, fn_lang: Lang, fn_type: FunctionType) -> Self {
        Self {
            header: self.header.push(fn_lang, fn_type),
            ..self
        }
    }

    pub fn get_true_fn_type(&self, fn_lang: &Lang) -> Option<FunctionType> {
        self.header.get_true_fn_type(fn_lang)
    }

    pub fn update_variable(self, var: Var) -> Self {
        Self {
            typing_context: self.typing_context.update_variable(var),
            ..self
        }
    }

    pub fn not_generic_yet(&self, name: String) -> bool {
        self.header.not_generic_yet(name)
    }

    pub fn set_target_language(self, language: TargetLanguage) -> Self {
        Self {
            config: self.config.set_target_language(language),
            header: self.header.set_function_list(language),
            typing_context: self.typing_context.source(language),
            ..self
        }
    }

    pub fn add_js_subcontext(self, js_context: Context) -> (Self, u32) {
        (Self {
            js_subcontexts: self.js_subcontexts.push_back(js_context),
            ..self
        },
        self.js_subcontexts.len() as u32)
    }

    pub fn get_js_subcontext(&self, id: u32) -> Self {
        self.js_subcontexts[id as usize].clone()
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
