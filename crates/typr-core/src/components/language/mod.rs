pub mod argument_value;
pub mod function_lang;
pub mod var_function;
pub mod module_lang;
pub mod array_lang;
pub mod operators;
pub mod var;

use crate::components::language::argument_value::ArgumentValue;
use crate::processes::transpiling::translatable::RTranslatable;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::parsing::operation_priority::TokenKind;
use crate::components::error_message::locatable::Locatable;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::error_message::help_data::HelpData;
use crate::processes::parsing::lang_token::LangToken;
use crate::components::r#type::vector_type::VecType;
use crate::components::context::config::Environment;
use crate::processes::parsing::elements::elements;
use crate::components::context::config::Config;
use crate::components::language::operators::Op;
use crate::processes::type_checking::typing;
use crate::components::language::var::Var;
use crate::components::context::Context;
use crate::components::r#type::Type;
use serde::{Deserialize, Serialize};
use crate::utils::builder;
use std::str::FromStr;

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub enum ModulePosition {
    Internal,
    External,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Lang {
    Number {
        value: f32,
        help_data: HelpData,
    },
    Integer {
        value: i32,
        help_data: HelpData,
    },
    Bool {
        value: bool,
        help_data: HelpData,
    },
    Char {
        value: String,
        help_data: HelpData,
    },
    Scope {
        body: Vec<Lang>,
        help_data: HelpData,
    },
    Function {
        parameters: Vec<ArgumentType>,
        return_type: Type,
        body: Box<Lang>,
        help_data: HelpData,
    },
    Lambda {
        parameters: Vec<Lang>,
        body: Box<Lang>,
        help_data: HelpData,
    },
    Module {
        name: String,
        body: Vec<Lang>,
        module_position: ModulePosition,
        config: Config,
        help_data: HelpData,
    },
    Variable {
        name: String,
        is_opaque: bool,
        related_type: Type,
        help_data: HelpData,
    },
    FunctionApp {
        identifier: Box<Lang>,
        arguments: Vec<Lang>,
        help_data: HelpData,
    },
    VecFunctionApp {
        vector_type: VecType,
        identifier: Box<Lang>,
        arguments: Vec<Lang>,
        help_data: HelpData,
    },
    ArrayIndexing {
        identifier: Box<Lang>,
        indexing: Box<Lang>,
        help_data: HelpData,
    },
    Let {
        variable: Box<Lang>,
        r#type: Type,
        expression: Box<Lang>,
        help_data: HelpData,
    },
    Alias {
        identifier: Box<Lang>,
        parameters: Vec<Type>,
        target_type: Type,
        help_data: HelpData,
    },
    Array {
        value: Vec<Lang>,
        help_data: HelpData,
    },
    List {
        value: Vec<ArgumentValue>,
        help_data: HelpData,
    },
    DataFrame {
        value: Vec<ArgumentValue>,
        help_data: HelpData,
    },
    Tuple {
        value: Vec<Lang>,
        help_data: HelpData,
    },
    Lines {
        value: Vec<Lang>,
        help_data: HelpData,
    },
    Comment {
        value: String,
        help_data: HelpData,
    },
    ModuleImport {
        value: String,
        help_data: HelpData,
    },
    Import {
        value: Type,
        help_data: HelpData,
    },
    Test {
        value: Vec<Lang>,
        help_data: HelpData,
    },
    Return {
        value: Box<Lang>,
        help_data: HelpData,
    },
    VecBlock {
        value: String,
        help_data: HelpData,
    },
    Library {
        value: String,
        help_data: HelpData,
    },
    Exp {
        value: String,
        help_data: HelpData,
    },
    Vector {
        value: Vec<Lang>,
        help_data: HelpData,
    },
    Not {
        value: Box<Lang>,
        help_data: HelpData,
    },
    TestBlock {
        value: Box<Lang>,
        help_data: HelpData,
    },
    Use {
        lang: Box<Lang>,
        members: Box<Lang>,
        help_data: HelpData,
    },
    WhileLoop {
        condition: Box<Lang>,
        body: Box<Lang>,
        help_data: HelpData,
    },
    Sequence {
        body: Vec<Lang>,
        help_data: HelpData,
    },
    Tag {
        name: String,
        value: Box<Lang>,
        help_data: HelpData,
    },
    GenFunc {
        name: String,
        help_data: HelpData,
    },
    If {
        condition: Box<Lang>,
        if_block: Box<Lang>,
        else_block: Box<Lang>,
        help_data: HelpData,
    },
    Match {
        target: Box<Lang>,
        branches: Vec<(Lang, Box<Lang>)>,
        help_data: HelpData,
    },
    Assign {
        identifier: Box<Lang>,
        expression: Box<Lang>,
        help_data: HelpData,
    },
    Signature {
        identifier: Var,
        target_type: Type,
        help_data: HelpData,
    },
    ForLoop {
        identifier: Var,
        expression: Box<Lang>,
        body: Box<Lang>,
        help_data: HelpData,
    },
    RFunction {
        parameters: Vec<Lang>,
        body: String,
        help_data: HelpData,
    },
    KeyValue {
        key: String,
        value: Box<Lang>,
        help_data: HelpData,
    },
    Operator {
        operator: Op,
        rhs: Box<Lang>,
        lhs: Box<Lang>,
        help_data: HelpData,
    },
    /// Pattern matching on primitive types: `x as int => ...`
    /// TypePattern(variable_name, matched_type, help_data)
    TypePattern {
        variable_name: String,
        matched_type: Type,
        help_data: HelpData,
    },
    Union(Box<Lang>, Box<Lang>, HelpData),
    JSBlock(Box<Lang>, u32, HelpData),
    Break(HelpData),
    Null(HelpData),
    NA(HelpData),
    Empty(HelpData),
}

impl PartialEq for Lang {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lang::Number { value: a, .. }, Lang::Number { value: b, .. }) => a == b,
            (Lang::Integer { value: a, .. }, Lang::Integer { value: b, .. }) => a == b,
            (Lang::Bool { value: a, .. }, Lang::Bool { value: b, .. }) => a == b,
            (Lang::Char { value: a, .. }, Lang::Char { value: b, .. }) => a == b,
            (Lang::Union(a1, a2, _), Lang::Union(b1, b2, _)) => a1 == b1 && a2 == b2,
            (Lang::Scope { body: a, .. }, Lang::Scope { body: b, .. }) => a == b,
            (
                Lang::Function {
                    parameters: a1,
                    return_type: a2,
                    body: a3,
                    ..
                },
                Lang::Function {
                    parameters: b1,
                    return_type: b2,
                    body: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Lang::Module {
                    name: a1,
                    body: a2,
                    module_position: a3,
                    config: a4,
                    ..
                },
                Lang::Module {
                    name: b1,
                    body: b2,
                    module_position: b3,
                    config: b4,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4,
            (
                Lang::Variable {
                    name: a1,
                    is_opaque: a2,
                    related_type: a3,
                    ..
                },
                Lang::Variable {
                    name: b1,
                    is_opaque: b2,
                    related_type: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Lang::FunctionApp {
                    identifier: a1,
                    arguments: a2,
                    ..
                },
                Lang::FunctionApp {
                    identifier: b1,
                    arguments: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Lang::VecFunctionApp {
                    vector_type: a0,
                    identifier: a1,
                    arguments: a2,
                    ..
                },
                Lang::VecFunctionApp {
                    vector_type: b0,
                    identifier: b1,
                    arguments: b2,
                    ..
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2,
            (
                Lang::ArrayIndexing {
                    identifier: a1,
                    indexing: a2,
                    ..
                },
                Lang::ArrayIndexing {
                    identifier: b1,
                    indexing: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Lang::Let {
                    variable: a1,
                    r#type: a2,
                    expression: a3,
                    help_data: _,
                },
                Lang::Let {
                    variable: b1,
                    r#type: b2,
                    expression: b3,
                    help_data: _,
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Lang::Alias {
                    identifier: a1,
                    parameters: a2,
                    target_type: a3,
                    ..
                },
                Lang::Alias {
                    identifier: b1,
                    parameters: b2,
                    target_type: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (Lang::Array { value: a, .. }, Lang::Array { value: b, .. }) => a == b,
            (Lang::List { value: a, .. }, Lang::List { value: b, .. }) => a == b,
            (Lang::DataFrame { value: a, .. }, Lang::DataFrame { value: b, .. }) => a == b,
            (
                Lang::Tag {
                    name: a1,
                    value: a2,
                    ..
                },
                Lang::Tag {
                    name: b1,
                    value: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Lang::If {
                    condition: a1,
                    if_block: a2,
                    else_block: a3,
                    ..
                },
                Lang::If {
                    condition: b1,
                    if_block: b2,
                    else_block: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Lang::Match {
                    target: a1,
                    branches: a2,
                    ..
                },
                Lang::Match {
                    target: b1,
                    branches: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Tuple { value: a, .. }, Lang::Tuple { value: b, .. }) => a == b,
            (Lang::Lines { value: a, .. }, Lang::Lines { value: b, .. }) => a == b,
            (
                Lang::Assign {
                    identifier: a1,
                    expression: a2,
                    ..
                },
                Lang::Assign {
                    identifier: b1,
                    expression: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Comment { value: a, .. }, Lang::Comment { value: b, .. }) => a == b,
            (Lang::ModuleImport { value: a, .. }, Lang::ModuleImport { value: b, .. }) => a == b,
            (Lang::Import { value: a, .. }, Lang::Import { value: b, .. }) => a == b,
            (
                Lang::GenFunc {
                    name: a1,
                    help_data: a2,
                },
                Lang::GenFunc {
                    name: b1,
                    help_data: b2,
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Test { value: a, .. }, Lang::Test { value: b, .. }) => a == b,
            (Lang::Return { value: a, .. }, Lang::Return { value: b, .. }) => a == b,
            (Lang::VecBlock { value: a, .. }, Lang::VecBlock { value: b, .. }) => a == b,
            (Lang::Lambda { parameters: a, .. }, Lang::Lambda { parameters: b, .. }) => a == b,
            (Lang::Library { value: a, .. }, Lang::Library { value: b, .. }) => a == b,
            (Lang::Exp { value: a, .. }, Lang::Exp { value: b, .. }) => a == b,
            (
                Lang::Signature {
                    identifier: a1,
                    target_type: a2,
                    ..
                },
                Lang::Signature {
                    identifier: b1,
                    target_type: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Lang::ForLoop {
                    identifier: a1,
                    expression: a2,
                    body: a3,
                    ..
                },
                Lang::ForLoop {
                    identifier: b1,
                    expression: b2,
                    body: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Lang::RFunction {
                    parameters: a1,
                    body: a2,
                    ..
                },
                Lang::RFunction {
                    parameters: b1,
                    body: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Lang::KeyValue {
                    key: a1, value: a2, ..
                },
                Lang::KeyValue {
                    key: b1, value: b2, ..
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Vector { value: a, .. }, Lang::Vector { value: b, .. }) => a == b,
            (Lang::Sequence { body: a, .. }, Lang::Sequence { body: b, .. }) => a == b,
            (Lang::Not { value: a, .. }, Lang::Not { value: b, .. }) => a == b,
            (Lang::TestBlock { value: a, .. }, Lang::TestBlock { value: b, .. }) => a == b,
            (Lang::JSBlock(a1, a2, _), Lang::JSBlock(b1, b2, _)) => a1 == b1 && a2 == b2,
            (
                Lang::Use {
                    lang: a1,
                    members: a2,
                    ..
                },
                Lang::Use {
                    lang: b1,
                    members: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Empty(_), Lang::Empty(_)) => true,
            (
                Lang::WhileLoop {
                    condition: a1,
                    body: a2,
                    ..
                },
                Lang::WhileLoop {
                    condition: b1,
                    body: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Break(_), Lang::Break(_)) => true,
            (
                Lang::Operator {
                    operator: a1,
                    rhs: a2,
                    lhs: a3,
                    ..
                },
                Lang::Operator {
                    operator: b1,
                    rhs: b2,
                    lhs: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Lang::TypePattern {
                    variable_name: a1,
                    matched_type: a2,
                    ..
                },
                Lang::TypePattern {
                    variable_name: b1,
                    matched_type: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (Lang::Null(_), Lang::Null(_)) => true,
            (Lang::NA(_), Lang::NA(_)) => true,
            _ => false,
        }
    }
}

impl Eq for Lang {}

impl Default for Lang {
    fn default() -> Lang {
        builder::empty_lang()
    }
}

impl Locatable for Lang {
    fn get_help_data(&self) -> HelpData {
        Lang::get_help_data(self)
    }
}

impl From<Var> for Lang {
    fn from(val: Var) -> Self {
        Lang::Variable {
            name: val.name,
            is_opaque: val.is_opaque,
            related_type: val.related_type,
            help_data: val.help_data,
        }
    }
}

impl From<LangToken> for Lang {
    fn from(val: LangToken) -> Self {
        match val {
            LangToken::Expression(exp) => exp,
            LangToken::Operator(op) => panic!("Shouldn't convert the token to lang {}", op),
            LangToken::EmptyOperator => panic!("Shouldn't be empty "),
        }
    }
}

pub fn set_related_type_if_variable((val, arg): (&Lang, &Type)) -> Lang {
    let oargs = FunctionType::try_from(arg.clone()).map(|fn_t| fn_t.get_param_types());

    match oargs {
        Ok(args) if !args.is_empty() => val.set_type_if_variable(&args[0]),
        Ok(_) => val.clone(),
        Err(_) => val.clone(),
    }
}

//main
impl Lang {
    pub fn save_in_memory(&self) -> bool {
        matches!(self, Lang::Let { .. } | Lang::Assign { .. })
    }

    pub fn to_module(self, name: &str, environment: Environment) -> Self {
        match self {
            Lang::Lines {
                value: v,
                help_data: h,
            } => Lang::Module {
                name: name.to_string(),
                body: v,
                module_position: ModulePosition::External,
                config: Config::default().set_environment(environment),
                help_data: h,
            },
            s => s,
        }
    }

    fn set_type_if_variable(&self, typ: &Type) -> Lang {
        match self {
            Lang::Variable {
                name,
                is_opaque: spec,
                related_type: existing_type,
                help_data: h,
            } => {
                let new_type = if typ.is_generic() && !existing_type.is_empty() {
                    existing_type.clone()
                } else {
                    typ.clone()
                };
                Lang::Variable {
                    name: name.clone(),
                    is_opaque: *spec,
                    related_type: new_type,
                    help_data: h.clone(),
                }
            }
            _ => self.clone(),
        }
    }

    pub fn to_arg_type(&self) -> Option<ArgumentType> {
        match self {
            Lang::Let {
                variable: var,
                r#type: ty,
                expression: _,
                help_data: _,
            } => Some(ArgumentType::new(
                &Var::from_language((**var).clone()).unwrap().get_name(),
                ty,
            )),
            Lang::Alias {
                identifier: var,
                parameters: _types,
                target_type: ty,
                ..
            } => Some(ArgumentType::new(
                &Var::from_language((**var).clone()).unwrap().get_name(),
                ty,
            )),
            _ => None,
        }
    }

    pub fn extract_types_from_expression(&self, context: &Context) -> Vec<Type> {
        if self.is_value() {
            vec![typing(context, self).value.clone()]
        } else {
            match self {
                Lang::FunctionApp {
                    identifier: exp,
                    arguments: arg_typs,
                    ..
                } => {
                    let typs = exp.extract_types_from_expression(context);
                    let typs2 = arg_typs
                        .iter()
                        .flat_map(|x| x.extract_types_from_expression(context))
                        .collect::<Vec<_>>();
                    typs.iter().chain(typs2.iter()).cloned().collect()
                }
                _ => vec![],
            }
        }
    }

    pub fn is_value(&self) -> bool {
        matches!(
            self,
            Lang::Number { .. }
                | Lang::Integer { .. }
                | Lang::Bool { .. }
                | Lang::Char { .. }
                | Lang::Null(_)
                | Lang::Array { .. }
        )
    }

    pub fn is_undefined(&self) -> bool {
        if let Lang::Function { body, .. } = self {
            if let Lang::Scope { body: v, .. } = *body.clone() {
                let ele = v.first().unwrap();
                matches!(ele, Lang::Empty(_))
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Lang::Function { .. } | Lang::RFunction { .. })
    }

    pub fn infer_var_name(&self, args: &[Lang], context: &Context) -> Var {
        if let Some(first) = args.first() {
            let first = typing(context, first).value;
            Var::from_language(self.clone())
                .unwrap()
                .set_type(first.clone())
        } else {
            Var::from_language(self.clone()).unwrap()
        }
    }

    pub fn get_related_function(self, args: &[Lang], context: &Context) -> Option<FunctionType> {
        let var_name = self.infer_var_name(args, context);
        let fn_ty = typing(context, &var_name.to_language()).value;
        fn_ty.clone().to_function_type()
    }

    pub fn lang_substitution(&self, sub_var: &Lang, var: &Lang, context: &Context) -> String {
        if let Lang::Variable { name, .. } = var {
            let res = match self {
                Lang::Variable { help_data: h, .. } if self == sub_var => Lang::Exp {
                    value: format!("{}[[2]]", name),
                    help_data: h.clone(),
                },
                lang => lang.clone(),
            };
            res.to_r(context).0
        } else {
            panic!("var is not a variable")
        }
    }

    pub fn get_help_data(&self) -> HelpData {
        match self {
            Lang::Number { help_data: h, .. } => h,
            Lang::Integer { help_data: h, .. } => h,
            Lang::Char { help_data: h, .. } => h,
            Lang::Bool { help_data: h, .. } => h,
            Lang::Union(_, _, h) => h,
            Lang::Scope { help_data: h, .. } => h,
            Lang::Function { help_data: h, .. } => h,
            Lang::Module { help_data: h, .. } => h,
            Lang::Variable { help_data: h, .. } => h,
            Lang::FunctionApp { help_data: h, .. } => h,
            Lang::VecFunctionApp { help_data: h, .. } => h,
            Lang::ArrayIndexing { help_data: h, .. } => h,
            Lang::Let { help_data: h, .. } => h,
            Lang::Array { help_data: h, .. } => h,
            Lang::List { help_data: h, .. } => h,
            Lang::DataFrame { help_data: h, .. } => h,
            Lang::Alias { help_data: h, .. } => h,
            Lang::Tag { help_data: h, .. } => h,
            Lang::If { help_data: h, .. } => h,
            Lang::Match { help_data: h, .. } => h,
            Lang::Tuple { help_data: h, .. } => h,
            Lang::Lines { help_data: h, .. } => h,
            Lang::Assign { help_data: h, .. } => h,
            Lang::Comment { help_data: h, .. } => h,
            Lang::ModuleImport { help_data: h, .. } => h,
            Lang::Import { help_data: h, .. } => h,
            Lang::GenFunc { help_data: h, .. } => h,
            Lang::Test { help_data: h, .. } => h,
            Lang::Return { help_data: h, .. } => h,
            Lang::VecBlock { help_data: h, .. } => h,
            Lang::Lambda { help_data: h, .. } => h,
            Lang::Library { help_data: h, .. } => h,
            Lang::Exp { help_data: h, .. } => h,
            Lang::Empty(h) => h,
            Lang::Signature { help_data: h, .. } => h,
            Lang::ForLoop { help_data: h, .. } => h,
            Lang::RFunction { help_data: h, .. } => h,
            Lang::KeyValue { help_data: h, .. } => h,
            Lang::Vector { help_data: h, .. } => h,
            Lang::Not { help_data: h, .. } => h,
            Lang::Sequence { help_data: h, .. } => h,
            Lang::TestBlock { help_data: h, .. } => h,
            Lang::JSBlock(_, _, h) => h,
            Lang::Use { help_data: h, .. } => h,
            Lang::WhileLoop { help_data: h, .. } => h,
            Lang::Break(h) => h,
            Lang::Operator { help_data: h, .. } => h,
            Lang::TypePattern { help_data: h, .. } => h,
            Lang::Null(h) => h,
            Lang::NA(h) => h,
        }
        .clone()
    }

    pub fn linearize_array(&self) -> Vec<Lang> {
        match self {
            Lang::Array { value: v, .. } => v.iter().fold(Vec::<Lang>::new(), |acc, x: &Lang| {
                acc.iter()
                    .chain(x.linearize_array().iter())
                    .cloned()
                    .collect()
            }),
            _ => vec![self.to_owned()],
        }
    }

    pub fn is_r_function(&self) -> bool {
        matches!(self, Lang::RFunction { .. })
    }

    pub fn nb_params(&self) -> usize {
        self.simple_print();
        match self {
            Lang::Function {
                parameters: params, ..
            } => params.len(),
            _ => 0_usize,
        }
    }

    pub fn simple_print(&self) -> String {
        match self {
            Lang::Number { .. } => "Number".to_string(),
            Lang::Integer { .. } => "Integer".to_string(),
            Lang::Char { .. } => "Char".to_string(),
            Lang::Bool { .. } => "Bool".to_string(),
            Lang::Union(_, _, _) => "Union".to_string(),
            Lang::Scope { .. } => "Scope".to_string(),
            Lang::Function { .. } => "Function".to_string(),
            Lang::Module { .. } => "Module".to_string(),
            Lang::Variable { name, .. } => format!("Variable({})", name),
            Lang::FunctionApp {
                identifier: var, ..
            } => format!(
                "FunctionApp({})",
                Var::from_language(*(var.clone())).unwrap().get_name()
            ),
            Lang::VecFunctionApp {
                vector_type: vec_typ,
                identifier: var,
                ..
            } => format!(
                "VecFunctionApp({}, {})",
                vec_typ,
                Var::from_language(*(var.clone())).unwrap().get_name()
            ),
            Lang::ArrayIndexing { .. } => "ArrayIndexing".to_string(),
            Lang::Let { variable: var, .. } => format!(
                "let {}",
                Var::from_language((**var).clone()).unwrap().get_name()
            ),
            Lang::Array { .. } => "Array".to_string(),
            Lang::List { .. } => "Record".to_string(),
            Lang::DataFrame { .. } => "DataFrame".to_string(),
            Lang::Alias { .. } => "Alias".to_string(),
            Lang::Tag { .. } => "Tag".to_string(),
            Lang::If { .. } => "If".to_string(),
            Lang::Match { .. } => "Match".to_string(),
            Lang::Tuple { .. } => "Tuple".to_string(),
            Lang::Lines { .. } => "Sequence".to_string(),
            Lang::Assign { .. } => "Assign".to_string(),
            Lang::Comment { .. } => "Comment".to_string(),
            Lang::ModuleImport { .. } => "ModImp".to_string(),
            Lang::Import { .. } => "Import".to_string(),
            Lang::GenFunc { .. } => "GenFunc".to_string(),
            Lang::Test { .. } => "Test".to_string(),
            Lang::Return { .. } => "Return".to_string(),
            Lang::VecBlock { .. } => "VecBloc".to_string(),
            Lang::Lambda { .. } => "Lambda".to_string(),
            Lang::Library { .. } => "Library".to_string(),
            Lang::Exp { .. } => "Exp".to_string(),
            Lang::Empty(_) => "Empty".to_string(),
            Lang::Signature { .. } => "Signature".to_string(),
            Lang::ForLoop { .. } => "ForLoop".to_string(),
            Lang::RFunction { .. } => "RFunction".to_string(),
            Lang::KeyValue { .. } => "KeyValue".to_string(),
            Lang::Vector { .. } => "Vector".to_string(),
            Lang::Not { .. } => "Not".to_string(),
            Lang::Sequence { .. } => "Sequence".to_string(),
            Lang::TestBlock { .. } => "TestBlock".to_string(),
            Lang::JSBlock(_, _, _) => "JSBlock".to_string(),
            Lang::Use { .. } => "Use".to_string(),
            Lang::WhileLoop { .. } => "WhileLoop".to_string(),
            Lang::Break(_) => "Break".to_string(),
            Lang::Operator { .. } => "Operator".to_string(),
            Lang::TypePattern {
                variable_name: name,
                matched_type: typ,
                ..
            } => {
                format!("TypePattern({} as {})", name, typ.pretty2())
            }
            Lang::Null(_) => "Null".to_string(),
            Lang::NA(_) => "NA".to_string(),
        }
    }

    pub fn typing(&self, context: &Context) -> TypeContext {
        typing(context, self)
    }

    pub fn to_js(&self, context: &Context) -> (String, Context) {
        match self {
            Lang::Char { value: val, .. } => (format!("\\'{}\\'", val), context.clone()),
            Lang::Null(_) => ("null".to_string(), context.clone()),
            Lang::NA(_) => ("NA".to_string(), context.clone()),
            Lang::Bool { value: b, .. } => (b.to_string().to_uppercase(), context.clone()),
            Lang::Number { value: n, .. } => (format!("{}", n), context.clone()),
            Lang::Integer { value: i, .. } => (format!("{}", i), context.clone()),
            Lang::Let {
                variable: var,
                r#type: _,
                expression: body,
                help_data: _,
            } => (
                format!(
                    "let {} = {};",
                    Var::from_language(*(var.clone())).unwrap().get_name(),
                    body.to_js(context).0
                ),
                context.clone(),
            ),
            Lang::Assign {
                identifier: var,
                expression: body,
                ..
            } => (
                format!("{} = {};", var.to_js(context).0, body.to_js(context).0),
                context.clone(),
            ),
            Lang::Scope { body: langs, .. } => {
                let res = langs
                    .iter()
                    .map(|x| x.to_js(context).0)
                    .collect::<Vec<_>>()
                    .join("\n");
                (res, context.clone())
            }
            Lang::Return { value: exp, .. } => {
                (format!("return {};", exp.to_js(context).0), context.clone())
            }
            Lang::FunctionApp {
                identifier: exp,
                arguments: params,
                ..
            } => {
                let var = Var::try_from(exp.clone()).unwrap();
                let res = format!(
                    "{}({})",
                    var.get_name().replace("__", "."),
                    params
                        .iter()
                        .map(|x| x.to_js(context).0)
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                (res, context.clone())
            }
            Lang::Function {
                parameters: params,
                body,
                ..
            } => {
                let parameters = &params
                    .iter()
                    .map(|x| x.get_argument_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                (
                    format!("({}) => {{\n{}\n}}", parameters, body.to_js(context).0),
                    context.clone(),
                )
            }
            Lang::Use {
                lang: lib, members, ..
            } => {
                let body = match (**members).clone() {
                    Lang::Vector { value: v, .. } => v
                        .iter()
                        .map(|val: &Lang| val.to_js(context).0.replace("\\'", ""))
                        .collect::<Vec<_>>()
                        .join(", "),
                    Lang::Char { value: val, .. } => val.clone(),
                    lang => lang.simple_print(),
                };
                (
                    format!("import {{ {} }} from {};", body, lib.to_js(context).0),
                    context.clone(),
                )
            }
            Lang::Sequence { body: v, .. } => {
                let res = "[".to_string()
                    + &v.iter()
                        .map(|lang: &Lang| lang.to_js(context).0)
                        .collect::<Vec<_>>()
                        .join(", ")
                    + "]";
                (res, context.clone())
            }
            Lang::Array { value: v, .. } => {
                let res = "[".to_string()
                    + &v.iter()
                        .map(|lang: &Lang| lang.to_js(context).0)
                        .collect::<Vec<_>>()
                        .join(", ")
                    + "]";
                (res, context.clone())
            }
            Lang::Vector { value: v, .. } => {
                let res = "[".to_string()
                    + &v.iter()
                        .map(|lang: &Lang| lang.to_js(context).0)
                        .collect::<Vec<_>>()
                        .join(", ")
                    + "]";
                (res, context.clone())
            }
            Lang::List {
                value: arg_vals, ..
            } => {
                let res = "{".to_string()
                    + &arg_vals
                        .iter()
                        .map(|arg_val: &ArgumentValue| {
                            arg_val.get_argument().replace("'", "")
                                + ": "
                                + &arg_val.get_value().to_js(context).0
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                    + "}";
                (res, context.clone())
            }
            Lang::Lambda {
                parameters: params,
                body,
                ..
            } => {
                let param_names: Vec<String> = params
                    .iter()
                    .map(|p: &Lang| match p {
                        Lang::Variable { name, .. } => name.clone(),
                        _ => "x".to_string(),
                    })
                    .collect();
                let params_str = if param_names.len() == 1 {
                    param_names[0].clone()
                } else {
                    format!("({})", param_names.join(", "))
                };
                (
                    format!("{} => {}", params_str, body.to_js(context).0),
                    context.clone(),
                )
            }
            Lang::Operator {
                operator: op,
                rhs: e1,
                lhs: e2,
                ..
            } => (
                format!("{} {} {}", e1.to_js(context).0, op, e2.to_js(context).0),
                context.clone(),
            ),
            _ => self.to_r(context),
        }
    }

    pub fn to_simple_r(&self, context: &Context) -> (String, Context) {
        match self {
            Lang::Number { value: n, .. } => (n.to_string(), context.clone()),
            Lang::Array { value: v, .. } => {
                if v.len() == 1 {
                    v[0].to_simple_r(context)
                } else {
                    panic!("Not yet implemented for indexing of multiple elements")
                }
            }
            _ => self.to_r(context),
        }
    }

    pub fn to_module_member(self) -> Lang {
        match self {
            Lang::Module {
                name,
                body,
                help_data: h,
                ..
            } => Lang::Lines {
                value: body.clone(),
                help_data: h,
            }
            .to_module_helper(&name),
            res => res,
        }
    }

    pub fn to_module_helper(self, name: &str) -> Lang {
        match self.clone() {
            Lang::Variable { help_data: h, .. } => Lang::Operator {
                operator: Op::Dollar(h.clone()),
                rhs: Box::new(Var::from_name(name).to_language()),
                lhs: Box::new(self),
                help_data: h,
            },
            Lang::Let {
                variable: var,
                r#type: typ,
                expression: lang,
                help_data: h,
            } => {
                let expr = Lang::Operator {
                    operator: Op::Dollar(h.clone()),
                    rhs: var,
                    lhs: Box::new(Var::from_name(name).to_language()),
                    help_data: h.clone(),
                };
                Lang::Let {
                    variable: Box::new(expr),
                    r#type: typ,
                    expression: lang,
                    help_data: h,
                }
            }
            Lang::Alias {
                identifier: var,
                parameters: types,
                target_type: typ,
                help_data: h,
            } => {
                let expr = Lang::Operator {
                    operator: Op::Dollar(h.clone()),
                    rhs: Box::new(Var::from_name(name).to_language()),
                    lhs: var,
                    help_data: h.clone(),
                };
                Lang::Alias {
                    identifier: Box::new(expr),
                    parameters: types,
                    target_type: typ,
                    help_data: h,
                }
            }
            Lang::Function {
                parameters: args,
                return_type: typ,
                body,
                help_data: h,
            } => Lang::Function {
                parameters: args,
                return_type: typ,
                body: Box::new(body.to_module_helper(name)),
                help_data: h,
            },
            Lang::Lines {
                value: exprs,
                help_data: h,
            } => Lang::Lines {
                value: exprs
                    .iter()
                    .cloned()
                    .map(|expr: Lang| expr.to_module_helper(name))
                    .collect::<Vec<_>>(),
                help_data: h,
            },
            rest => rest,
        }
    }

    pub fn to_arg_value(self, type_module: &Type, context: &Context) -> Option<Vec<ArgumentValue>> {
        match self {
            Lang::Let {
                variable: lang,
                r#type: _,
                expression: body,
                help_data: h,
            } if Var::from_language(*lang.clone()).is_some() => {
                let var = Var::from_language(*lang).unwrap();
                type_module
                    .get_first_function_parameter_type(&var.get_name())
                    .map(|typ_par| {
                        var.clone().set_name(&format!(
                            "{}.{}",
                            var.get_name(),
                            context.get_type_anotation_no_parentheses(&typ_par)
                        ))
                    })
                    .map(|var2| {
                        Some(vec![
                            ArgumentValue(
                                var.get_name(),
                                Lang::GenFunc {
                                    name: var.get_name(),
                                    help_data: h,
                                },
                            ),
                            ArgumentValue(var2.get_name(), *body.clone()),
                        ])
                    })
                    .unwrap_or(Some(vec![ArgumentValue(var.get_name(), *body)]))
            }
            _ => None,
        }
    }

    pub fn get_token_type(&self) -> TokenKind {
        TokenKind::Expression
    }

    pub fn get_binding_power(&self) -> i32 {
        1
    }

    pub fn get_members_if_array(&self) -> Option<Vec<Lang>> {
        match self {
            Lang::Array { value: members, .. } => Some(members.clone()),
            _ => None,
        }
    }

    pub fn len(&self) -> i32 {
        match self {
            Lang::Integer { value: i, .. } => *i,
            Lang::Array { value: v, .. } => v.len() as i32,
            Lang::Vector { value: v, .. } => v.len() as i32,
            n => panic!("not implemented for language {}", n.simple_print()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn to_vec(self) -> Vec<Lang> {
        match self {
            Lang::Lines { value: v, .. } => v,
            l => vec![l],
        }
    }
}

impl From<Lang> for HelpData {
    fn from(val: Lang) -> Self {
        match val {
            Lang::Number { help_data: h, .. } => h,
            Lang::Integer { help_data: h, .. } => h,
            Lang::Bool { help_data: h, .. } => h,
            Lang::Char { help_data: h, .. } => h,
            Lang::Variable { help_data: h, .. } => h,
            Lang::Match { help_data: h, .. } => h,
            Lang::FunctionApp { help_data: h, .. } => h,
            Lang::VecFunctionApp { help_data: h, .. } => h,
            Lang::Empty(h) => h,
            Lang::Array { help_data: h, .. } => h,
            Lang::List { help_data: h, .. } => h,
            Lang::DataFrame { help_data: h, .. } => h,
            Lang::Scope { help_data: h, .. } => h,
            Lang::Let { help_data: h, .. } => h,
            Lang::Alias { help_data: h, .. } => h,
            Lang::Lambda { help_data: h, .. } => h,
            Lang::Function { help_data: h, .. } => h,
            Lang::VecBlock { help_data: h, .. } => h,
            Lang::If { help_data: h, .. } => h,
            Lang::Assign { help_data: h, .. } => h,
            Lang::Union(_, _, h) => h,
            Lang::Module { help_data: h, .. } => h,
            Lang::ModuleImport { help_data: h, .. } => h,
            Lang::Import { help_data: h, .. } => h,
            Lang::ArrayIndexing { help_data: h, .. } => h,
            Lang::Tag { help_data: h, .. } => h,
            Lang::Tuple { help_data: h, .. } => h,
            Lang::Lines { help_data: h, .. } => h,
            Lang::Comment { help_data: h, .. } => h,
            Lang::GenFunc { help_data: h, .. } => h,
            Lang::Test { help_data: h, .. } => h,
            Lang::Return { help_data: h, .. } => h,
            Lang::Library { help_data: h, .. } => h,
            Lang::Exp { help_data: h, .. } => h,
            Lang::Signature { help_data: h, .. } => h,
            Lang::ForLoop { help_data: h, .. } => h,
            Lang::RFunction { help_data: h, .. } => h,
            Lang::KeyValue { help_data: h, .. } => h,
            Lang::Vector { help_data: h, .. } => h,
            Lang::Not { help_data: h, .. } => h,
            Lang::Sequence { help_data: h, .. } => h,
            Lang::TestBlock { help_data: h, .. } => h,
            Lang::JSBlock(_, _, h) => h,
            Lang::Use { help_data: h, .. } => h,
            Lang::WhileLoop { help_data: h, .. } => h,
            Lang::Break(h) => h,
            Lang::Operator { help_data: h, .. } => h,
            Lang::TypePattern { help_data: h, .. } => h,
            Lang::Null(h) => h,
            Lang::NA(h) => h,
        }
        .clone()
    }
}

use std::fmt;
impl fmt::Display for Lang {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Lang::Variable {
                name,
                related_type: typ,
                ..
            } => format!("{} -> {}", name, typ),
            _ => format!("{:?}", self),
        };
        write!(f, "{}", res)
    }
}

pub fn format_backtick(s: String) -> String {
    "`".to_string() + &s.replace("`", "") + "`"
}

#[derive(Debug)]
pub struct ErrorStruct;

impl FromStr for Lang {
    type Err = ErrorStruct;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val = elements(s.into()).map(|x| x.1).unwrap_or_default();
        Ok(val)
    }
}
