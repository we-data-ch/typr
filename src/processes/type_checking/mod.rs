#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
pub mod function_application;
pub mod let_expression;
pub mod signature_expression;
pub mod type_checker;
pub mod type_comparison;
pub mod type_context;
pub mod unification;
pub mod unification_map;

use crate::components::context::config::TargetLanguage;
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::array_lang::ArrayLang;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::typer::Typer;
use crate::components::r#type::Type;
use crate::processes::type_checking::function_application::function_application;
use crate::processes::type_checking::let_expression::let_expression;
use crate::processes::type_checking::signature_expression::signature_expression;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::type_checking::type_context::TypeContext;
use crate::utils::builder;
use crate::utils::package_loader::PackageManager;
use std::collections::HashSet;
use std::error::Error;
use std::process::Command;

/// Result of type checking, containing the type context and collected errors
#[derive(Debug, Clone)]
pub struct TypingResult {
    /// The type context with the inferred type, transformed Lang, and updated context
    pub type_context: TypeContext,
    /// All type errors collected during type checking
    pub errors: Vec<TypRError>,
}

impl TypingResult {
    /// Create a new TypingResult from a TypeContext
    pub fn new(tc: TypeContext) -> Self {
        let errors = tc.errors.clone();
        TypingResult {
            type_context: tc,
            errors,
        }
    }

    /// Check if type checking produced any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the inferred type
    pub fn get_type(&self) -> &Type {
        &self.type_context.value
    }

    /// Get the transformed Lang expression
    pub fn get_lang(&self) -> &Lang {
        &self.type_context.lang
    }

    /// Get the updated context
    pub fn get_context(&self) -> &Context {
        &self.type_context.context
    }

    /// Get all errors
    pub fn get_errors(&self) -> &Vec<TypRError> {
        &self.errors
    }

    /// Display all errors as formatted strings
    pub fn display_errors(&self) -> Vec<String> {
        self.errors.iter().map(|e| e.clone().display()).collect()
    }
}

impl From<TypeContext> for TypingResult {
    fn from(tc: TypeContext) -> Self {
        TypingResult::new(tc)
    }
}

/// Perform type checking and return a TypingResult with all collected errors
///
/// This function performs type checking on the given expression and collects
/// all type errors instead of panicking. The caller can then handle the errors
/// appropriately (e.g., display all errors, continue with partial results).
///
/// # Arguments
/// * `context` - The typing context with known variables and types
/// * `expr` - The expression to type check
///
/// # Returns
/// A `TypingResult` containing the inferred type (or `Any` if errors occurred)
/// and all collected type errors.
pub fn typing_with_errors(context: &Context, expr: &Lang) -> TypingResult {
    let tc = typing(context, expr);
    TypingResult::new(tc)
}

pub fn execute_r_function(function_code: &str) -> Result<String, Box<dyn Error>> {
    let r_script = format!("{}\n", function_code);

    let output = Command::new("Rscript").arg("-e").arg(&r_script).output()?;

    if output.status.success() {
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout.trim().to_string())
    } else {
        let stderr = String::from_utf8(output.stderr)?;
        Err(format!("Erreur lors de l'exécution de R: {}", stderr).into())
    }
}

fn install_package(name: &str) -> () {
    let _status = Command::new("Rscript")
        .args([
            "-e",
            &format!(
                "if (!requireNamespace(\"{}\", quietly = TRUE)) install.packages(\"{}\")",
                name, name
            ),
        ])
        .status()
        .expect("failed to execute Rscript");
}

pub fn eval(context: &Context, expr: &Lang) -> TypeContext {
    match expr {
        Lang::Let(name, ty, exp, h) => let_expression(context, name, ty, exp, h),
        Lang::Alias(exp, params, typ, h) => {
            let var = Var::try_from(exp)
                .unwrap()
                .set_type(Type::Params(params.to_vec(), h.clone()));
            let alias_context = context.clone().push_alias(var.get_name(), typ.to_owned());
            let new_context =
                context
                    .clone()
                    .push_var_type(var.clone(), typ.clone(), &alias_context);
            (
                builder::unknown_function_type(),
                expr.clone(),
                new_context.push_alias(var.get_name(), typ.to_owned()),
            )
                .into()
        }
        Lang::Assign(left_expr, right_expr, h) => {
            let left_tc = typing(&context, left_expr);
            let right_tc = typing(&context, right_expr);
            let mut errors = left_tc.errors.clone();
            errors.extend(right_tc.errors.clone());

            let left_type = left_tc.value;
            let right_type = right_tc.value;
            let reduced_left_type = reduce_type(context, &left_type);
            let reduced_right_type = reduce_type(context, &right_type);

            if reduced_right_type.is_subtype(&reduced_left_type, context).0 {
                let var = Var::from_language((**left_expr).clone())
                    .unwrap()
                    .set_type(right_type.clone());
                TypeContext::new(
                    right_type.clone(),
                    expr.clone(),
                    context.clone().push_var_type(var, right_type, context),
                )
                .with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::Let(
                    left_type.clone().set_help_data(h.clone()),
                    right_type.clone(),
                )));
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            }
        }
        Lang::Library(name, _h) => {
            install_package(name);
            let package_manager = PackageManager::to_package(name).unwrap();
            if !package_manager.exists() {
                package_manager.clone().save();
            }
            let var_type = package_manager.load().unwrap();
            (
                builder::unknown_function_type(),
                expr.clone(),
                context.clone().extend_typing_context(var_type),
            )
                .into()
        }
        Lang::ModuleDecl(_name, _h) => (
            builder::unknown_function_type(),
            expr.clone(),
            context.clone(),
        )
            .into(),
        Lang::Signature(var, typ, _h) => signature_expression(context, expr, var, typ),
        Lang::TestBlock(body, _) => {
            //Needed to be type checked
            let tc = typing(context, body);
            TypeContext::new(
                builder::unknown_function_type(),
                expr.clone(),
                context.clone(),
            )
            .with_errors(tc.errors)
        }
        Lang::Module(_name, members, _position, _config, h) => {
            let module_expr = if members.len() > 1 {
                Lang::Lines(members.iter().cloned().collect(), h.clone())
            } else {
                members.iter().next().unwrap().clone()
            }; // TODO: Modules can't be empty
            let tc = typing(&Context::default(), &module_expr);
            TypeContext::new(
                builder::empty_type(),
                expr.clone(),
                context.clone() + tc.context,
            )
            .with_errors(tc.errors)
        }
        _ => (
            builder::unknown_function_type(),
            expr.clone(),
            context.clone(),
        )
            .into(),
    }
}

fn get_gen_type(type1: &Type, type2: &Type) -> Option<Vec<(Type, Type)>> {
    match (type1, type2) {
        (_, Type::Any(_)) => Some(vec![]),
        (Type::Integer(i, _), Type::Integer(j, _)) => (j.gen_of(i) || i == j).then(|| vec![]),
        (Type::Char(c, _), Type::Char(d, _)) => (d.gen_of(c) || d == c).then(|| vec![]),
        (_, Type::Generic(_, _))
        | (_, Type::IndexGen(_, _))
        | (_, Type::LabelGen(_, _))
        | (_, Type::Interface(_, _)) => Some(vec![(type1.clone(), type2.clone())]),
        (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
            let res = args1
                .iter()
                .zip(args2.iter())
                .chain([(&(**ret_typ1), &(**ret_typ2))].iter().cloned())
                .flat_map(|(typ1, typ2)| get_gen_type(typ1, typ2))
                .flat_map(|x| x)
                .collect::<Vec<_>>();
            Some(res)
        }
        (Type::Vec(_, ind1, typ1, _), Type::Vec(_, ind2, typ2, _)) => {
            let gen1 = get_gen_type(ind1, ind2);
            let gen2 = get_gen_type(typ1, typ2);
            match (gen1, gen2) {
                (None, _) | (_, None) => None,
                (Some(g1), Some(g2)) => Some(g1.iter().chain(g2.iter()).cloned().collect()),
            }
        }
        (Type::Record(v1, _), Type::Record(v2, _)) => {
            let res = v1
                .iter()
                .zip(v2.iter())
                .flat_map(|(argt1, argt2)| {
                    let gen1 = get_gen_type(&argt1.get_argument(), &argt2.get_argument())
                        .unwrap_or(vec![]);
                    let gen2 = get_gen_type(&argt1.get_type(), &argt2.get_type()).unwrap_or(vec![]);
                    gen1.iter().chain(gen2.iter()).cloned().collect::<Vec<_>>()
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>();
            Some(res)
        }
        (Type::Tag(_name1, typ1, _h1), Type::Tag(_name2, typ2, _h2)) => get_gen_type(typ1, typ2),
        (t1, t2) if t1.is_subtype(t2, &Context::empty()).0 => Some(vec![]),
        _ => None,
    }
}

//Check if we really have a type (type1) matched with a genery (type2)
pub fn match_types_to_generic(
    ctx: &Context,
    type1: &Type,
    type2: &Type,
) -> Option<Vec<(Type, Type)>> {
    let type1 = reduce_type(ctx, type1);
    let type2 = reduce_type(ctx, type2);
    get_gen_type(&type1, &type2).map(|vec| {
        vec.iter()
            .flat_map(|(arg, par)| unification::unify(ctx, &arg, &par))
            .collect::<Vec<_>>()
    })
}

fn are_homogenous_types(types: &[Type]) -> bool {
    types.windows(2).all(|w| w[0] == w[1])
}

trait WithLang2 {
    fn with_lang(self, expr: &Lang, context: &Context) -> (Type, Lang, Context);
}

impl WithLang2 for Type {
    fn with_lang(self, expr: &Lang, context: &Context) -> (Type, Lang, Context) {
        (self, expr.clone(), context.clone())
    }
}

//main
pub fn typing(context: &Context, expr: &Lang) -> TypeContext {
    match expr {
        Lang::Number(_, h) => (Type::Number(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Integer(i, h) => (
            builder::integer_type(*i).set_help_data(h.clone()),
            expr.clone(),
            context.clone(),
        )
            .into(),
        Lang::Bool(_, h) => (Type::Boolean(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Char(s, h) => (
            builder::character_type(&s).set_help_data(h.clone()),
            expr.clone(),
            context.clone(),
        )
            .into(),
        Lang::Empty(h) => (Type::Empty(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Operator(Op::And(_), e1, e2, _) | Lang::Operator(Op::Or(_), e1, e2, _) => {
            let tc1 = typing(context, e1);
            let tc2 = typing(context, e2);
            let mut errors = tc1.errors.clone();
            errors.extend(tc2.errors.clone());

            if tc1.value.is_boolean() && tc2.value.is_boolean() {
                TypeContext::new(builder::boolean_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::WrongExpression(
                    expr.get_help_data(),
                )));
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            }
        }
        Lang::Operator(Op::Eq(_), e1, e2, _)
        | Lang::Operator(Op::LesserOrEqual(_), e1, e2, _)
        | Lang::Operator(Op::GreaterOrEqual(_), e1, e2, _)
        | Lang::Operator(Op::GreaterThan(_), e1, e2, _)
        | Lang::Operator(Op::LesserThan(_), e1, e2, _) => {
            let tc1 = typing(context, e1);
            let tc2 = typing(context, e2);
            let mut errors = tc1.errors.clone();
            errors.extend(tc2.errors.clone());

            if tc1.value == tc2.value {
                TypeContext::new(builder::boolean_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::WrongExpression(
                    expr.get_help_data(),
                )));
                TypeContext::new(builder::boolean_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            }
        }
        Lang::Operator(Op::Dot(_), e1, e2, _) | Lang::Operator(Op::Pipe(_), e1, e2, _) => {
            if let Lang::FunctionApp(exp, v, h) = (**e2).clone() {
                let fun_app = Lang::FunctionApp(
                    exp,
                    [(**e1).clone()]
                        .iter()
                        .chain(v.iter())
                        .cloned()
                        .collect::<Vec<_>>(),
                    h.clone(),
                );
                typing(context, &fun_app)
            } else {
                let tc2 = typing(context, e2);
                let mut errors = tc2.errors.clone();
                let ty2 = tc2.value.clone().reduce(context);
                match (ty2.clone(), *e1.clone()) {
                    (Type::Record(fields, _), Lang::Variable(name, _, _, h)) => {
                        match fields
                            .iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        {
                            Some(arg_typ) => {
                                TypeContext::new(arg_typ.1.clone(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::FieldNotFound(
                                    (name, h),
                                    ty2,
                                )));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                        }
                    }
                    (Type::Record(fields, _), Lang::Char(name, h)) => {
                        match fields
                            .iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        {
                            Some(arg_typ) => {
                                TypeContext::new(arg_typ.1.clone(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::FieldNotFound(
                                    (name, h),
                                    ty2,
                                )));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                        }
                    }
                    (Type::Tuple(vals, _), Lang::Integer(i, h)) => {
                        match vals.iter().nth((i - 1) as usize) {
                            Some(typ) => {
                                TypeContext::new(typ.clone(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::WrongExpression(h)));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                        }
                    }
                    (Type::Record(fields1, h), Lang::Record(_, _)) => {
                        let tc1 = e1.typing(context);
                        errors.extend(tc1.errors.clone());
                        let fields3: HashSet<_> = match tc1.value {
                            Type::Record(fields2, _) => fields1.union(&fields2).cloned().collect(),
                            _ => {
                                errors.push(TypRError::Type(TypeError::WrongExpression(
                                    e1.get_help_data(),
                                )));
                                fields1
                            }
                        };
                        TypeContext::new(
                            Type::Record(fields3, h.clone()),
                            expr.clone(),
                            context.clone(),
                        )
                        .with_errors(errors)
                    }
                    (Type::Generic(_, _), Lang::Record(_, _)) => {
                        let tc1 = e1.typing(context);
                        errors.extend(tc1.errors.clone());
                        TypeContext::new(
                            builder::intersection_type(&[ty2.clone(), tc1.value]),
                            expr.clone(),
                            context.clone(),
                        )
                        .with_errors(errors)
                    }
                    (_, _) => {
                        errors.push(TypRError::Type(TypeError::WrongExpression(
                            expr.get_help_data(),
                        )));
                        TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                            .with_errors(errors)
                    }
                }
            }
        }
        Lang::Operator(Op::Dollar(hd), e1, e2, _) => {
            let op = match expr {
                Lang::Operator(op, _, _, _) => op.clone(),
                _ => Op::Dollar(HelpData::default()),
            };
            let tc1 = typing(context, e1);
            let mut errors = tc1.errors.clone();
            let ty1 = tc1.value.clone();
            match (ty1.reduce(context), *e2.clone(), &op) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, h), _) => {
                    match fields
                        .iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                    {
                        Some(arg_typ) => {
                            TypeContext::new(arg_typ.1.clone(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::FieldNotFound((name, h), ty1)));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (Type::Module(fields, _), Lang::Variable(name, _, _, h), _) => {
                    match fields
                        .iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                    {
                        Some(arg_typ) => {
                            TypeContext::new(arg_typ.1.clone(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::UndefinedVariable(
                                Lang::Variable(name, false, builder::any_type(), h.clone()),
                            )));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (Type::Module(fields, _), Lang::FunctionApp(exp, _, _), _) => {
                    match Var::from_language(*exp.clone()) {
                        Some(var) => {
                            match fields
                                .iter()
                                .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                            {
                                Some(arg_typ) => match FunctionType::try_from(arg_typ.get_type()) {
                                    Ok(ft) => TypeContext::new(
                                        ft.get_return_type(),
                                        expr.clone(),
                                        context.clone(),
                                    )
                                    .with_errors(errors),
                                    Err(_) => {
                                        errors.push(TypRError::Type(TypeError::WrongExpression(
                                            exp.get_help_data(),
                                        )));
                                        TypeContext::new(
                                            builder::any_type(),
                                            expr.clone(),
                                            context.clone(),
                                        )
                                        .with_errors(errors)
                                    }
                                },
                                None => {
                                    errors.push(TypRError::Type(TypeError::FunctionNotFound(var)));
                                    TypeContext::new(
                                        builder::any_type(),
                                        expr.clone(),
                                        context.clone(),
                                    )
                                    .with_errors(errors)
                                }
                            }
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::WrongExpression(
                                exp.get_help_data(),
                            )));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (Type::Record(fields, _), Lang::Char(name, h), _) => {
                    match fields
                        .iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                    {
                        Some(arg_typ) => {
                            TypeContext::new(arg_typ.1.clone(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::FieldNotFound((name, h), ty1)));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (Type::Tuple(vals, _), Lang::Integer(i, h), _) => {
                    match vals.iter().nth((i - 1) as usize) {
                        Some(typ) => TypeContext::new(typ.clone(), expr.clone(), context.clone())
                            .with_errors(errors),
                        None => {
                            errors.push(TypRError::Type(TypeError::WrongExpression(h)));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (Type::Record(fields1, h), Lang::Record(fields2, _), _) => {
                    let at = fields2[0].clone();
                    let fields3 = fields1
                        .iter()
                        .map(replace_fields_type_if_needed(context, at))
                        .collect::<HashSet<_>>();
                    TypeContext::new(
                        Type::Record(fields3, h.clone()),
                        expr.clone(),
                        context.clone(),
                    )
                    .with_errors(errors)
                }
                (Type::Record(fields, _), Lang::FunctionApp(exp, params, _), _) => {
                    match Var::from_language(*exp.clone()) {
                        Some(var) => {
                            match fields
                                .iter()
                                .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                            {
                                Some(arg_typ) => {
                                    let typ = arg_typ.get_type();
                                    let tc = typing(
                                        &context.clone().push_var_type(var, typ, context),
                                        e2,
                                    );
                                    errors.extend(tc.errors.clone());
                                    TypeContext::new(tc.value, expr.clone(), context.clone())
                                        .with_errors(errors)
                                }
                                None => {
                                    errors.push(TypRError::Type(TypeError::FunctionNotFound(
                                        var.set_type_from_params(&params, context),
                                    )));
                                    TypeContext::new(
                                        builder::any_type(),
                                        expr.clone(),
                                        context.clone(),
                                    )
                                    .with_errors(errors)
                                }
                            }
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::WrongExpression(
                                exp.get_help_data(),
                            )));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (Type::UnknownFunction(h), Lang::FunctionApp(_, _, _), _) => {
                    TypeContext::new(Type::UnknownFunction(h), (*expr).clone(), context.clone())
                        .with_errors(errors)
                }
                (Type::Vec(vtype, n, _, h), Lang::Variable(_, _, _, _), _) => {
                    match ArrayLang::try_from(e1) {
                        Ok(arr_lang) => match arr_lang.get_first_argument() {
                            Some(first_arg) => {
                                let tc = typing(
                                    context,
                                    &builder::operation(
                                        Op::Dollar(hd.clone()),
                                        first_arg,
                                        (**e2).clone(),
                                    ),
                                );
                                errors.extend(tc.errors.clone());
                                TypeContext::new(
                                    Type::Vec(vtype, n, Box::new(tc.value), h.clone()),
                                    tc.lang,
                                    context.clone(),
                                )
                                .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::WrongExpression(
                                    expr.get_help_data(),
                                )));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                        },
                        Err(_) => {
                            errors.push(TypRError::Type(TypeError::WrongExpression(
                                expr.get_help_data(),
                            )));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (_, Lang::FunctionApp(exp, args, h2), Op::Dot(_)) => {
                    let tc = typing(
                        context,
                        &Lang::FunctionApp(
                            exp,
                            [(**e1).clone()]
                                .iter()
                                .chain(args.iter())
                                .cloned()
                                .collect(),
                            h2,
                        ),
                    );
                    errors.extend(tc.errors);
                    TypeContext::new(tc.value, tc.lang, tc.context).with_errors(errors)
                }
                (_a, _b, _c) => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(
                        expr.get_help_data(),
                    )));
                    TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors)
                }
            }
        }
        Lang::Operator(op, e1, e2, h) => {
            let var_exp = Var::from_name(&format!("`{}`", op))
                .set_help_data(e1.get_help_data())
                .to_language();
            let fun_app = Lang::FunctionApp(
                Box::new(var_exp),
                vec![(**e1).clone(), (**e2).clone()],
                h.clone(),
            );
            typing(context, &fun_app)
        }
        Lang::Function(params, ret_ty, body, h) => {
            let list_of_types = params
                .iter()
                .map(ArgumentType::get_type)
                .collect::<Vec<_>>();
            let sub_context = params
                .into_iter()
                .map(|arg_typ| arg_typ.clone().to_var(context))
                .zip(
                    list_of_types
                        .clone()
                        .into_iter()
                        .map(|typ| typ.reduce(context)),
                )
                .fold(context.clone(), |cont, (var, typ)| {
                    cont.clone().push_var_type(var, typ, &cont)
                });
            let body_type = body.typing(&sub_context);
            let mut errors = body_type.errors.clone();
            let reduced_body_type = body_type.value.clone().reduce(&sub_context);
            let reduced_expected_ty = ret_ty.reduce(&context);
            if !reduced_body_type
                .is_subtype(&reduced_expected_ty, context)
                .0
            {
                errors.push(TypRError::Type(TypeError::UnmatchingReturnType(
                    ret_ty.clone(),
                    body_type.value.clone(),
                )));
            }
            TypeContext::new(
                Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone()),
                expr.clone(),
                body_type.context,
            )
            .with_errors(errors)
        }
        Lang::Lines(exprs, _h) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                let tc = typing(context, &res);
                let (typ, langs, errors) = (tc.value, tc.lang, tc.errors);
                TypeContext::new(
                    typ.clone(),
                    langs.clone(),
                    context
                        .clone()
                        .push_var_type(Var::from("_out"), typ.clone(), &context),
                )
                .with_errors(errors)
            } else if exprs.len() == 0 {
                TypeContext::new(
                    builder::unknown_function_type(),
                    expr.clone(),
                    context.clone(),
                )
            } else {
                let context2 = context.clone();
                let mut exprs2 = exprs.clone();
                let exp = exprs2.pop().unwrap();
                let mut all_errors = Vec::new();
                let new_context = exprs.iter().fold(context2, |ctx, expr| {
                    let tc = typing(&ctx, expr);
                    all_errors.extend(tc.errors);
                    tc.context
                });
                let final_tc = typing(&new_context, &exp);
                all_errors.extend(final_tc.errors);
                TypeContext::new(final_tc.value, final_tc.lang, final_tc.context)
                    .with_errors(all_errors)
            }
        }
        Lang::FunctionApp(fn_var_name, values, h) => {
            function_application(context, fn_var_name, values, h)
        }
        Lang::Tag(name, tag_expr, h) => {
            let tc = typing(context, tag_expr);
            TypeContext::new(
                Type::Tag(name.clone(), Box::new(tc.value.clone()), h.clone()),
                expr.clone(),
                context.clone(),
            )
            .with_errors(tc.errors)
        }
        Lang::If(cond, true_branch, false_branch, _h) => {
            let cond_tc = typing(context, cond);
            let mut errors = cond_tc.errors.clone();

            if cond_tc.value.is_boolean() {
                let true_tc = typing(context, true_branch);
                let false_tc = typing(context, false_branch);
                errors.extend(true_tc.errors);
                errors.extend(false_tc.errors);

                let result_type = if true_tc.value == false_tc.value {
                    true_tc.value
                } else if false_tc.value.is_empty() {
                    true_tc.value
                } else {
                    builder::union_type(&[true_tc.value, false_tc.value])
                };
                TypeContext::new(result_type, expr.clone(), context.clone()).with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::WrongExpression(
                    cond.get_help_data(),
                )));
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            }
        }
        Lang::Array(exprs, h) => {
            let type_contexts: Vec<_> = exprs.iter().map(|expr| typing(context, expr)).collect();
            let mut errors: Vec<TypRError> = type_contexts
                .iter()
                .flat_map(|tc| tc.errors.clone())
                .collect();
            let types: Vec<_> = type_contexts
                .iter()
                .map(|tc| tc.value.clone().reduce(context))
                .collect();

            if exprs.is_empty() {
                let new_type = "[0, Empty]"
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            } else if are_homogenous_types(&types) {
                let new_type = format!("[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
                let new_type = format!("[{}, Any]", exprs.len())
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            }
        }
        Lang::Vector(exprs, h) => {
            let type_contexts: Vec<_> = exprs.iter().map(|expr| typing(context, expr)).collect();
            let mut errors: Vec<TypRError> = type_contexts
                .iter()
                .flat_map(|tc| tc.errors.clone())
                .collect();
            let types: Vec<_> = type_contexts.iter().map(|tc| tc.value.clone()).collect();

            if exprs.is_empty() {
                let new_type = "Vec[0, Any]"
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            } else if are_homogenous_types(&types) {
                let new_type = format!("Vec[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
                let new_type = format!("Vec[{}, Any]", exprs.len())
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            }
        }
        Lang::Sequence(exprs, h) => {
            let type_contexts: Vec<_> = exprs.iter().map(|expr| typing(context, expr)).collect();
            let mut errors: Vec<TypRError> = type_contexts
                .iter()
                .flat_map(|tc| tc.errors.clone())
                .collect();
            let types: Vec<_> = type_contexts.iter().map(|tc| tc.value.clone()).collect();

            if exprs.is_empty() {
                let new_type = "Seq[0, Empty]"
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            } else if are_homogenous_types(&types) {
                let new_type = format!("Seq[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            } else {
                errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
                let new_type = format!("Seq[{}, Any]", exprs.len())
                    .parse::<Type>()
                    .unwrap()
                    .set_help_data(h.clone());
                TypeContext::new(
                    new_type.clone(),
                    expr.clone(),
                    context.clone().push_types(&[new_type]),
                )
                .with_errors(errors)
            }
        }
        Lang::Record(fields, h) => {
            let type_contexts: Vec<_> = fields
                .iter()
                .map(|arg_val| typing(context, &arg_val.get_value()))
                .collect();
            let errors: Vec<TypRError> = type_contexts
                .iter()
                .flat_map(|tc| tc.errors.clone())
                .collect();
            let field_types = fields
                .iter()
                .zip(type_contexts.iter())
                .map(|(arg_val, tc)| (arg_val.get_argument(), tc.value.clone()).into())
                .collect();
            TypeContext::new(
                Type::Record(field_types, h.clone()),
                expr.clone(),
                context.clone(),
            )
            .with_errors(errors)
        }
        Lang::Match(match_exp, var, branches, _h) => {
            let match_tc = typing(context, &**match_exp);
            let mut errors = match_tc.errors.clone();
            let var_ty = reduce_type(context, &match_tc.value);

            match var_ty {
                typ if matches!(&typ, Type::Operator(TypeOperator::Union, _, _, _)) => {
                    let union_types = flatten_operator_union(&typ);
                    let set = branches
                        .iter()
                        .map(|(t, _)| t)
                        .cloned()
                        .collect::<HashSet<Type>>();
                    if union_types != set {
                        errors.push(TypRError::Type(TypeError::WrongExpression(
                            match_exp.get_help_data(),
                        )));
                    }
                    let branch_tcs: Vec<_> = branches
                        .iter()
                        .map(|(typ, bexp)| {
                            let new_context =
                                context
                                    .clone()
                                    .push_var_type(var.clone(), typ.clone(), context);
                            typing(&new_context, bexp)
                        })
                        .collect();
                    errors.extend(branch_tcs.iter().flat_map(|tc| tc.errors.clone()));
                    let types: Vec<_> = branch_tcs.iter().map(|tc| tc.value.clone()).collect();

                    let output_type = if types.len() == 1 {
                        types[0].clone()
                    } else {
                        builder::union_type(&types)
                    };
                    TypeContext::new(output_type, expr.clone(), context.clone()).with_errors(errors)
                }
                _ => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(
                        match_exp.get_help_data(),
                    )));
                    TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors)
                }
            }
        }
        Lang::ArrayIndexing(arr_exp, index, h) => {
            let tc = typing(context, arr_exp);
            let mut errors = tc.errors.clone();
            let typ1 = tc.value;
            let args_target = typ1.clone().linearize();

            match index.get_members_if_array() {
                Some(members) => {
                    let args_index: Vec<_> = members
                        .iter()
                        .map(|x| builder::integer_type(x.len()).set_help_data((*x).clone().into()))
                        .collect();
                    let is_indexable = args_target
                        .iter()
                        .zip(args_index.iter())
                        .all(|(target, idx)| idx <= target);
                    let typ2 =
                        Type::to_array2(args_index).set_help_data((**arr_exp).clone().into());

                    if is_indexable {
                        TypeContext::new(typ2, expr.clone(), context.clone()).with_errors(errors)
                    } else {
                        errors.push(TypRError::Type(TypeError::WrongIndexing(
                            typ1,
                            typ2.clone(),
                        )));
                        TypeContext::new(typ2, expr.clone(), context.clone()).with_errors(errors)
                    }
                }
                None => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
                    TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors)
                }
            }
        }
        Lang::Variable(_, _, _, _) => {
            let old_var = Var::try_from(expr.clone()).unwrap();
            let var = context.get_true_variable(&old_var);
            context
                .get_type_from_existing_variable(var)
                .with_lang(expr, context)
                .into()
        }
        Lang::Scope(expr, _) if expr.len() == 1 => typing(context, &expr[0]),
        Lang::Scope(expr, h) => typing(context, &Lang::Lines(expr.to_vec(), h.clone())),
        Lang::Tuple(elements, h) => {
            let tcs: Vec<_> = elements.iter().map(|x| typing(context, x)).collect();
            let errors: Vec<TypRError> = tcs.iter().flat_map(|tc| tc.errors.clone()).collect();
            let types: Vec<_> = tcs.iter().map(|tc| tc.value.clone()).collect();
            TypeContext::new(Type::Tuple(types, h.clone()), expr.clone(), context.clone())
                .with_errors(errors)
        }
        Lang::VecBlock(_, h) => {
            TypeContext::new(Type::Empty(h.clone()), expr.clone(), context.clone())
        }
        Lang::RFunction(_, _, h) => TypeContext::new(
            Type::UnknownFunction(h.clone()),
            expr.clone(),
            context.clone(),
        ),
        Lang::ForLoop(var, iter, body, h) => {
            let iter_tc = typing(context, iter);
            let mut errors = iter_tc.errors.clone();

            match iter_tc.value.to_array() {
                Some(arr) => {
                    let base_type = arr.base_type;
                    let var = var.clone().set_type(base_type.clone());
                    let _typer_result = Typer::from(context.clone())
                        .set_type(base_type)
                        .set_var(var)
                        .push_var_type()
                        .typing((**body).clone());
                    TypeContext::new(
                        builder::unknown_function_type(),
                        expr.clone(),
                        context.clone(),
                    )
                    .with_errors(errors)
                }
                None => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
                    TypeContext::new(
                        builder::unknown_function_type(),
                        expr.clone(),
                        context.clone(),
                    )
                    .with_errors(errors)
                }
            }
        }
        Lang::Not(not_exp, h) => {
            let tc = typing(context, not_exp);
            let mut errors = tc.errors.clone();

            match tc.value {
                Type::Boolean(_) => {
                    TypeContext::new(Type::Boolean(h.clone()), expr.clone(), context.clone())
                        .with_errors(errors)
                }
                _ => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(
                        not_exp.get_help_data(),
                    )));
                    TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors)
                }
            }
        }
        Lang::JSBlock(body, _, h) => {
            let js_context = Context::default().set_target_language(TargetLanguage::JS);
            let _ = typing(&js_context, body).context;
            //TODO add js subcontext
            let (new_context, id) = (context.clone(), 0); //.add_js_subcontext(js_context);
            let new_expr = Lang::JSBlock(body.clone(), id, h.clone());
            builder::character_type_default()
                .with_lang(&new_expr, &new_context)
                .into()
        }
        Lang::Let(..) => eval(context, expr),
        Lang::Assign(..) => eval(context, expr),
        Lang::Alias(..) => eval(context, expr).with_lang(expr).into(),
        Lang::Library(..) => eval(context, expr).with_lang(expr).into(),
        Lang::ModuleDecl(..) => eval(context, expr).with_lang(expr).into(),
        Lang::TestBlock(..) => eval(context, expr).with_lang(expr).into(),
        Lang::Signature(..) => eval(context, expr).with_lang(expr).into(),
        Lang::Return(exp, _) => typing(context, exp),
        Lang::Module(_, _, _position, _, _) => eval(context, expr).with_lang(expr).into(),
        _ => builder::any_type().with_lang(expr, context).into(),
    }
}

/// Flatten a nested `Type::Operator(Union, ...)` tree into a flat `HashSet<Type>`.
fn flatten_operator_union(typ: &Type) -> HashSet<Type> {
    match typ {
        Type::Operator(TypeOperator::Union, t1, t2, _) => {
            let mut set = flatten_operator_union(t1);
            set.extend(flatten_operator_union(t2));
            set
        }
        other => {
            let mut set = HashSet::new();
            set.insert(other.clone());
            set
        }
    }
}

fn replace_fields_type_if_needed(
    context: &Context,
    at: ArgumentValue,
) -> impl FnMut(&ArgumentType) -> ArgumentType + use<'_> {
    move |arg_typ2| {
        (arg_typ2.get_argument_str() == at.get_argument())
            .then_some(ArgumentType::new(
                &at.get_argument(),
                &typing(context, &at.get_value()).value,
            ))
            .unwrap_or(arg_typ2.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::fluent_parser::FluentParser;
    //use crate::processes::parsing::parse;
    use super::*;

    #[test]
    fn test_function_application_unknown_function() {
        // Test that calling an unknown function collects an error instead of panicking
        let res = "typr(true)".parse::<Lang>().unwrap();
        let context = Context::default();
        let result = typing_with_errors(&context, &res);

        // Should have collected an error for the unknown function
        assert!(
            result.has_errors(),
            "Expected an error for unknown function 'typr'"
        );

        // The inferred type should be Any when the function is not found
        assert_eq!(result.get_type().clone(), builder::any_type());
    }

    #[test]
    fn test_let1() {
        let context = Context::default();
        let lang = Var::default().set_name("a");
        let typ = builder::integer_type_default();
        let context2 = context
            .clone()
            .push_var_type(lang.clone(), typ.clone(), &context);
        let _ = context2.get_type_from_variable(&lang);
        assert!(true)
    }

    #[test]
    fn test_let2() {
        //let context = Context::default();
        //let let_exps = parse("let a: int <- 8;".into());
        //let let_exp = let_exps.clone();
        //let var = Var::default().set_name("a");
        //let new_context = typing(&context, &let_exp).context;
        //let res = new_context.get_type_from_variable(&var);
        //let typ = builder::integer_type_default();
        assert!(true);
    }

    #[test]
    fn test_let2_0() {
        //let context = Context::default();
        //let let_exps = parse("a".into());
        //let let_exp = let_exps.clone();
        //let var = Var::default().set_name("a");
        //let new_context = typing(&context, &let_exp).context;
        //let res = new_context.get_type_from_variable(&var);
        //let typ = builder::integer_type_default();
        assert!(true);
    }

    #[test]
    fn test_let3() {
        let fp = FluentParser::new()
            .push("let n <- 8;")
            .parse_type_next()
            .push("n")
            .parse_next();
        println!("{}", fp);
        assert!(true)
    }

    #[test]
    fn test_simple_signature1() {
        let val = FluentParser::new()
            .push("@as__character: (Any) -> char;")
            .parse_type_next()
            .push("as__character(3)")
            .parse_type_next()
            .get_last_type();
        println!("{}", val);
        assert!(true);
    }

    #[test]
    fn test_function_return_type1() {
        let typ = FluentParser::new()
            .set_context(Context::default())
            .push("@incr: (int) -> int;")
            .parse_type_next()
            .push("incr([1, 2])")
            .parse_type_next()
            .get_last_type();
        println!("{}", typ.pretty());
        assert!(true);
    }

    #[test]
    fn test_function_return_type2() {
        let typ = FluentParser::new()
            .set_context(Context::default())
            .push("@scale: (bool, int) -> bool;")
            .parse_type_next()
            .push("scale([true, false], 2)")
            .parse_type_next()
            .get_last_type();
        println!("{}", typ.pretty());
        assert!(true);
    }
}
