#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
pub mod function;
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
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::type_checking::function::function;
use crate::processes::type_checking::function_application::function_application;
use crate::processes::type_checking::let_expression::let_expression;
use crate::processes::type_checking::signature_expression::signature_expression;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::type_checking::type_context::TypeContext;
use crate::utils::builder;
use std::collections::HashSet;
use std::error::Error;

#[cfg(not(feature = "wasm"))]
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

/// Execute an R function and return the output.
/// Only available in native mode (not in WASM).
#[cfg(not(feature = "wasm"))]
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

/// Stub for WASM mode - R execution is not supported
#[cfg(feature = "wasm")]
pub fn execute_r_function(_function_code: &str) -> Result<String, Box<dyn Error>> {
    Err("R execution is not supported in WASM mode".into())
}

/// Check if an R package is already installed in the system.
/// Only available in native mode.
#[cfg(not(feature = "wasm"))]
fn is_package_installed(name: &str) -> bool {
    let check_code = format!("length(find.package(\"{}\", quiet = TRUE)) > 0", name);
    let output = Command::new("Rscript").args(["-e", &check_code]).output();

    match output {
        Ok(out) => {
            let stdout = String::from_utf8_lossy(&out.stdout);
            stdout.trim().contains("TRUE")
        }
        Err(_) => false,
    }
}

/// Install an R package. Only available in native mode.
/// First checks if the package is already installed in the system.
#[cfg(not(feature = "wasm"))]
fn install_package(name: &str) {
    if is_package_installed(name) {
        return;
    }

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

/// Stub for WASM mode - package installation is not supported
#[cfg(feature = "wasm")]
fn install_package(_name: &str) {
    // No-op in WASM mode - packages must be pre-registered
}

pub fn eval(context: &Context, expr: &Lang) -> TypeContext {
    match expr {
        Lang::Let {
            variable: name,
            r#type: ty,
            expression: exp,
            is_public,
            help_data: h,
        } => let_expression(context, name, ty, exp, *is_public, h),
        Lang::Alias {
            identifier: exp,
            parameters: params,
            target_type: typ,
            help_data: h,
            ..
        } => {
            let var = match Var::try_from(exp) {
                Ok(v) => v.set_type(Type::Params(params.to_vec(), h.clone())),
                Err(_) => {
                    return TypeContext::new(
                        builder::unknown_function_type(),
                        expr.clone(),
                        context.clone(),
                    )
                }
            };
            // Inside a module body, opaque aliases are transparent so internal
            // functions can use operators on the underlying type.
            let effective_var = if context.is_in_module_body() && var.get_opacity() {
                var.clone().set_opacity(false)
            } else {
                var.clone()
            };
            let alias_context = context
                .clone()
                .push_alias(effective_var.get_name(), typ.to_owned());
            let new_context =
                context
                    .clone()
                    .push_var_type(effective_var.clone(), typ.clone(), &alias_context);

            let mut errors = Vec::new();
            let generics_in_type = typ.extract_generics();
            let params_set: HashSet<String> = params
                .iter()
                .filter_map(|p| {
                    if let Type::Generic(name, _) = p {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
                .collect();
            let mut missing_generics: Vec<String> = Vec::new();

            for generic in &generics_in_type {
                if let Type::Generic(name, _) = generic {
                    if !params_set.contains(name) {
                        missing_generics.push(name.clone());
                    }
                }
            }

            if !missing_generics.is_empty() {
                errors.push(TypRError::Type(TypeError::AliasMissingGenerics(
                    var.get_name().to_string(),
                    missing_generics,
                    typ.clone(),
                )));
            }

            let ctx_with_alias = new_context.push_alias(effective_var.get_name(), typ.to_owned());

            // For union aliases, register variant types in the subtype graph so that
            // get_classes() returns the correct hierarchy (e.g. Tag("Red") → Alias("Color"))
            let final_context = if let Type::Operator(TypeOperator::Union, _, _, _) = typ {
                let alias_type = Type::Alias(effective_var.get_name(), vec![], false, h.clone());
                let members = flatten_operator_union(typ);
                let new_subtypes = members
                    .iter()
                    .fold(ctx_with_alias.subtypes.clone(), |graph, member| {
                        graph.cache_subtype(member.clone(), alias_type.clone(), true)
                    });
                ctx_with_alias.with_subtypes(new_subtypes)
            } else {
                ctx_with_alias
            };

            TypeContext::new(
                builder::unknown_function_type(),
                expr.clone(),
                final_context,
            )
            .with_errors(errors)
        }
        Lang::Assign {
            identifier: left_expr,
            expression: right_expr,
            help_data: h,
            ..
        } => {
            let left_tc = typing(context, left_expr);
            let right_tc = typing(context, right_expr);
            let mut errors = left_tc.errors.clone();
            errors.extend(right_tc.errors.clone());

            let left_type = left_tc.value;
            let right_type = right_tc.value;
            let reduced_left_type = reduce_type(context, &left_type);
            let reduced_right_type = reduce_type(context, &right_type);

            if reduced_right_type.is_subtype(&reduced_left_type, context).0 {
                let Some(var) = Var::from_language((**left_expr).clone())
                    .map(|v| v.set_type(right_type.clone()))
                else {
                    return TypeContext::new(right_type, expr.clone(), context.clone())
                        .with_errors(errors);
                };
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
        Lang::Library {
            value: name,
            help_data: _h,
        } => {
            // In WASM mode, library imports are no-op (types must be pre-registered)
            // In native mode, this would install and load the package
            install_package(name);

            // For now, just return unknown function type
            // The package types should be pre-loaded in the context for WASM
            (
                builder::unknown_function_type(),
                expr.clone(),
                context.clone(),
            )
                .into()
        }
        Lang::Signature {
            identifier: var,
            target_type: typ,
            ..
        } => signature_expression(context, expr, var, typ),
        Lang::TestBlock { value: body, .. } => {
            //Needed to be type checked
            let tc = typing(context, body);
            TypeContext::new(
                builder::unknown_function_type(),
                expr.clone(),
                context.clone(),
            )
            .with_errors(tc.errors)
        }
        Lang::Module {
            name: module_name,
            body: members,
            help_data: h,
            ..
        } => {
            let module_expr = if members.len() > 1 {
                Lang::Lines {
                    value: members.to_vec(),
                    help_data: h.clone(),
                }
            } else if let Some(first) = members.iter().next() {
                first.clone()
            } else {
                return TypeContext::new(builder::empty_type(), expr.clone(), context.clone());
            };
            // Type-check the module body with an internal context where opaque
            // aliases are transparent — module-internal functions see through them.
            let internal_context = Context::default().set_in_module_body();
            let typing_context = typing(&internal_context, &module_expr);

            // Collect opaque alias names declared in this module so we can
            // re-opacify them in exported function signatures.
            let opaque_names: std::collections::HashSet<String> = members
                .iter()
                .filter_map(|m| match m {
                    Lang::Alias { identifier: id, .. } => Var::try_from(id)
                        .ok()
                        .filter(|v| v.get_opacity())
                        .map(|v| v.get_name()),
                    _ => None,
                })
                .collect();

            // Re-opacify any alias in `ty` whose name is in `opaque_names`.
            fn reify_opaque(ty: Type, opaque_names: &std::collections::HashSet<String>) -> Type {
                match ty {
                    Type::Alias(name, params, _, h) if opaque_names.contains(&name) => {
                        Type::Alias(name, params, true, h)
                    }
                    Type::Function(args, ret, h) => Type::Function(
                        args.into_iter()
                            .map(|a| {
                                ArgumentType::new(
                                    &a.get_argument_str(),
                                    &reify_opaque(a.get_type(), opaque_names),
                                )
                            })
                            .collect(),
                        Box::new(reify_opaque(*ret, opaque_names)),
                        h,
                    ),
                    other => other,
                }
            }

            // Build public member types for the module's Type::Module.
            // For opaque aliases, export the opaque alias type (not the underlying type)
            // so callers outside the module cannot see through it.
            let pub_arg_types: Vec<ArgumentType> = members
                .iter()
                .filter_map(|member| match member {
                    Lang::Let {
                        variable: var,
                        is_public: true,
                        ..
                    } => Var::try_from(var).ok().map(|v| {
                        let var_type = typing_context
                            .context
                            .get_type_from_variable(&v)
                            .unwrap_or_else(|_| builder::empty_type());
                        let reified = reify_opaque(var_type, &opaque_names);
                        let exported = reduce_type(&typing_context.context, &reified);
                        ArgumentType::new(&v.get_name(), &exported)
                    }),
                    Lang::Alias {
                        identifier: exp,
                        target_type: typ,
                        is_public: true,
                        ..
                    } => Var::try_from(exp).ok().map(|v| {
                        let exported_type = if v.get_opacity() {
                            v.clone().to_alias_type()
                        } else {
                            typ.clone()
                        };
                        ArgumentType::new(&v.get_name(), &exported_type)
                    }),
                    _ => None,
                })
                .collect();

            // Register only the module itself — members must be imported explicitly via `use`
            let module_type = Type::Module(pub_arg_types, h.clone());
            let module_var = Var::from_name(module_name);
            let final_context = context
                .clone()
                .push_var_type(module_var, module_type, context);

            TypeContext::new(builder::empty_type(), expr.clone(), final_context)
                .with_errors(typing_context.errors)
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
        (Type::Integer(i, _), Type::Integer(j, _)) => (j.gen_of(i) || i == j).then_some(vec![]),
        (Type::Char(c, _), Type::Char(d, _)) => (d.gen_of(c) || d == c).then_some(vec![]),
        (Type::Alias(name1, params1, _, _), Type::Alias(name2, params2, _, _)) => {
            if name1 == name2 && params1.len() == params2.len() {
                let res = params1
                    .iter()
                    .zip(params2.iter())
                    .flat_map(|(p1, p2)| get_gen_type(p1, p2))
                    .flatten()
                    .collect::<Vec<_>>();
                Some(res)
            } else {
                None
            }
        }
        (Type::Alias(_, params1, _, _), Type::Generic(_, _)) => {
            let res = params1.iter().map(|p| (p.clone(), type2.clone())).collect();
            Some(res)
        }
        (Type::Tag(_, _, _), Type::Generic(_, _)) => Some(vec![(type1.clone(), type2.clone())]),
        (_, Type::Generic(_, _))
        | (_, Type::IndexGen(_, _))
        | (_, Type::LabelGen(_, _))
        | (_, Type::Interface(_, _)) => Some(vec![(type1.clone(), type2.clone())]),
        (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
            let args1_types: Vec<Type> = args1.iter().map(|arg| arg.get_type()).collect();
            let args2_types: Vec<Type> = args2.iter().map(|arg| arg.get_type()).collect();
            let ret1 = (**ret_typ1).clone();
            let ret2 = (**ret_typ2).clone();
            let mut res = Vec::new();
            for (t1, t2) in args1_types.iter().zip(args2_types.iter()) {
                if let Some(gen) = get_gen_type(t1, t2) {
                    res.extend(gen);
                }
            }
            if let Some(gen) = get_gen_type(&ret1, &ret2) {
                res.extend(gen);
            }
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
        (Type::Tag(name1, inner1, _), Type::Tag(name2, inner2, _)) if name1 == name2 => {
            if matches!(
                inner2.as_ref(),
                Type::Generic(_, _) | Type::IndexGen(_, _) | Type::LabelGen(_, _)
            ) {
                let inner_type: Type = (**inner2).clone();
                Some(vec![(type1.clone(), inner_type)])
            } else {
                get_gen_type(inner1, inner2)
            }
        }
        (Type::Tag(tag_name, inner, _), Type::Alias(alias_name, params, _, _)) => {
            if params.len() == 1 {
                get_gen_type(inner, &params[0])
            } else {
                None
            }
        }
        (Type::Tag(tag_name, inner, _), Type::Operator(TypeOperator::Union, t1, t2, _)) => {
            let t1_inner: &Type = t1.as_ref();
            let t2_inner: &Type = t2.as_ref();
            match (t1_inner, t2_inner) {
                (Type::Tag(name1, _, _), _) if name1 == tag_name => {
                    let t1_val: Type = (**t1).clone();
                    Some(vec![(type1.clone(), t1_val)])
                }
                (_, Type::Tag(name2, _, _)) if name2 == tag_name => {
                    let t2_val: Type = (**t2).clone();
                    Some(vec![(type1.clone(), t2_val)])
                }
                _ => None,
            }
        }
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
            .flat_map(|(arg, par)| unification::unify(ctx, arg, par))
            .collect::<Vec<_>>()
    })
}

fn are_homogenous_types(types: &[Type]) -> bool {
    types
        .windows(2)
        .all(|w| w[0].to_category() == w[1].to_category())
}

trait WithLang2 {
    fn with_lang(self, expr: &Lang, context: &Context) -> (Type, Lang, Context);
}

impl WithLang2 for Type {
    fn with_lang(self, expr: &Lang, context: &Context) -> (Type, Lang, Context) {
        (self, expr.clone(), context.clone())
    }
}

/// Type-check a homogeneous container literal (Array, Vector, Sequence).
///
/// - `type_prefix`: the string prefix for the type representation ("", "Vec", "Seq")
/// - `empty_elem_type`: the element type used when the container is empty ("Empty" or "Any")
/// - `reduce`: whether to alias-reduce element types before checking homogeneity
/// - `generalize`: whether to generalize the element type when all elements share a type
fn typing_container(
    context: &Context,
    expr: &Lang,
    exprs: &[Lang],
    h: &HelpData,
    type_prefix: &str,
    empty_elem_type: &str,
    reduce: bool,
    generalize: bool,
) -> TypeContext {
    let type_contexts: Vec<_> = exprs.iter().map(|e| typing(context, e)).collect();
    let mut errors: Vec<TypRError> = type_contexts
        .iter()
        .flat_map(|tc| tc.errors.clone())
        .collect();
    let types: Vec<_> = type_contexts
        .iter()
        .map(|tc| {
            if reduce {
                tc.value.clone().reduce(context)
            } else {
                tc.value.clone()
            }
        })
        .collect();

    let new_type = if exprs.is_empty() {
        format!("{}[0, {}]", type_prefix, empty_elem_type)
            .parse::<Type>()
            .unwrap()
            .set_help_data(h.clone())
    } else if are_homogenous_types(&types) {
        let elem_str = if generalize {
            types[0].clone().generalize().pretty()
        } else {
            types[0].clone().pretty()
        };
        format!("{}[{}, {}]", type_prefix, exprs.len(), elem_str)
            .parse::<Type>()
            .unwrap()
            .set_help_data(h.clone())
    } else {
        errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
        format!("{}[{}, Any]", type_prefix, exprs.len())
            .parse::<Type>()
            .unwrap()
            .set_help_data(h.clone())
    };

    TypeContext::new(
        new_type.clone(),
        expr.clone(),
        context.clone().push_types(&[new_type]),
    )
    .with_errors(errors)
}

//main
pub fn typing(context: &Context, expr: &Lang) -> TypeContext {
    match expr {
        Lang::Number { help_data: h, .. } => {
            (Type::Number(crate::components::r#type::tnumber::Tnum::Unknown, h.clone()), expr.clone(), context.clone()).into()
        }
        Lang::Integer {
            value: i,
            help_data: h,
        } => (
            builder::integer_type(*i).set_help_data(h.clone()),
            expr.clone(),
            context.clone(),
        )
            .into(),
        Lang::Bool { help_data: h, .. } => {
            (Type::Boolean(crate::components::r#type::tbool::Tbool::Unknown, h.clone()), expr.clone(), context.clone()).into()
        }
        Lang::Char {
            value: s,
            help_data: h,
        } => (
            builder::character_type(s).set_help_data(h.clone()),
            expr.clone(),
            context.clone(),
        )
            .into(),
        Lang::Null(h) => (Type::Null(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Empty(h) => (Type::Empty(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Break(h) => (Type::Empty(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Loop {
            body, help_data: h, ..
        } => {
            let tc = typing(context, body);
            let errors = tc.errors.clone();
            TypeContext::new(Type::Empty(h.clone()), expr.clone(), context.clone())
                .with_errors(errors)
                .into()
        }
        Lang::Operator {
            operator: Op::And(_),
            rhs: e1,
            lhs: e2,
            ..
        }
        | Lang::Operator {
            operator: Op::Or(_),
            rhs: e1,
            lhs: e2,
            ..
        } => {
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
        Lang::Operator {
            operator: Op::Eq(_),
            rhs: e1,
            lhs: e2,
            ..
        }
        | Lang::Operator {
            operator: Op::LesserOrEqual(_),
            rhs: e1,
            lhs: e2,
            ..
        }
        | Lang::Operator {
            operator: Op::GreaterOrEqual(_),
            rhs: e1,
            lhs: e2,
            ..
        }
        | Lang::Operator {
            operator: Op::GreaterThan(_),
            rhs: e1,
            lhs: e2,
            ..
        }
        | Lang::Operator {
            operator: Op::LesserThan(_),
            rhs: e1,
            lhs: e2,
            ..
        } => {
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
        Lang::Operator {
            operator: Op::Dot(_),
            rhs: e1,
            lhs: e2,
            ..
        }
        | Lang::Operator {
            operator: Op::Pipe(_),
            rhs: e1,
            lhs: e2,
            ..
        } => {
            if let Lang::FunctionApp {
                identifier: exp,
                arguments: v,
                help_data: h,
            } = (**e2).clone()
            {
                let fun_app = Lang::FunctionApp {
                    identifier: exp,
                    arguments: [(**e1).clone()]
                        .iter()
                        .chain(v.iter())
                        .cloned()
                        .collect::<Vec<_>>(),
                    help_data: h.clone(),
                };
                typing(context, &fun_app)
            } else {
                let tc2 = typing(context, e2);
                let mut errors = tc2.errors.clone();
                let ty2 = tc2.value.clone().reduce(context);
                match (ty2.clone(), *e1.clone()) {
                    (
                        Type::Record(fields, _),
                        Lang::Variable {
                            name, help_data: h, ..
                        },
                    ) => {
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
                    (
                        Type::Record(fields, _),
                        Lang::Char {
                            value: name,
                            help_data: h,
                        },
                    ) => {
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
                    (
                        Type::Tuple(vals, _),
                        Lang::Integer {
                            value: i,
                            help_data: h,
                        },
                    ) => match vals.get((i - 1) as usize) {
                        Some(typ) => TypeContext::new(typ.clone(), expr.clone(), context.clone())
                            .with_errors(errors),
                        None => {
                            errors.push(TypRError::Type(TypeError::WrongExpression(h)));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    },
                    (Type::Record(fields1, h), Lang::List { .. }) => {
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
                    (Type::Generic(_, _), Lang::List { .. }) => {
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
        Lang::Operator {
            operator: Op::Dollar(hd),
            rhs: e1,
            lhs: e2,
            ..
        } => {
            let op = match expr {
                Lang::Operator { operator: op, .. } => op.clone(),
                _ => Op::Dollar(HelpData::default()),
            };
            let tc1 = typing(context, e1);
            let mut errors = tc1.errors.clone();
            let ty1 = tc1.value.clone();
            match (ty1.reduce(context), *e2.clone(), &op) {
                (
                    Type::Record(fields, _),
                    Lang::Variable {
                        name, help_data: h, ..
                    },
                    _,
                ) => {
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
                (
                    Type::Module(fields, _),
                    Lang::Variable {
                        name, help_data: h, ..
                    },
                    _,
                ) => {
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
                                Lang::Variable {
                                    name,
                                    is_opaque: false,
                                    related_type: builder::any_type(),
                                    help_data: h.clone(),
                                },
                            )));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                    }
                }
                (
                    Type::Module(fields, _),
                    Lang::FunctionApp {
                        identifier: exp, ..
                    },
                    _,
                ) => match Var::from_language(*exp.clone()) {
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
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
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
                },
                (
                    Type::Record(fields, _),
                    Lang::Char {
                        value: name,
                        help_data: h,
                    },
                    _,
                ) => {
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
                (
                    Type::Tuple(vals, _),
                    Lang::Integer {
                        value: i,
                        help_data: h,
                    },
                    _,
                ) => match vals.get((i - 1) as usize) {
                    Some(typ) => TypeContext::new(typ.clone(), expr.clone(), context.clone())
                        .with_errors(errors),
                    None => {
                        errors.push(TypRError::Type(TypeError::WrongExpression(h)));
                        TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                            .with_errors(errors)
                    }
                },
                (Type::Record(fields1, h), Lang::List { value: fields2, .. }, _) => {
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
                (
                    Type::Record(fields, _),
                    Lang::FunctionApp {
                        identifier: exp,
                        arguments: params,
                        ..
                    },
                    _,
                ) => match Var::from_language(*exp.clone()) {
                    Some(var) => {
                        match fields
                            .iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                        {
                            Some(arg_typ) => {
                                let typ = arg_typ.get_type();
                                let tc =
                                    typing(&context.clone().push_var_type(var, typ, context), e2);
                                errors.extend(tc.errors.clone());
                                TypeContext::new(tc.value, expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::FunctionNotFound(
                                    var.set_type_from_params(&params, context),
                                )));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
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
                },
                (Type::UnknownFunction(h), Lang::FunctionApp { .. }, _) => {
                    TypeContext::new(Type::UnknownFunction(h), (*expr).clone(), context.clone())
                        .with_errors(errors)
                }
                // DataFrame $ access: df$col -> [N, col_type] (VecType::Vector)
                (
                    Type::Vec(VecType::DataFrame, n, ref body, h),
                    Lang::Variable {
                        name,
                        help_data: ref vh,
                        ..
                    },
                    _,
                ) => match body.as_ref() {
                    Type::Record(fields, _) => {
                        match fields
                            .iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        {
                            Some(arg_typ) => {
                                let inner_type = match arg_typ.1.clone() {
                                    Type::Vec(_, _, inner, _) => *inner,
                                    other => other,
                                };
                                TypeContext::new(
                                    Type::Vec(
                                        VecType::Vector,
                                        n.clone(),
                                        Box::new(inner_type),
                                        h.clone(),
                                    ),
                                    expr.clone(),
                                    context.clone(),
                                )
                                .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::FieldNotFound(
                                    (name, vh.clone()),
                                    ty1,
                                )));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                        }
                    }
                    _ => {
                        errors.push(TypRError::Type(TypeError::WrongExpression(
                            expr.get_help_data(),
                        )));
                        TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                            .with_errors(errors)
                    }
                },
                // DataFrame $ access with string literal: df$"col"
                (
                    Type::Vec(VecType::DataFrame, n, ref body, h),
                    Lang::Char {
                        value: name,
                        help_data: ref vh,
                    },
                    _,
                ) => match body.as_ref() {
                    Type::Record(fields, _) => {
                        match fields
                            .iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        {
                            Some(arg_typ) => {
                                let inner_type = match arg_typ.1.clone() {
                                    Type::Vec(_, _, inner, _) => *inner,
                                    other => other,
                                };
                                TypeContext::new(
                                    Type::Vec(VecType::Vector, n, Box::new(inner_type), h.clone()),
                                    expr.clone(),
                                    context.clone(),
                                )
                                .with_errors(errors)
                            }
                            None => {
                                errors.push(TypRError::Type(TypeError::FieldNotFound(
                                    (name, vh.clone()),
                                    ty1,
                                )));
                                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                                    .with_errors(errors)
                            }
                        }
                    }
                    _ => {
                        errors.push(TypRError::Type(TypeError::WrongExpression(
                            expr.get_help_data(),
                        )));
                        TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                            .with_errors(errors)
                    }
                },
                // DataFrame $ update with list: df$:{col: new_type}
                (
                    Type::Vec(VecType::DataFrame, n, ref body, h),
                    Lang::List { value: fields2, .. },
                    _,
                ) => match body.as_ref() {
                    Type::Record(fields1, rh) => {
                        let at = fields2[0].clone();
                        let fields3 = fields1
                            .iter()
                            .map(replace_fields_type_if_needed(context, at))
                            .collect::<HashSet<_>>();
                        TypeContext::new(
                            Type::Vec(
                                VecType::DataFrame,
                                n,
                                Box::new(Type::Record(fields3, rh.clone())),
                                h.clone(),
                            ),
                            expr.clone(),
                            context.clone(),
                        )
                        .with_errors(errors)
                    }
                    _ => {
                        errors.push(TypRError::Type(TypeError::WrongExpression(
                            expr.get_help_data(),
                        )));
                        TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                            .with_errors(errors)
                    }
                },
                // DataFrame $ function call: df$fn(args)
                (
                    Type::Vec(VecType::DataFrame, _n, ref body, _h),
                    Lang::FunctionApp {
                        identifier: exp,
                        arguments: params,
                        ..
                    },
                    _,
                ) => match body.as_ref() {
                    Type::Record(fields, _) => match Var::from_language(*exp.clone()) {
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
                    },
                    _ => {
                        errors.push(TypRError::Type(TypeError::WrongExpression(
                            expr.get_help_data(),
                        )));
                        TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                            .with_errors(errors)
                    }
                },
                (Type::Vec(vtype, n, _, h), Lang::Variable { .. }, _) => {
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
                (
                    _,
                    Lang::FunctionApp {
                        identifier: exp,
                        arguments: args,
                        help_data: h2,
                    },
                    Op::Dot(_),
                ) => {
                    let tc = typing(
                        context,
                        &Lang::FunctionApp {
                            identifier: exp,
                            arguments: [(**e1).clone()]
                                .iter()
                                .chain(args.iter())
                                .cloned()
                                .collect(),
                            help_data: h2,
                        },
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
        Lang::Operator {
            operator: op,
            rhs: e1,
            lhs: e2,
            help_data: h,
        } => {
            let var_exp = Var::from_name(&format!("`{}`", op))
                .set_help_data(e1.get_help_data())
                .to_language();
            let fun_app = Lang::FunctionApp {
                identifier: Box::new(var_exp),
                arguments: vec![(**e1).clone(), (**e2).clone()],
                help_data: h.clone(),
            };
            typing(context, &fun_app)
        }
        Lang::Function {
            parameters: params,
            return_type: ret_ty,
            body,
            help_data: h,
        } => function(context, expr, params, ret_ty, body, h),
        Lang::Lines {
            value: exprs,
            help_data: _h,
        } => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                let tc = typing(context, &res);
                let (typ, langs, errors) = (tc.value, tc.lang, tc.errors);
                TypeContext::new(
                    typ.clone(),
                    langs.clone(),
                    context
                        .clone()
                        .push_var_type(Var::from("_out"), typ.clone(), context),
                )
                .with_errors(errors)
            } else if exprs.is_empty() {
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
        Lang::FunctionApp {
            identifier: fn_var_name,
            arguments: values,
            help_data: h,
        } => function_application(context, fn_var_name, values, h),
        Lang::Tag {
            name,
            value: tag_expr,
            help_data: h,
        } => {
            let tc = typing(context, tag_expr);
            TypeContext::new(
                Type::Tag(name.clone(), Box::new(tc.value.clone()), h.clone()),
                expr.clone(),
                tc.context,
            )
            .with_errors(tc.errors)
        }
        Lang::If {
            condition: cond,
            if_block: true_branch,
            else_block: false_branch,
            help_data: _h,
        } => {
            let cond_tc = typing(context, cond);
            let mut errors = cond_tc.errors.clone();

            if cond_tc.value.is_boolean() {
                let true_tc = typing(context, true_branch);
                let false_tc = typing(context, false_branch);
                errors.extend(true_tc.errors);
                errors.extend(false_tc.errors);

                let result_type = if true_tc.value == false_tc.value || false_tc.value.is_empty() {
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
        Lang::Array {
            value: exprs,
            help_data: h,
        } => typing_container(context, expr, exprs, h, "", "Empty", true, true),
        Lang::Vector {
            value: exprs,
            help_data: h,
        } => typing_container(context, expr, exprs, h, "Vec", "Any", false, true),
        Lang::Sequence {
            body: exprs,
            help_data: h,
        } => typing_container(context, expr, exprs, h, "Seq", "Empty", false, false),
        Lang::List {
            value: fields,
            help_data: h,
        } => {
            let type_contexts: Vec<_> = fields
                .iter()
                .map(|arg_val: &ArgumentValue| typing(context, &arg_val.get_value()))
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
        Lang::DataFrame {
            value: fields,
            help_data: h,
        } => {
            let type_contexts: Vec<_> = fields
                .iter()
                .map(|arg_val: &ArgumentValue| typing(context, &arg_val.get_value()))
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
            let index = fields
                .first()
                .map(|arg_val: &ArgumentValue| {
                    let col_type = typing(context, &arg_val.get_value()).value;
                    match col_type {
                        Type::Vec(_, idx, _, _) => *idx,
                        _ => builder::any_type(),
                    }
                })
                .unwrap_or_else(builder::any_type);
            TypeContext::new(
                Type::Vec(
                    VecType::DataFrame,
                    Box::new(index),
                    Box::new(Type::Record(field_types, h.clone())),
                    h.clone(),
                ),
                expr.clone(),
                context.clone(),
            )
            .with_errors(errors)
        }
        Lang::Match {
            target: match_exp,
            branches,
            ..
        } => {
            let match_tc = typing(context, match_exp);
            let mut errors = match_tc.errors.clone();
            let match_type = match_tc.value.clone();

            let branch_tcs: Vec<_> = branches
                .iter()
                .map(|(pattern, bexp)| {
                    // For tag patterns with bindings, we extract the inner type
                    let new_context = match pattern {
                        Lang::Tag {
                            name: tag_name,
                            value: inner,
                            ..
                        } => match inner.as_ref() {
                            Lang::Variable { name: var_name, .. } => {
                                let inner_type = match &match_type {
                                    Type::Tag(tn, inner_t, _h) if tn == tag_name => {
                                        (**inner_t).clone()
                                    }
                                    Type::Alias(name, generics, _, _h) => {
                                        if name == "Option"
                                            && (tag_name == "Some" || tag_name == "None")
                                        {
                                            generics
                                                .first()
                                                .cloned()
                                                .unwrap_or_else(builder::any_type)
                                        } else {
                                            builder::any_type()
                                        }
                                    }
                                    _ => builder::any_type(),
                                };
                                let var = Var::from_name(var_name);
                                context.clone().push_var_type(var, inner_type, context)
                            }
                            _ => context.clone(),
                        },
                        // For type patterns `x as int`, bind variable to that type
                        Lang::TypePattern {
                            variable_name: var_name,
                            matched_type: typ,
                            ..
                        } => {
                            let var = Var::from_name(var_name);
                            context.clone().push_var_type(var, typ.clone(), context)
                        }
                        // For tuple patterns `:{a, b, c}`, bind each variable by position
                        Lang::Tuple {
                            value: elements, ..
                        } => {
                            let tuple_types = match &match_type {
                                Type::Tuple(types, _) => Some(types.clone()),
                                _ => None,
                            };
                            elements.iter().enumerate().fold(
                                context.clone(),
                                |ctx: Context, (i, elem)| {
                                    if let Lang::Variable { name: var_name, .. } = elem {
                                        if var_name == "_" {
                                            return ctx;
                                        }
                                        let elem_type = tuple_types
                                            .as_ref()
                                            .and_then(|types| types.get(i).cloned())
                                            .unwrap_or_else(builder::any_type);
                                        let var = Var::from_name(&var_name);
                                        ctx.push_var_type(var, elem_type, context)
                                    } else {
                                        ctx
                                    }
                                },
                            )
                        }
                        // For list/record patterns `:{nom: n, age: a}`, bind each variable
                        Lang::List { value: fields, .. } => {
                            // Extract record field types from the matched expression if available
                            let record_fields = match &match_type {
                                Type::Record(field_types, _) => Some(field_types.clone()),
                                _ => None,
                            };
                            fields.iter().fold(
                                context.clone(),
                                |ctx: Context, arg_val: &ArgumentValue| {
                                    if let Lang::Variable { name: var_name, .. } =
                                        &arg_val.get_value()
                                    {
                                        let field_type = record_fields
                                            .as_ref()
                                            .and_then(|fields| {
                                                fields.iter().find(|at| {
                                                    at.get_argument_str() == arg_val.get_argument()
                                                })
                                            })
                                            .map(|at| at.get_type())
                                            .unwrap_or_else(builder::any_type);
                                        let var = Var::from_name(&var_name);
                                        ctx.push_var_type(var, field_type, context)
                                    } else {
                                        ctx
                                    }
                                },
                            )
                        }
                        _ => context.clone(),
                    };
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
        Lang::ArrayIndexing {
            identifier: arr_exp,
            indexing: index,
            help_data: h,
        } => {
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
        Lang::Variable { .. } => {
            let old_var = Var::try_from(expr.clone()).unwrap();
            let var = context.get_true_variable(&old_var);
            context
                .get_type_from_existing_variable(var)
                .with_lang(expr, context)
                .into()
        }
        Lang::Scope { body: expr, .. } if expr.len() == 1 => typing(context, &expr[0]),
        Lang::Scope {
            body: expr,
            help_data: h,
        } => typing(
            context,
            &Lang::Lines {
                value: expr.to_vec(),
                help_data: h.clone(),
            },
        ),
        Lang::Tuple {
            value: elements,
            help_data: h,
        } => {
            let tcs: Vec<_> = elements.iter().map(|x| typing(context, x)).collect();
            let errors: Vec<TypRError> = tcs.iter().flat_map(|tc| tc.errors.clone()).collect();
            let types: Vec<_> = tcs.iter().map(|tc| tc.value.clone()).collect();
            TypeContext::new(Type::Tuple(types, h.clone()), expr.clone(), context.clone())
                .with_errors(errors)
        }
        Lang::VecBlock { help_data: h, .. } => {
            TypeContext::new(Type::Empty(h.clone()), expr.clone(), context.clone())
        }
        Lang::RFunction { help_data: h, .. } => TypeContext::new(
            Type::UnknownFunction(h.clone()),
            expr.clone(),
            context.clone(),
        ),
        Lang::ForLoop {
            identifier: var,
            expression: iter,
            body,
            help_data: h,
        } => {
            let iter_tc = typing(context, iter);
            let mut errors = iter_tc.errors.clone();

            match iter_tc.value.to_array() {
                Some(arr) => {
                    let base_type = arr.base_type;
                    let var = var.clone().set_type(base_type.clone());
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
        Lang::Not {
            value: not_exp,
            help_data: h,
        } => {
            let tc = typing(context, not_exp);
            let mut errors = tc.errors.clone();

            match tc.value {
                Type::Boolean(_, _) => {
                    TypeContext::new(Type::Boolean(crate::components::r#type::tbool::Tbool::Unknown, h.clone()), expr.clone(), context.clone())
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
        Lang::Let { .. } => eval(context, expr),
        Lang::Assign { .. } => eval(context, expr),
        Lang::Alias { .. } => eval(context, expr).with_lang(expr),
        Lang::Library { .. } => eval(context, expr).with_lang(expr),
        Lang::TestBlock { .. } => eval(context, expr).with_lang(expr),
        Lang::Signature { .. } => eval(context, expr).with_lang(expr),
        Lang::Return { value: exp, .. } => typing(context, exp),
        Lang::Module { .. } => eval(context, expr).with_lang(expr),
        Lang::Lambda {
            parameters: params,
            body,
            help_data: h,
        } => {
            let fresh_param_types: Vec<Type> = params
                .iter()
                .enumerate()
                .map(|(i, _)| Type::Generic(format!("T{}", i), HelpData::default()))
                .collect();
            let sub_context = params.iter().zip(fresh_param_types.iter()).fold(
                context.clone(),
                |ctx: Context, (param, typ): (&Lang, &Type)| match Var::from_language(param.clone())
                {
                    Some(var) => ctx.clone().push_var_type(var, typ.clone(), &ctx),
                    None => ctx,
                },
            );
            let body_tc = typing(&sub_context, body);
            let fresh_arg_types: Vec<ArgumentType> = fresh_param_types
                .iter()
                .enumerate()
                .map(|(i, typ)| {
                    let arg_name = crate::components::r#type::generate_arg(i);
                    ArgumentType::new(&arg_name, typ)
                })
                .collect();
            let func_type =
                Type::Function(fresh_arg_types, Box::new(body_tc.value.clone()), h.clone());
            // Don't propagate errors from the initial lambda typing:
            // the body will be re-typed after specialization in apply_from_variable
            // with concrete types substituted for the fresh type variables.
            TypeContext::new(func_type, expr.clone(), context.clone())
        }
        Lang::Dots(h) => Type::Any(h.clone()).with_lang(expr, context).into(),
        Lang::UseModule {
            module_path,
            selector,
            help_data,
        } => {
            use crate::components::language::use_lang::UseSelector;
            let mut errors = Vec::new();
            let mut new_context = context.clone();

            if module_path.is_empty() {
                return TypeContext::new(builder::empty_type(), expr.clone(), context.clone());
            }

            // Resolve first segment of path in context
            let module_var = Var::from_name(&module_path[0]);
            let root_type = match context.get_type_from_variable(&module_var) {
                Ok(t) => t,
                Err(_) => {
                    errors.push(TypRError::Type(TypeError::UndefinedVariable(
                        Lang::Variable {
                            name: module_path[0].clone(),
                            is_opaque: false,
                            related_type: builder::any_type(),
                            help_data: help_data.clone(),
                        },
                    )));
                    return TypeContext::new(builder::empty_type(), expr.clone(), context.clone())
                        .with_errors(errors);
                }
            };

            // Navigate nested path segments
            let mut current_type = root_type;
            for segment in module_path.iter().skip(1) {
                match current_type.clone().to_module_type() {
                    Ok(mt) => match mt.get_type_from_name(segment) {
                        Ok(t) => current_type = t,
                        Err(_) => {
                            errors.push(TypRError::Type(TypeError::UndefinedVariable(
                                Lang::Variable {
                                    name: segment.clone(),
                                    is_opaque: false,
                                    related_type: builder::any_type(),
                                    help_data: help_data.clone(),
                                },
                            )));
                            return TypeContext::new(
                                builder::empty_type(),
                                expr.clone(),
                                context.clone(),
                            )
                            .with_errors(errors);
                        }
                    },
                    Err(_) => {
                        errors.push(TypRError::Type(TypeError::UndefinedVariable(
                            Lang::Variable {
                                name: segment.clone(),
                                is_opaque: false,
                                related_type: builder::any_type(),
                                help_data: help_data.clone(),
                            },
                        )));
                        return TypeContext::new(
                            builder::empty_type(),
                            expr.clone(),
                            context.clone(),
                        )
                        .with_errors(errors);
                    }
                }
            }

            let mod_type = match current_type.to_module_type() {
                Ok(mt) => mt,
                Err(_) => {
                    errors.push(TypRError::Type(TypeError::UndefinedVariable(
                        Lang::Variable {
                            name: module_path.last().cloned().unwrap_or_default(),
                            is_opaque: false,
                            related_type: builder::any_type(),
                            help_data: help_data.clone(),
                        },
                    )));
                    return TypeContext::new(builder::empty_type(), expr.clone(), context.clone())
                        .with_errors(errors);
                }
            };

            // Track names imported in this directive to detect intra-directive conflicts
            let mut imported_names: std::collections::HashSet<String> =
                std::collections::HashSet::new();

            match selector {
                UseSelector::Wildcard => {
                    for member in mod_type.get_public_members() {
                        let local_name = member.get_argument_str();
                        // Conflict with local declaration
                        if context
                            .get_type_from_variable(&Var::from_name(&local_name))
                            .is_ok()
                        {
                            errors.push(TypRError::Type(TypeError::ImmutableVariable(
                                Var::from_name(&local_name),
                                Var::from_name(&local_name),
                            )));
                        } else {
                            new_context = new_context.clone().push_var_type(
                                Var::from_name(&local_name),
                                member.get_type(),
                                &new_context,
                            );
                        }
                    }
                }
                UseSelector::Items(items) => {
                    use crate::components::language::use_lang::UseItem;
                    for item in items {
                        let local_name = item.alias.as_ref().unwrap_or(&item.name).clone();

                        // Conflict with another import in this directive
                        if imported_names.contains(&local_name) {
                            errors.push(TypRError::Type(TypeError::ImmutableVariable(
                                Var::from_name(&local_name),
                                Var::from_name(&local_name),
                            )));
                            continue;
                        }

                        // Conflict with existing local
                        if context
                            .get_type_from_variable(&Var::from_name(&local_name))
                            .is_ok()
                        {
                            errors.push(TypRError::Type(TypeError::ImmutableVariable(
                                Var::from_name(&local_name),
                                Var::from_name(&local_name),
                            )));
                            continue;
                        }

                        match mod_type.get_type_from_name(&item.name) {
                            Ok(member_type) => {
                                imported_names.insert(local_name.clone());
                                new_context = new_context.clone().push_var_type(
                                    Var::from_name(&local_name),
                                    member_type,
                                    &new_context,
                                );
                            }
                            Err(_) => {
                                errors.push(TypRError::Type(TypeError::UndefinedVariable(
                                    Lang::Variable {
                                        name: item.name.clone(),
                                        is_opaque: false,
                                        related_type: builder::any_type(),
                                        help_data: help_data.clone(),
                                    },
                                )));
                            }
                        }
                    }
                }
            }

            TypeContext::new(builder::empty_type(), expr.clone(), new_context).with_errors(errors)
        }
        Lang::ConstructorCall {
            type_name,
            fields: _,
            help_data: h,
        } => {
            let alias_exists = context
                .aliases()
                .any(|(var, _)| var.get_name() == *type_name);
            if alias_exists {
                TypeContext::new(
                    Type::Alias(type_name.clone(), vec![], false, h.clone()),
                    expr.clone(),
                    context.clone(),
                )
            } else {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
            }
        }
        Lang::ArrayConstructorCall {
            type_name,
            help_data: h,
            ..
        } => {
            let alias_exists = context
                .aliases()
                .any(|(var, _)| var.get_name() == *type_name);
            if alias_exists {
                TypeContext::new(
                    Type::Alias(type_name.clone(), vec![], false, h.clone()),
                    expr.clone(),
                    context.clone(),
                )
            } else {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
            }
        }
        Lang::UnionConstructor {
            union_name,
            help_data: h,
            ..
        } => TypeContext::new(
            Type::Alias(union_name.clone(), vec![], false, h.clone()),
            expr.clone(),
            context.clone(),
        ),
        Lang::ValidatingCast {
            expression,
            type_name,
            help_data: h,
        } => {
            let expr_tc = typing(context, expression);
            let alias_type = Type::Alias(type_name.clone(), vec![], false, h.clone());
            TypeContext::new(alias_type, expr.clone(), context.clone())
                .with_errors(expr_tc.errors)
        }
        Lang::Comment { help_data: h, .. }
        | Lang::ModuleImport { help_data: h, .. }
        | Lang::Import { help_data: h, .. }
        | Lang::Test { help_data: h, .. }
        | Lang::Use { help_data: h, .. } => {
            TypeContext::new(Type::Empty(h.clone()), expr.clone(), context.clone())
        }
        Lang::GenFunc { help_data: h, .. } => TypeContext::new(
            builder::unknown_function_type(),
            expr.clone(),
            context.clone(),
        ),
        Lang::KeyValue { value, .. } => typing(context, value),
        _ => builder::any_type().with_lang(expr, context).into(),
    }
}

/// Flatten a nested `Type::Operator(Union, ...)` tree into a flat `HashSet<Type>`.
pub fn flatten_operator_union(typ: &Type) -> HashSet<Type> {
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
        if arg_typ2.get_argument_str() == at.get_argument() {
            ArgumentType::new(&at.get_argument(), &typing(context, &at.get_value()).value)
        } else {
            arg_typ2.clone()
        }
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

    // DataFrame tests

    #[test]
    fn test_dataframe_parsing_simple() {
        let res = "data__frame(x = [1, 2, 3]) ".parse::<Lang>().unwrap();
        println!("{:?}", res);
        assert_eq!(res.simple_print(), "DataFrame");
    }

    #[test]
    fn test_dataframe_type_simple() {
        let expr = "data__frame(x = [1, 2, 3]) ".parse::<Lang>().unwrap();
        let result = typing_with_errors(&Context::default(), &expr);
        let typ = result.get_type().clone();
        println!("typ: {:?}", typ);
        assert!(
            matches!(typ, Type::Vec(VecType::DataFrame, _, _, _)),
            "Expected Vec(DataFrame, ...) but got {:?}",
            typ
        );
    }

    #[test]
    fn test_dataframe_dollar_access() {
        let fp = FluentParser::new()
            .push("let df <- data__frame(x = [1, 2, 3]);")
            .parse_type_next();
        let context = fp.context.clone();
        let df_type = context.get_type_from_existing_variable(Var::from_name("df"));
        println!("df type: {:?}", df_type);
        use crate::processes::parsing::parse_from_string;
        let expr = parse_from_string("df$x", "test");
        let tc = typing(&context, &expr);
        println!("df$x type: {:?}", tc.value);
        assert!(
            matches!(tc.value, Type::Vec(VecType::Vector, _, _, _)),
            "Expected Vec(Vector, ...) but got {:?}",
            tc.value
        );
    }

    #[test]
    fn test_dataframe_dollar_unknown_field() {
        let fp = FluentParser::new()
            .push("let df <- data__frame(x = [1, 2, 3]);")
            .parse_type_next();
        let context = fp.context.clone();
        use crate::processes::parsing::parse_from_string;
        let expr = parse_from_string("df$unknown", "test");
        let result = typing_with_errors(&context, &expr);
        assert!(
            result.has_errors(),
            "Expected a FieldNotFound error for df$unknown"
        );
    }

    #[test]
    fn test_dataframe_dollar_preserves_inner_type() {
        let fp = FluentParser::new()
            .push("let df <- data__frame(x = [\"a\", \"b\", \"c\"]);")
            .parse_type_next();
        let context = fp.context.clone();
        use crate::processes::parsing::parse_from_string;
        let expr = parse_from_string("df$x", "test");
        let tc = typing(&context, &expr);
        let typ = tc.value.clone();
        if let Type::Vec(VecType::Vector, _, inner, _) = &typ {
            println!("inner type: {:?}", inner);
            assert!(
                matches!(&**inner, Type::Any(_) | Type::Char(_, _)),
                "Expected inner type to be Any or Char but got {:?}",
                inner
            );
        } else {
            panic!("Expected Vec(Vector, ...) but got {:?}", typ);
        }
    }

    #[test]
    fn test_dataframe_dollar_numeric_columns() {
        let fp = FluentParser::new()
            .push("let df <- data__frame(x = [1.0], y = [2.0]);")
            .parse_type_next();
        let context = fp.context.clone();
        use crate::processes::parsing::parse_from_string;
        let expr = parse_from_string("df$x", "test");
        let tc = typing(&context, &expr);
        let typ = tc.value.clone();
        assert!(
            matches!(typ, Type::Vec(VecType::Vector, _, _, _)),
            "Expected Vec(Vector, ...) but got {:?}",
            typ
        );
        if let Type::Vec(VecType::Vector, _, inner, _) = &typ {
            assert!(
                matches!(**inner, Type::Number(_, _)),
                "Expected inner type to be Number but got {:?}",
                inner
            );
        }
    }

    #[test]
    fn test_dataframe_not_subtype_of_record() {
        use crate::components::r#type::argument_type::ArgumentType;
        use crate::components::r#type::vector_type::VecType;
        use std::collections::HashSet;

        let mut fields = HashSet::new();
        fields.insert(ArgumentType::new(
            "Training",
            &builder::character_type_default(),
        ));
        fields.insert(ArgumentType::new("Pulse", &builder::integer_type_default()));

        let record_type = Type::Record(fields, HelpData::default());
        let df_type = Type::Vec(
            VecType::DataFrame,
            Box::new(builder::integer_type(3)),
            Box::new(record_type.clone()),
            HelpData::default(),
        );

        let context = Context::default();
        assert!(
            !df_type.is_subtype(&record_type, &context).0,
            "DataFrame should not be a subtype of Record"
        );
    }

    #[test]
    fn test_record_not_subtype_of_dataframe() {
        use crate::components::r#type::argument_type::ArgumentType;
        use crate::components::r#type::vector_type::VecType;
        use std::collections::HashSet;

        let mut fields = HashSet::new();
        fields.insert(ArgumentType::new(
            "Training",
            &builder::character_type_default(),
        ));
        fields.insert(ArgumentType::new("Pulse", &builder::integer_type_default()));

        let record_type = Type::Record(fields, HelpData::default());
        let df_type = Type::Vec(
            VecType::DataFrame,
            Box::new(builder::integer_type(3)),
            Box::new(record_type.clone()),
            HelpData::default(),
        );

        let context = Context::default();
        assert!(
            !record_type.is_subtype(&df_type, &context).0,
            "Record should not be a subtype of DataFrame"
        );
    }

    #[test]
    fn test_alias_missing_generics_error() {
        use crate::processes::parsing::parse2;
        let res = parse2("type Okay <- int;".into()).unwrap();
        let context = Context::default();
        let result = typing_with_errors(&context, &res);

        assert!(
            !result.has_errors(),
            "Simple type alias without generics should not have errors"
        );
    }

    #[test]
    fn test_alias_catching_generics_no_error() {
        assert!(
            true,
            "Placeholder test - generics in type aliases need proper tag type support"
        );
    }

    #[test]
    fn test_module_type_access_pub_valid() {
        use crate::processes::parsing::parse_from_string;
        let fp = FluentParser::new()
            .push("module geo { @pub type Meters <- int; };")
            .parse_type_next();
        let context = fp.context.clone();

        let expr = parse_from_string("let x: geo::Meters <- 50;", "test");
        let result = typing_with_errors(&context, &expr);
        assert!(
            !result.has_errors(),
            "Expected no errors for @pub type access geo::Meters, got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn test_module_type_access_pub_type_mismatch() {
        use crate::processes::parsing::parse_from_string;
        let fp = FluentParser::new()
            .push("module geo { @pub type Meters <- int; };")
            .parse_type_next();
        let context = fp.context.clone();

        let expr = parse_from_string("let x: geo::Meters <- true;", "test");
        let result = typing_with_errors(&context, &expr);
        assert!(
            result.has_errors(),
            "Expected a type mismatch error when assigning Boolean to geo::Meters (int)"
        );
    }

    #[test]
    fn test_module_type_access_private_not_accessible() {
        use crate::components::error_message::type_error::TypeError;
        use crate::processes::parsing::parse_from_string;
        let fp = FluentParser::new()
            .push("module geo { type Meters <- int; };")
            .parse_type_next();
        let context = fp.context.clone();

        let expr = parse_from_string("let x: geo::Meters <- true;", "test");
        let result = typing_with_errors(&context, &expr);
        assert!(
            result.has_errors(),
            "Expected AliasNotFound for private type geo::Meters"
        );
        assert!(
            result
                .get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::AliasNotFound(_)))),
            "Expected AliasNotFound error but got: {:?}",
            result.get_errors()
        );
    }

    // ==================== Opaque type tests ====================

    #[test]
    fn test_opaque_type_internal_transparent() {
        // Inside a module, opaque types are transparent — functions can use
        // operators on the underlying type.
        use crate::processes::parsing::parse_from_string;
        let expr = parse_from_string(
            "module units { @pub opaque Unit <- int; @pub let make_unit <- fn(x: int): Unit { x }; };",
            "test",
        );
        let result = typing_with_errors(&Context::default(), &expr);
        assert!(
            !result.has_errors(),
            "Module with opaque type should compile without errors, got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn test_opaque_type_external_opaque() {
        // Outside the module, M::Unit resolves to the opaque alias type.
        use crate::processes::parsing::parse_from_string;
        let fp = FluentParser::new()
            .push("module units { @pub opaque Unit <- int; };")
            .parse_type_next();
        let context = fp.context.clone();

        // 5 is int; outside the module, int is NOT a subtype of opaque Unit
        let expr = parse_from_string("let x: units::Unit <- 5;", "test");
        let result = typing_with_errors(&context, &expr);
        assert!(
            result.has_errors(),
            "Assigning int to opaque Unit outside the module should fail"
        );
    }

    #[test]
    fn test_opaque_type_constructor_roundtrip() {
        // The module exports make_unit with return type opaque Unit.
        let fp = FluentParser::new()
            .push(
                "module units { @pub opaque Unit <- int; @pub let make_unit <- fn(x: int): Unit { x }; @pub let get_value <- fn(u: Unit): int { u }; };",
            )
            .parse_type_next();
        let context = fp.context.clone();

        // The module should export make_unit as (int) -> Unit (opaque)
        let units_type = context
            .get_type_from_variable(&Var::from_name("units"))
            .expect("units module should be in context")
            .to_module_type()
            .expect("units should have module type");
        let make_unit_type = units_type
            .get_type_from_name("make_unit")
            .expect("make_unit should be exported");
        println!("make_unit type: {:?}", make_unit_type);
        // Return type of make_unit should be opaque Unit
        if let Type::Function(_, ret, _) = make_unit_type {
            assert!(
                matches!(*ret, Type::Alias(ref name, _, true, _) if name == "Unit"),
                "Expected opaque Unit return type but got {:?}",
                ret
            );
        } else {
            panic!(
                "Expected function type for make_unit, got {:?}",
                make_unit_type
            );
        }
    }

    #[test]
    fn test_opaque_arrow_syntax() {
        // The opaque keyword should accept both `=` and `<-`.
        use crate::processes::parsing::parse;
        let src1 = "module mo { @pub opaque Ox = int; };";
        let src2 = "module mo { @pub opaque Ox <- int; };";
        let r1 = parse(src1.into());
        let r2 = parse(src2.into());
        assert!(!r1.has_errors(), "opaque with = should parse");
        assert!(!r2.has_errors(), "opaque with <- should parse");
    }

    #[test]
    fn test_use_items_imports_into_context() {
        let module_src = "module person { @pub opaque Person <- list { name: char, age: int }; @pub let new_person <- fn(name: char, age: int): Person { :{ name: name, age: age } }; @pub let is_minor <- fn(p: Person): bool { p$age < 18 }; };";
        let fp = FluentParser::new()
            .push(module_src)
            .parse_type_next()
            .push("use person::new_person;")
            .parse_type_next();
        let ctx = fp.context.clone();
        assert!(
            ctx.get_type_from_variable(&Var::from_name("new_person"))
                .is_ok(),
            "new_person should be in context after 'use person::new_person'"
        );
        assert!(
            ctx.get_type_from_variable(&Var::from_name("is_minor"))
                .is_err(),
            "is_minor should NOT be in context (not imported)"
        );
    }

    #[test]
    fn test_use_full_file_simulation() {
        use crate::processes::parsing::parse_from_string;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let src = r#"module person {
    @pub opaque Person <- list { name: char, age: int };
    @pub let new_person <- fn(name: char, age: int): Person { :{ name: name, age: age } };
    @pub let is_minor <- fn(p: Person): bool { p$age < 18 };
};
use person::new_person;
let p <- new_person("Anna", 32);
p"#;
        let lang = parse_from_string(src, "test_app");

        // Step 1: check module alone
        let module_expr = if let Lang::Lines { value: exprs, .. } = &lang {
            exprs[0].clone()
        } else {
            unreachable!()
        };
        let use_expr = if let Lang::Lines { value: exprs, .. } = &lang {
            exprs[1].clone()
        } else {
            unreachable!()
        };

        let ctx0 = Context::default();
        let tc_after_module = TypeChecker::new(ctx0).typing_no_panic(&module_expr);
        let ctx_after_module = tc_after_module.get_context();

        // Check person is in context
        assert!(
            ctx_after_module
                .get_type_from_variable(&Var::from_name("person"))
                .is_ok(),
            "person module should be in context after module definition"
        );

        // Check person's module type has new_person
        let person_type = ctx_after_module
            .get_type_from_variable(&Var::from_name("person"))
            .unwrap();
        let mod_type = person_type
            .to_module_type()
            .expect("person should be a module type");
        let new_person_exported = mod_type.get_type_from_name("new_person");
        assert!(
            new_person_exported.is_ok(),
            "new_person should be exported by person module, got members: {:?}",
            mod_type
                .get_public_members()
                .iter()
                .map(|m| m.get_argument_str())
                .collect::<Vec<_>>()
        );

        // Step 2: check use expression updates context
        let tc_after_use = TypeChecker::new(ctx_after_module).typing_no_panic(&use_expr);
        let ctx_after_use = tc_after_use.get_context();
        assert!(
            ctx_after_use
                .get_type_from_variable(&Var::from_name("new_person"))
                .is_ok(),
            "new_person should be in context after use person::new_person"
        );

        // Step 3: check supertypes and function matching
        if let Lang::Lines { value: exprs, .. } = &lang {
            let tc0 = TypeChecker::new(Context::default());
            let tc1 = tc0.typing_helper_pub(&exprs[0]);
            let ctx_after_module = tc1.get_context();

            // Print what member_type is returned for new_person from module type
            let person_type = ctx_after_module
                .get_type_from_variable(&Var::from_name("person"))
                .unwrap();
            let mod_type = person_type.to_module_type().unwrap();
            let new_person_type = mod_type.get_type_from_name("new_person").unwrap();
            println!("new_person type from module: {:?}", new_person_type);
            println!("new_person type pretty: {}", new_person_type.pretty());
            // Check extract_types
            let extracted = new_person_type.extract_types();
            println!(
                "extracted types: {:?}",
                extracted.iter().map(|t| t.pretty()).collect::<Vec<_>>()
            );

            let tc2 = tc1.typing_helper_pub(&exprs[1]);
            let ctx_after_use = tc2.get_context();

            let anna_type = Type::Char(
                crate::components::r#type::tchar::Tchar::Val("Anna".to_string()),
                crate::components::error_message::help_data::HelpData::default(),
            );
            let char_unknown = builder::character_type_default();

            // Check if Char(Unknown) is in the graph after module
            let in_graph_after_module = ctx_after_module
                .subtypes
                .get_ordered_supertypes(&char_unknown, &ctx_after_module);
            println!(
                "supertypes of Char(Unknown) after module: {:?}",
                in_graph_after_module
                    .iter()
                    .map(|t| t.pretty())
                    .collect::<Vec<_>>()
            );

            // Check if Char(Unknown) is in graph after use
            let in_graph_after_use = ctx_after_use
                .subtypes
                .get_ordered_supertypes(&char_unknown, &ctx_after_use);
            println!(
                "supertypes of Char(Unknown) after use: {:?}",
                in_graph_after_use
                    .iter()
                    .map(|t| t.pretty())
                    .collect::<Vec<_>>()
            );

            // Check supertypes of Char(Val("Anna"))
            let supertypes = ctx_after_use
                .subtypes
                .get_ordered_supertypes(&anna_type, &ctx_after_use);
            println!(
                "supertypes of Char(Val('Anna')): {:?}",
                supertypes.iter().map(|t| t.pretty()).collect::<Vec<_>>()
            );

            // Print memory entries that are char-related
            let hierarchy = ctx_after_use.subtypes.get_hierarchy();
            let level1_lines: Vec<&str> = hierarchy
                .lines()
                .filter(|l| l.trim_start_matches(' ').starts_with("char") || *l == "  char")
                .collect();
            println!(
                "Char top-level hierarchy entries: {:?}",
                &level1_lines[..level1_lines.len().min(10)]
            );
        }
    }

    #[test]
    fn test_graph_char_subtype() {
        use crate::components::context::graph::Graph;
        let ctx = Context::default();
        let char_unknown = builder::character_type_default();
        let char_anna = builder::character_type("Anna");
        let graph = Graph::new();
        let graph = graph.add_type(char_unknown.clone(), &ctx);
        let supertypes = graph.get_ordered_supertypes(&char_anna, &ctx);
        println!(
            "After adding Char(Unknown), supertypes of Char(Val('Anna')): {:?}",
            supertypes.iter().map(|t| t.pretty()).collect::<Vec<_>>()
        );
        assert!(
            supertypes.iter().any(|t| t == &char_unknown),
            "Char(Unknown) should be a supertype of Char(Val('Anna')), got: {:?}",
            supertypes
        );
    }

    #[test]
    fn test_module_non_opaque_type_transpile() {
        // A non-opaque `type` alias in a module should NOT generate opaque/Any suffix.
        use crate::processes::type_checking::type_checker::TypeChecker;
        let fp = FluentParser::new()
            .push("module machin { @pub type Truc <- list { a: int, b: int }; @pub let get_a <- fn(t: Truc): int { t$a }; };")
            .parse_type_next();
        let context = fp.context.clone();

        // get_a exported with type should have Truc (not Any) as parameter class
        let machin_type = context
            .get_type_from_variable(&Var::from_name("machin"))
            .expect("machin should be in context")
            .to_module_type()
            .expect("machin should be a module type");
        let get_a_type = machin_type
            .get_type_from_name("get_a")
            .expect("get_a should be exported");
        println!("get_a type in module: {:?}", get_a_type);
        // The parameter should NOT be Any — it should be Truc or Record, not Any
        if let Type::Function(args, _, _) = &get_a_type {
            let param_ty = args[0].get_type();
            assert!(
                !matches!(param_ty, Type::Any(_)),
                "Parameter type of get_a should not be Any, but was {:?}",
                param_ty
            );
        } else {
            panic!("get_a should be a function type, got {:?}", get_a_type);
        }
    }

    #[test]
    fn test_record_constructor_generated() {
        let fp = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run();
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Point <- function("),
            "Expected constructor function for Point, got:\n{}",
            r_code
        );
        assert!(
            r_code.contains("structure(list("),
            "Expected structure(list(...)) in constructor, got:\n{}",
            r_code
        );
        assert!(
            r_code.contains("class = c(\"Point\", \"list\")"),
            "Expected S3 class annotation in constructor, got:\n{}",
            r_code
        );
    }

    #[test]
    fn test_constructor_call_syntax() {
        let fp = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run()
            .push("let p <- Point:{ x = 1, y = 2 };")
            .run();
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Point("),
            "Expected Point(...) call from constructor syntax, got:\n{}",
            r_code
        );
    }

    // ==================== Union Constructor Tests ====================

    #[test]
    fn test_union_tag_constructor_parsing() {
        let res = "Color.Red".parse::<Lang>().unwrap();
        assert_eq!(res.simple_print(), "UnionConstructor(Color.Red)");
    }

    #[test]
    fn test_union_record_constructor_parsing() {
        let res = "Color.Rgb:{ r = 10, g = 20, b = 30 }"
            .parse::<Lang>()
            .unwrap();
        assert_eq!(res.simple_print(), "UnionConstructor(Color.Rgb)");
        if let Lang::UnionConstructor {
            union_name,
            variant_name,
            fields,
            ..
        } = &res
        {
            assert_eq!(union_name, "Color");
            assert_eq!(variant_name, "Rgb");
            assert_eq!(fields.len(), 3);
        } else {
            panic!("Expected UnionConstructor");
        }
    }

    #[test]
    fn test_union_constructor_typing() {
        let fp = FluentParser::new()
            .push("type Color <- .Red | .Blue;")
            .run()
            .push("Color.Red")
            .parse_type_next();
        let typ = fp.get_last_type();
        assert!(
            matches!(typ, Type::Alias(ref n, _, _, _) if n == "Color"),
            "Expected Color alias type, got: {}",
            typ
        );
    }

    #[test]
    fn test_union_tag_transpilation() {
        let fp = FluentParser::new()
            .push("type Color <- .Red | .Blue;")
            .run()
            .push("Color.Red")
            .run();
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Red()"),
            "Expected Red() in transpiled code, got:\n{}",
            r_code
        );
    }

    #[test]
    fn test_union_record_variant_transpilation() {
        let fp = FluentParser::new()
            .push("type Rgb <- list { r: int, g: int, b: int };")
            .run()
            .push("type Color <- .Red | .Blue | Rgb;")
            .run()
            .push("Color.Rgb:{ r = 10, g = 20, b = 30 }")
            .run();
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Rgb("),
            "Expected Rgb(...) call, got:\n{}",
            r_code
        );
    }

    #[test]
    fn test_union_alias_generates_tag_constructors() {
        let fp = FluentParser::new()
            .push("type Color <- .Red | .Blue;")
            .run();
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Red <- function()"),
            "Expected Red constructor, got:\n{}",
            r_code
        );
        assert!(
            r_code.contains("Blue <- function()"),
            "Expected Blue constructor, got:\n{}",
            r_code
        );
        assert!(
            r_code.contains(r#"class = c("Red", "Color", "list")"#),
            "Expected S3 class hierarchy for Red, got:\n{}",
            r_code
        );
    }

    #[test]
    fn test_union_alias_generates_record_variant_constructor() {
        let fp = FluentParser::new()
            .push("type Rgb <- list { r: int, g: int, b: int };")
            .run()
            .push("type Color <- .Red | Rgb;")
            .run();
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains(r#"class = c("Rgb", "Color", "list")"#),
            "Expected S3 class hierarchy for Rgb, got:\n{}",
            r_code
        );
    }
}
