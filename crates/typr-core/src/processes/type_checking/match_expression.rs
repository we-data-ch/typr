//! `Lang::Match`: pattern matching over tags, type patterns, tuples, and
//! record literals. Each branch pattern determines what bindings (if any) it
//! adds to that branch's typing context before the branch body is typed.
use crate::components::context::Context;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::type_checking::typing;
use crate::utils::builder;

/// Builds the branch-local context for one `match` arm: binds whatever
/// variables `pattern` introduces, using `match_type` (the scrutinee's type)
/// to resolve their types where possible.
fn build_match_branch_context(context: &Context, pattern: &Lang, match_type: &Type) -> Context {
    match pattern {
        // For tag patterns with bindings, we extract the inner type
        Lang::Tag {
            name: tag_name,
            value: inner,
            ..
        } => match inner.as_ref() {
            Lang::Variable { name: var_name, .. } => {
                let inner_type = match match_type {
                    Type::Tag(tn, inner_t, _h) if tn == tag_name => (**inner_t).clone(),
                    Type::Alias(name, generics, _, _h) => {
                        if name == "Option" && (tag_name == "Some" || tag_name == "None") {
                            generics.first().cloned().unwrap_or_else(builder::any_type)
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
            let tuple_types = match match_type {
                Type::Tuple(types, _) => Some(types.clone()),
                _ => None,
            };
            elements
                .iter()
                .enumerate()
                .fold(context.clone(), |ctx: Context, (i, elem)| {
                    if let Lang::Variable { name: var_name, .. } = elem {
                        if var_name == "_" {
                            return ctx;
                        }
                        let elem_type = tuple_types
                            .as_ref()
                            .and_then(|types| types.get(i).cloned())
                            .unwrap_or_else(builder::any_type);
                        let var = Var::from_name(var_name);
                        ctx.push_var_type(var, elem_type, context)
                    } else {
                        ctx
                    }
                })
        }
        // For list/record patterns `:{nom: n, age: a}`, bind each variable
        Lang::List { value: fields, .. } => {
            // Extract record field types from the matched expression if available
            let record_fields = match match_type {
                Type::Record(field_types, _) => Some(field_types.clone()),
                _ => None,
            };
            fields
                .iter()
                .fold(context.clone(), |ctx: Context, arg_val: &ArgumentValue| {
                    if let Lang::Variable { name: var_name, .. } = &arg_val.get_value() {
                        let field_type = record_fields
                            .as_ref()
                            .and_then(|fields| {
                                fields
                                    .iter()
                                    .find(|at| at.get_argument_str() == arg_val.get_argument())
                            })
                            .map(|at| at.get_type())
                            .unwrap_or_else(builder::any_type);
                        let var = Var::from_name(var_name);
                        ctx.push_var_type(var, field_type, context)
                    } else {
                        ctx
                    }
                })
        }
        _ => context.clone(),
    }
}

pub fn match_expression(
    context: &Context,
    expr: &Lang,
    match_exp: &Lang,
    branches: &[(Lang, Box<Lang>)],
) -> TypeContext {
    let match_tc = typing(context, match_exp);
    let mut errors = match_tc.errors;
    let match_type = match_tc.value;

    let branch_tcs: Vec<_> = branches
        .iter()
        .map(|(pattern, bexp)| {
            let new_context = build_match_branch_context(context, pattern, &match_type);
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
