//! `Op::Dot` (`e1.e2`) and `Op::Pipe` (`e1 |> e2`): both share the same
//! "receiver-first" desugaring and field-access rules, so they're typed by
//! the same function.
use crate::components::context::Context;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::Lang;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::type_checking::field_access;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::type_checking::typing;
use crate::utils::builder;

pub fn dot_pipe_access(context: &Context, expr: &Lang, e1: &Lang, e2: &Lang) -> TypeContext {
    // Uniform function call over the native `c(...)` vector constructor:
    // `receiver.c(args)` desugars to `c(receiver, args)` (a Lang::Vector,
    // not a FunctionApp), so handle it before the generic method case.
    if let Lang::Vector {
        value: v,
        help_data: h,
    } = e2.clone()
    {
        let new_vec = Lang::Vector {
            value: [e1.clone()].iter().chain(v.iter()).cloned().collect(),
            help_data: h.clone(),
        };
        return typing(context, &new_vec);
    }

    // Method-call sugar: `receiver.method(args)` / `receiver |> method(args)`
    // desugars to `method(receiver, args)` regardless of `receiver`'s type.
    if let Lang::FunctionApp {
        identifier: exp,
        arguments: v,
        help_data: h,
    } = e2.clone()
    {
        let fun_app = Lang::FunctionApp {
            identifier: exp,
            arguments: [e1.clone()].iter().chain(v.iter()).cloned().collect(),
            help_data: h.clone(),
        };
        return typing(context, &fun_app);
    }

    let tc2 = typing(context, e2);
    let mut errors = tc2.errors.clone();
    let ty2 = tc2.value.clone().reduce(context);

    match (ty2.clone(), e1.clone()) {
        (
            Type::Record(fields, _),
            Lang::Variable {
                name, help_data: h, ..
            },
        ) => field_access::find_field_or_push_error(&fields, &name, &ty2, h, &mut errors)
            .map(|ty| {
                TypeContext::new(ty, expr.clone(), context.clone()).with_errors(errors.clone())
            })
            .unwrap_or_else(|| {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors.clone())
            }),
        (
            Type::Record(fields, _),
            Lang::Char {
                value: name,
                help_data: h,
            },
        ) => field_access::find_field_or_push_error(&fields, &name, &ty2, h, &mut errors)
            .map(|ty| {
                TypeContext::new(ty, expr.clone(), context.clone()).with_errors(errors.clone())
            })
            .unwrap_or_else(|| {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors.clone())
            }),
        (
            Type::Tuple(vals, _),
            Lang::Integer {
                value: i,
                help_data: h,
            },
        ) => match vals.get((i - 1) as usize) {
            Some(typ) => {
                TypeContext::new(typ.clone(), expr.clone(), context.clone()).with_errors(errors)
            }
            None => {
                errors.push(TypRError::Type(TypeError::WrongExpression(h)));
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            }
        },
        // Record `.` merge with a `List`: adds/overrides fields by union,
        // unlike `$`'s update-in-place semantics (see `dollar_access.rs`).
        (Type::Record(fields1, h), Lang::List { .. }) => {
            let tc1 = e1.typing(context);
            errors.extend(tc1.errors.clone());
            let fields3 = match tc1.value {
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
            TypeContext::new(builder::any_type(), expr.clone(), context.clone()).with_errors(errors)
        }
    }
}
