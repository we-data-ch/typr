//! `Op::Dollar` (`e1$e2`): field/member access, record & DataFrame update,
//! and function calls scoped to a record/module/DataFrame's namespace.
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::array_lang::ArrayLang;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::type_checking::facets;
use crate::processes::type_checking::field_access;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::type_checking::typing;
use crate::utils::builder;
use std::collections::HashSet;

/// `record$field <- new_value`'s type-level counterpart: a `List` on the rhs
/// updates the matching field's type in place (unlike `.`'s union-merge, see
/// `dot_pipe_access.rs`).
fn replace_fields_type_if_needed(
    context: &Context,
    at: crate::components::language::argument_value::ArgumentValue,
) -> impl FnMut(&ArgumentType) -> ArgumentType + use<'_> {
    move |arg_typ2| {
        if arg_typ2.get_argument_str() == at.get_argument() {
            ArgumentType::new(&at.get_argument(), &typing(context, &at.get_value()).value)
        } else {
            arg_typ2.clone()
        }
    }
}

pub fn dollar_access(
    context: &Context,
    expr: &Lang,
    e1: &Lang,
    e2: &Lang,
    hd: &HelpData,
) -> TypeContext {
    let op = match expr {
        Lang::Operator { operator: op, .. } => op.clone(),
        _ => Op::Dollar(HelpData::default()),
    };
    let tc1 = typing(context, e1);
    let mut errors = tc1.errors;
    let ty1 = tc1.value;

    match (ty1.reduce(context), e2.clone(), &op) {
        (
            Type::Record(fields, _),
            Lang::Variable {
                name, help_data: h, ..
            },
            _,
        ) => field_access::find_field_or_push_error(&fields, &name, &ty1, h, &mut errors)
            .map(|ty| {
                TypeContext::new(ty, expr.clone(), context.clone()).with_errors(errors.clone())
            })
            .unwrap_or_else(|| {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors.clone())
            }),
        // Falls through here for anything that isn't a plain `Record`
        // (the arm above already caught that): a `List & Interface`
        // intersection, or a rigid generic bound to one (see
        // `function.rs`'s interface-parameter handling) — either way
        // `record_facet` recovers the field set so `$` still works.
        (
            reduced_ty,
            Lang::Variable {
                name, help_data: h, ..
            },
            _,
        ) if facets::record_facet(context, &reduced_ty).is_some() => {
            let fields = facets::record_facet(context, &reduced_ty).unwrap();
            field_access::find_field_or_push_error(&fields, &name, &ty1, h, &mut errors)
                .map(|ty| {
                    TypeContext::new(ty, expr.clone(), context.clone()).with_errors(errors.clone())
                })
                .unwrap_or_else(|| {
                    TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors.clone())
                })
        }
        (
            Type::Module(fields, _, _),
            Lang::Variable {
                name, help_data: h, ..
            },
            _,
        ) => field_access::find_module_field_or_push_error(&fields, &name, h, &mut errors)
            .map(|ty| {
                TypeContext::new(ty, expr.clone(), context.clone()).with_errors(errors.clone())
            })
            .unwrap_or_else(|| {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors.clone())
            }),
        (
            Type::Module(fields, _, _),
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
                        Ok(ft) => {
                            TypeContext::new(ft.get_return_type(), expr.clone(), context.clone())
                                .with_errors(errors)
                        }
                        Err(_) => {
                            errors.push(TypRError::Type(TypeError::WrongExpression(
                                exp.get_help_data(),
                            )));
                            TypeContext::new(builder::any_type(), expr.clone(), context.clone())
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
        ) => field_access::find_field_or_push_error(&fields, &name, &ty1, h, &mut errors)
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
            _,
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
                        let tc = typing(&context.clone().push_var_type(var, typ, context), e2);
                        errors.extend(tc.errors);
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
            TypeContext::new(Type::UnknownFunction(h), expr.clone(), context.clone())
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
                match field_access::find_field_or_push_error(
                    fields,
                    &name,
                    &ty1,
                    vh.clone(),
                    &mut errors,
                ) {
                    Some(field_ty) => {
                        let inner_type = match field_ty {
                            Type::Vec(_, _, inner, _) => *inner,
                            other => other,
                        };
                        TypeContext::new(
                            Type::Vec(VecType::Vector, n.clone(), Box::new(inner_type), h.clone()),
                            expr.clone(),
                            context.clone(),
                        )
                        .with_errors(errors)
                    }
                    None => TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors),
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
                match field_access::find_field_or_push_error(
                    fields,
                    &name,
                    &ty1,
                    vh.clone(),
                    &mut errors,
                ) {
                    Some(field_ty) => {
                        let inner_type = match field_ty {
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
                    None => TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                        .with_errors(errors),
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
        (Type::Vec(VecType::DataFrame, n, ref body, h), Lang::List { value: fields2, .. }, _) => {
            match body.as_ref() {
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
            }
        }
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
                            let tc = typing(&context.clone().push_var_type(var, typ, context), e2);
                            errors.extend(tc.errors);
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
            _ => {
                errors.push(TypRError::Type(TypeError::WrongExpression(
                    expr.get_help_data(),
                )));
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(errors)
            }
        },
        (Type::Vec(vtype, n, _, h), Lang::Variable { .. }, _) => {
            match ArrayLang::try_from(e1.clone()) {
                Ok(arr_lang) => match arr_lang.get_first_argument() {
                    Some(first_arg) => {
                        let tc = typing(
                            context,
                            &builder::operation(Op::Dollar(hd.clone()), first_arg, e2.clone()),
                        );
                        errors.extend(tc.errors);
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
                    arguments: [e1.clone()].iter().chain(args.iter()).cloned().collect(),
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
            TypeContext::new(builder::any_type(), expr.clone(), context.clone()).with_errors(errors)
        }
    }
}
