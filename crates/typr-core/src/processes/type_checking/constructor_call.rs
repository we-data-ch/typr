//! `TypeName:{ x = 1, y = 2 }` (non-`Self` case; see `typing_self_constructor`
//! in `mod.rs` for `Self:{...}`): resolves the target alias, then validates
//! the field list against the alias's record shape — explicit fields,
//! runtime `...spread`s, and the static `..spread`/coverage check.
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::type_checking::merge_record_fields_override;
use crate::processes::type_checking::resolve_module_member_type;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::type_checking::typing;
use crate::utils::builder;

#[allow(clippy::too_many_arguments)]
pub fn constructor_call(
    context: &Context,
    expr: &Lang,
    module_path: &[String],
    type_name: &str,
    fields: &[ArgumentValue],
    spread: &Option<(Vec<String>, String, HelpData)>,
    spreads: &[Lang],
    h: &HelpData,
) -> TypeContext {
    let resolved_alias = if module_path.is_empty() {
        context.get_type_from_aliases(&Var::from_name(type_name))
    } else {
        resolve_module_member_type(context, module_path, type_name)
    };
    let Some(resolved_alias) = resolved_alias else {
        return TypeContext::new(builder::any_type(), expr.clone(), context.clone());
    };

    let mut errors: Vec<TypRError> = Vec::new();
    let target_type = Type::Alias(type_name.to_string(), vec![], false, h.clone());
    // Thread the context through field/spread typing so that types
    // registered while typing the values (e.g. the on-the-fly ArrayN
    // alias created by an inline `as! [T]` cast) survive to transpile
    // time — otherwise their `as.<name>` lookup falls back to the
    // meaningless `as.Generic()`.
    let mut new_context = context.clone();

    // Record-shaped target: validate the field list (this also fixes the
    // preexisting bug where `fields` was never checked at all, spread or not).
    if let Type::Record(record_fields, _) = resolved_alias.reduce(context) {
        let mut seen = std::collections::HashSet::new();
        for f in fields {
            if !seen.insert(f.get_argument()) {
                errors.push(TypRError::Type(TypeError::DuplicateField(
                    f.get_argument(),
                    h.clone(),
                )));
            }
        }

        for f in fields {
            let fname = f.get_argument();
            match record_fields
                .iter()
                .find(|rf| rf.get_argument_str() == fname)
            {
                Some(rf) => {
                    let value_tc = typing(&new_context, &f.get_value());
                    errors.extend(value_tc.errors);
                    // reduce_and_subtype (not is_subtype): `record_fields`
                    // comes from the *reduced* record, so a field declared
                    // as `[Option]` reads `[any, .Some(T) | .None]` here,
                    // while the value keeps its unreduced `[any, Option]`
                    // alias — is_subtype would split the union before ever
                    // reducing the alias and spuriously fail.
                    if !value_tc.value.reduce_and_subtype(&rf.get_type(), context).0 {
                        errors.push(TypRError::Type(TypeError::Param(
                            rf.get_type(),
                            value_tc.value,
                        )));
                    }
                    new_context = value_tc.context;
                }
                None => {
                    errors.push(TypRError::Type(TypeError::FieldNotFound(
                        (fname, h.clone()),
                        target_type.clone(),
                    )));
                }
            }
        }

        // Runtime `...source` spread(s) (spread_operator2.md): structural
        // merge, sequential like in a record literal. Unlike the static `..`
        // spread below, this doesn't require `source` to be exactly
        // `Alias(type_name)` — only that the merged result covers every
        // declared field with a compatible type.
        let mut spread_merged: Vec<ArgumentType> = Vec::new();
        for spread_expr in spreads {
            let tc = typing(&new_context, spread_expr);
            errors.extend(tc.errors);
            new_context = tc.context;
            match tc.value.reduce(context) {
                Type::Record(spread_fields, _) => {
                    spread_merged = merge_record_fields_override(
                        &spread_merged,
                        &spread_fields.into_iter().collect::<Vec<_>>(),
                    );
                }
                _ => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(
                        spread_expr.get_help_data(),
                    )));
                }
            }
        }

        match spread {
            Some((spread_path, spread_var, spread_h)) => {
                let spread_type = if spread_path.is_empty() {
                    context
                        .get_type_from_variable(&Var::from_name(spread_var))
                        .ok()
                } else {
                    resolve_module_member_type(context, spread_path, spread_var)
                };
                match spread_type {
                    Some(Type::Alias(name, ..)) if name == *type_name => {}
                    Some(found) => {
                        errors.push(TypRError::Type(TypeError::SpreadTypeMismatch(
                            target_type.clone(),
                            found,
                            spread_h.clone(),
                        )));
                    }
                    None => {
                        errors.push(TypRError::Type(TypeError::UndefinedVariable(
                            Lang::Variable {
                                name: spread_var.clone(),
                                is_opaque: false,
                                related_type: builder::any_type(),
                                help_data: spread_h.clone(),
                            },
                        )));
                    }
                }
            }
            None => {
                // No static `..` spread: every record field must be covered
                // either explicitly or by a runtime `...` spread above.
                let provided: std::collections::HashSet<String> =
                    fields.iter().map(|f| f.get_argument()).collect();
                for rf in &record_fields {
                    let rname = rf.get_argument_str();
                    if provided.contains(&rname) {
                        continue;
                    }
                    match spread_merged
                        .iter()
                        .find(|mf| mf.get_argument_str() == rname)
                    {
                        Some(mf) => {
                            if !mf.get_type().is_subtype(&rf.get_type(), context).0 {
                                errors.push(TypRError::Type(TypeError::Param(
                                    rf.get_type(),
                                    mf.get_type(),
                                )));
                            }
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::MissingField(
                                rname,
                                target_type.clone(),
                                h.clone(),
                            )));
                        }
                    }
                }
            }
        }
    }

    TypeContext::new(target_type, expr.clone(), new_context).with_errors(errors)
}
