//! By-name resolution against the final `Context`, for names not bound anywhere in the current
//! `Lang` walk (spec §5.3) — stdlib functions, R package functions, anything not reachable
//! through the scope stack. There is no name-resolution pass in `typr-core` to delegate to (the
//! real dispatch machinery, `Context::get_type_from_variable`/`Var::match_with`, picks a single
//! winner without reporting how many candidates it had to choose from), so this replicates just
//! enough of it to produce a `Confidence`.

use crate::model::Confidence;
use typr_core::components::context::Context;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;

fn first_param_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Function(params, _, _) => params.first().map(|p| p.get_type()),
        _ => None,
    }
}

/// Resolves `name` against every top-level binding in `context` (stdlib included). `arg0` is the
/// type of the call's first argument, if any, used to narrow an overload set the same way
/// `Context::get_type_from_variable` does (via `is_subtype`), but without silently picking a
/// winner when it can't: that case is reported as `Ambiguous`.
pub fn resolve_by_name(context: &Context, name: &str, arg0: Option<&Type>) -> Option<(Type, Confidence)> {
    let candidates: Vec<&Type> = context
        .variables()
        .filter(|(v, _)| v.get_name() == name)
        .map(|(_, t)| t)
        .collect();

    match candidates.as_slice() {
        [] => None,
        [only] => Some(((*only).clone(), Confidence::Exact)),
        many => {
            let Some(arg0) = arg0 else {
                return Some((many[0].clone(), Confidence::ByName));
            };
            let matching: Vec<&Type> = many
                .iter()
                .filter(|t| {
                    first_param_type(t)
                        .map(|p| arg0.is_subtype(&p, context).0 || p.is_subtype(arg0, context).0)
                        .unwrap_or(false)
                })
                .copied()
                .collect();
            match matching.as_slice() {
                [only] => Some(((*only).clone(), Confidence::Exact)),
                [] => Some((many[0].clone(), Confidence::ByName)),
                _ => Some((
                    many[0].clone(),
                    Confidence::Ambiguous {
                        candidates: Vec::new(),
                    },
                )),
            }
        }
    }
}
