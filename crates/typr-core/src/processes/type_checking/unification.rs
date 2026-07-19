use crate::components::context::Context;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::Type;
use crate::processes::type_checking::type_comparison;
use std::collections::HashSet;

/// Compare two index/label keys by *name*.
///
/// `Type`'s `PartialEq` treats every `IndexGen`/`LabelGen` as equal regardless
/// of its name (e.g. `IndexGen("I") == IndexGen("J")`). That is fine for
/// structural subtyping but disastrous for substitution-map lookups: it makes
/// every index variable resolve to the *first* binding in the map. Index/label
/// lookups must therefore match on the variable name so that, for example,
/// `[#J-#I/#K, int]` with `{I→1, J→20, K→1}` resolves correctly.
///
/// NB: `Generic` is intentionally *not* handled here. Several inference paths
/// (notably untyped-lambda return-type inference) still rely on the loose
/// `Generic` equality, so reworking it is out of scope for this fix.
pub fn same_generic_key(key: &Type, var: &Type) -> bool {
    match (key, var) {
        (Type::Generic(a, _), Type::Generic(b, _)) => a == b,
        (Type::IndexGen(a, _), Type::IndexGen(b, _)) => a == b,
        (Type::LabelGen(a, _), Type::LabelGen(b, _)) => a == b,
        (Type::KindedGen(k1, a, _), Type::KindedGen(k2, b, _)) => k1 == k2 && a == b,
        _ => false,
    }
}

pub fn type_substitution(type_: &Type, substitutions: &[(Type, Type)]) -> Type {
    if substitutions.is_empty() {
        return type_.clone();
    }

    match type_ {
        // Generic type substitution
        Type::Generic(_, _) => {
            if let Some((_, replacement)) = substitutions.iter().find(|(gen_name, _)| gen_name == type_) {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Index generic substitution
        Type::IndexGen(_, _) => {
            if let Some((_, replacement)) = substitutions
                .iter()
                .find(|(idx_name, _)| same_generic_key(idx_name, type_))
            {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Label generic substitution
        Type::LabelGen(_, _) => {
            if let Some((_, replacement)) = substitutions
                .iter()
                .find(|(idx_name, _)| same_generic_key(idx_name, type_))
            {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Kinded generic substitution
        Type::KindedGen(_, _, _) => {
            if let Some((_, replacement)) = substitutions.iter().find(|(key, _)| same_generic_key(key, type_)) {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Arithmetic operations
        Type::Operator(TypeOperator::Addition, t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(n, h), Type::Number(_, _)) => Type::Number(n, h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1 + i2, h),
                _ => Type::Operator(TypeOperator::Addition, Box::new(v1), Box::new(v2), h.clone()),
            }
        }

        Type::Operator(TypeOperator::Substraction, t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(n, h), Type::Number(_, _)) => Type::Number(n, h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1 - i2, h),
                _ => Type::Operator(TypeOperator::Substraction, Box::new(v1), Box::new(v2), h.clone()),
            }
        }

        Type::Operator(TypeOperator::Multiplication, t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(n, h), Type::Number(_, _)) => Type::Number(n, h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1 * i2, h),
                _ => Type::Operator(TypeOperator::Multiplication, Box::new(v1), Box::new(v2), h.clone()),
            }
        }

        Type::Operator(TypeOperator::Division, t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(n, h), Type::Number(_, _)) => Type::Number(n, h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1 / i2, h),
                _ => Type::Operator(TypeOperator::Division, Box::new(v1), Box::new(v2), h.clone()),
            }
        }

        // Array type substitution
        Type::Vec(vtype, size, element_type, h) => Type::Vec(
            vtype.clone(),
            Box::new(type_substitution(size, substitutions)),
            Box::new(type_substitution(element_type, substitutions)),
            h.clone(),
        ),

        // Record type substitution
        Type::Record(fields, h) => Type::Record(
            fields
                .iter()
                .map(|arg_type| {
                    ArgumentType(
                        arg_type.0.clone(),
                        type_substitution(&arg_type.1, substitutions),
                        arg_type.2,
                        arg_type.3,
                        arg_type.4.clone(),
                    )
                })
                .collect(),
            h.clone(),
        ),

        // Function type substitution
        Type::Function(params, return_type, h) => Type::Function(
            params
                .iter()
                .map(|arg| {
                    let new_type = type_substitution(&arg.get_type(), substitutions);
                    ArgumentType::new(&arg.get_argument_str(), &new_type)
                })
                .collect(),
            Box::new(type_substitution(return_type, substitutions)),
            h.clone(),
        ),

        // Alias type substitution
        Type::Alias(name, params, opacity, h) => Type::Alias(
            name.clone(),
            params
                .iter()
                .map(|param| type_substitution(param, substitutions))
                .collect(),
            *opacity,
            h.clone(),
        ),

        // Tag type substitution
        Type::Tag(name, inner_type, h) => Type::Tag(
            name.clone(),
            Box::new(type_substitution(inner_type, substitutions)),
            h.clone(),
        ),

        // Operator type substitution (e.g. union types like .Some(T) | .None)
        Type::Operator(op, t1, t2, h) => Type::Operator(
            *op,
            Box::new(type_substitution(t1, substitutions)),
            Box::new(type_substitution(t2, substitutions)),
            h.clone(),
        ),

        // Default case: return the type unchanged
        _ => type_.clone(),
    }
}

/// Occurs check (G3): does `typ` contain a `Generic` named `name` anywhere
/// inside its structure? Used to reject a binding like `T ↦ [T]` before it
/// gets written into the substitution map — accepting it would make
/// `type_substitution` (and anything that walks the resulting type) recurse
/// forever the next time `T` is substituted back in.
fn type_contains_generic(typ: &Type, name: &str) -> bool {
    match typ {
        Type::Generic(g, _) => g == name,
        Type::Function(params, ret, _) => {
            params.iter().any(|p| type_contains_generic(&p.get_type(), name)) || type_contains_generic(ret, name)
        }
        Type::Vec(_, size, elem, _) => type_contains_generic(size, name) || type_contains_generic(elem, name),
        Type::Record(fields, _) => fields.iter().any(|f| type_contains_generic(&f.get_type(), name)),
        Type::Alias(_, params, _, _) => params.iter().any(|p| type_contains_generic(p, name)),
        Type::Tag(_, inner, _) => type_contains_generic(inner, name),
        Type::Interface(fields, _) => fields.iter().any(|f| type_contains_generic(&f.get_type(), name)),
        Type::Multi(inner, _) => type_contains_generic(inner, name),
        Type::Tuple(elems, _) => elems.iter().any(|e| type_contains_generic(e, name)),
        Type::Operator(_, t1, t2, _) => type_contains_generic(t1, name) || type_contains_generic(t2, name),
        _ => false,
    }
}

fn match_wildcard(fields: &HashSet<ArgumentType>, arg_type: ArgumentType) -> Vec<(Type, Type)> {
    let (labels, types) = fields.iter().fold((vec![], vec![]), |(mut lbl, mut typ), el| {
        lbl.push(el.get_argument());
        typ.push(el.get_type());
        (lbl, typ)
    });
    vec![
        (arg_type.get_argument(), Type::Tuple(labels.clone(), labels.into())),
        (arg_type.get_type(), Type::Tuple(types.clone(), types.into())),
    ]
}

// Add these new functions to the previous implementation

/// Attempts to unify `type1` and `type2`, producing the generic bindings
/// required to make them equal. `None` means the two types are genuinely
/// **not unifiable** (G2) — distinct from `Some(vec![])`, which means they
/// unify with no new bindings (e.g. already-equal concrete types). Before
/// this distinction existed, both cases returned a bare `vec![]` and callers
/// could not tell "these don't unify" from "these unify trivially".
#[allow(clippy::only_used_in_recursion)]
fn unification_helper(values: &[Type], type1: &Type, type2: &Type) -> Option<Vec<(Type, Type)>> {
    match (type1, type2) {
        // Direct equality case
        (t1, t2) if t1 == t2 => Some(vec![]),
        // Any case
        (Type::Any(_), _) => Some(vec![]),
        (_, Type::Any(_)) => Some(vec![]),

        // Generic case. Occurs check (G3): reject `T ↦ <something containing
        // T>` (e.g. `T ↦ [T]`) instead of writing a self-referential binding
        // that would make substitution recurse forever; `T ↦ T` itself is
        // fine and simply produces no binding.
        (t, Type::Generic(g, h)) | (Type::Generic(g, h), t) => {
            if matches!(t, Type::Generic(g2, _) if g2 == g) {
                Some(vec![])
            } else if type_contains_generic(t, g) {
                None
            } else {
                Some(vec![(Type::Generic(g.clone(), h.clone()), t.clone())])
            }
        }

        // label generic case with label
        (Type::Char(s, h), Type::LabelGen(g, h2)) | (Type::LabelGen(g, h2), Type::Char(s, h)) => Some(vec![(
            Type::LabelGen(g.clone(), h2.clone()),
            Type::Char(s.clone(), h.clone()),
        )]),

        // Index generic case with number
        (Type::Integer(i, h), Type::IndexGen(g, h2)) | (Type::IndexGen(g, h2), Type::Integer(i, h)) => Some(vec![(
            Type::IndexGen(g.clone(), h2.clone()),
            Type::Integer(*i, h.clone()),
        )]),

        // Kinded generic case with a concrete type: only bind if the
        // concrete type's kind matches the sigil (RFC sigils.md §4.3). On
        // mismatch, unification genuinely fails for this slot; not-yet-
        // resolvable shapes (Function, Tuple, Alias, ...) are permissively
        // accepted, matching `accepts_number_kind`'s philosophy.
        (concrete, Type::KindedGen(k, g, h2)) | (Type::KindedGen(k, g, h2), concrete)
            if !matches!(
                concrete,
                Type::Generic(_, _)
                    | Type::IndexGen(_, _)
                    | Type::LabelGen(_, _)
                    | Type::KindedGen(_, _, _)
                    | Type::Any(_)
            ) =>
        {
            // Number/Integer are definitely not any of the four `Kind`
            // variants (Number isn't a `Kind` value — see kind.rs), so they
            // must be rejected explicitly rather than falling through
            // `type_kind`'s permissive `None` case.
            if matches!(concrete, Type::Number(_, _) | Type::Integer(_, _)) {
                None
            } else {
                match crate::components::r#type::kind::type_kind(concrete) {
                    Some(actual) if actual == *k => {
                        Some(vec![(Type::KindedGen(*k, g.clone(), h2.clone()), concrete.clone())])
                    }
                    Some(_) => None,
                    None => Some(vec![(Type::KindedGen(*k, g.clone(), h2.clone()), concrete.clone())]),
                }
            }
        }

        // Function case
        (Type::Function(params1, ret1, _), Type::Function(params2, ret2, _)) => {
            if params1.len() != params2.len() {
                return None;
            }

            // Unify return types
            let mut matches = unification_helper(values, ret1, ret2)?;

            // Unify parameters (extract the actual Type from ArgumentType)
            for (p1, p2) in params1.iter().zip(params2.iter()) {
                let param_matches = unification_helper(values, &p1.get_type(), &p2.get_type())?;
                if !merge_substitutions(&mut matches, param_matches) {
                    return None;
                }
            }

            Some(matches)
        }

        // Array case
        (Type::Vec(_, size1, elem1, _), Type::Vec(_, size2, elem2, _)) => {
            let mut combined = unification_helper(values, size1, size2)?;
            let elem_matches = unification_helper(values, elem1, elem2)?;
            if !merge_substitutions(&mut combined, elem_matches) {
                return None;
            }
            Some(combined)
        }

        // Tag case
        (Type::Tag(name1, type1, _h1), Type::Tag(name2, type2, _h2)) if name1 == name2 => {
            unification_helper(values, type1, type2)
        }

        // Record case
        (Type::Record(fields1, _), Type::Record(fields2, _)) => {
            if let Some((intersection1, intersection2)) = record_intersection(fields1, fields2) {
                let types1: Vec<_> = intersection1.iter().map(|arg| &arg.1).collect();
                let types2: Vec<_> = intersection2.iter().map(|arg| &arg.1).collect();

                let mut all_matches = vec![];
                for (t1, t2) in types1.iter().zip(types2.iter()) {
                    let matches = unification_helper(values, t1, t2)?;
                    if !merge_substitutions(&mut all_matches, matches) {
                        return None;
                    }
                }
                Some(all_matches)
            } else {
                type2
                    .get_type_pattern()
                    .map(|arg_type| match_wildcard(fields1, arg_type))
            }
        }

        // Default case - types are not unifiable
        _ => None,
    }
}

/// Like `unify`, but distinguishes genuine unification failure (`None`) from
/// success with no bindings (`Some(vec![])`) — see `unification_helper`.
pub fn try_unify(cont: &Context, type1: &Type, type2: &Type) -> Option<Vec<(Type, Type)>> {
    let new_type1 = type_comparison::reduce_type(cont, type1);
    let new_type2 = type_comparison::reduce_type(cont, type2);
    unification_helper(&[], &new_type1, &new_type2)
}

pub fn unify(cont: &Context, type1: &Type, type2: &Type) -> Vec<(Type, Type)> {
    try_unify(cont, type1, type2).unwrap_or_default()
}

// Helper functions needed for unification

fn resolve_in_chain(existing: &[(Type, Type)], typ: &Type) -> Type {
    let mut current = typ.clone();
    let mut seen = HashSet::new();
    loop {
        let found = existing.iter().find(|(k, _)| *k == current);
        match found {
            Some((_, next)) => {
                if *next == current || !seen.insert(current.clone()) {
                    break;
                }
                current = (*next).clone();
            }
            None => break,
        }
    }
    current
}

fn transitive_closure(existing: &mut [(Type, Type)]) {
    let mut changed = true;
    while changed {
        changed = false;
        let mut i = 0;
        while i < existing.len() {
            let resolved = resolve_in_chain(existing, &existing[i].1);
            if resolved != existing[i].1 {
                existing[i].1 = resolved;
                changed = true;
            }
            i += 1;
        }
    }
}

/// Merges `new` bindings into `existing`. Returns `false` (G1) when a
/// generic is bound to two different, unrelated concrete types by the two
/// sets — e.g. `f(x: T, y: T)` unifying `T ↦ int` from `x` then `T ↦ char`
/// from `y`. This used to overwrite the earlier binding silently, making
/// such a call type-check with whichever binding happened to be last;
/// `existing` may be left partially updated when this returns `false`, but
/// every caller discards its accumulator on failure so that's harmless.
fn merge_substitutions(existing: &mut Vec<(Type, Type)>, new: Vec<(Type, Type)>) -> bool {
    for (name, type_) in new {
        let resolved_type = resolve_in_chain(existing, &type_);
        if let Some(pos) = existing.iter().position(|(n, _)| n == &name) {
            if resolved_type != existing[pos].1 {
                return false;
            }
        } else {
            existing.push((name, resolved_type));
        }
    }
    transitive_closure(existing);
    true
}

pub fn record_intersection(
    record1: &HashSet<ArgumentType>,
    record2: &HashSet<ArgumentType>,
) -> Option<(Vec<ArgumentType>, Vec<ArgumentType>)> {
    // Get labels (left elements) from both records
    let labels1: Vec<String> = record1
        .iter()
        .map(|arg| arg.get_argument_str().clone()) // Assuming ArgumentType has a label field
        .collect();

    let labels2: Vec<String> = record2.iter().map(|arg| arg.get_argument_str()).collect();

    // Find intersection of labels
    let common_labels: Vec<String> = labels1
        .iter()
        .filter(|label| labels2.contains(label))
        .cloned()
        .collect();

    // Get values for the common labels from each record
    let mut values1 = Vec::new();
    let mut values2 = Vec::new();

    for label in &common_labels {
        if let Some(value1) = record1.iter().find(|arg| arg.get_argument_str() == *label).cloned() {
            if let Some(value2) = record2.iter().find(|arg| arg.get_argument_str() == *label).cloned() {
                values1.push(value1);
                values2.push(value2);
            }
        }
    }

    // Merge labels with their respective values
    let intersection1 = values1.into_iter().collect();

    let intersection2 = values2.into_iter().collect();

    Some((intersection1, intersection2))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::error_message::help_data::HelpData;
    use crate::components::r#type::kind::Kind;
    use crate::utils::builder;

    #[test]
    fn test_integer_does_not_bind_to_record_kinded_generic() {
        let context = Context::default();
        let kinded = Type::KindedGen(Kind::Record, "R".to_string(), HelpData::default());
        let bindings = unify(&context, &builder::integer_type_default(), &kinded);
        assert!(
            bindings.is_empty(),
            "an int should not unify with a %R-kinded generic, got: {:?}",
            bindings
        );
    }

    #[test]
    fn test_record_binds_to_record_kinded_generic() {
        let context = Context::default();
        let kinded = Type::KindedGen(Kind::Record, "R".to_string(), HelpData::default());
        let record = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let bindings = unify(&context, &record, &kinded);
        assert!(!bindings.is_empty(), "a record should unify with a %R-kinded generic");
    }

    // G2: `try_unify` must distinguish real failure (`None`) from success
    // with no new bindings (`Some(vec![])`) — the bare `unify` wrapper used
    // to collapse both into the same empty `Vec`.
    #[test]
    fn test_try_unify_none_on_incompatible_concrete_types() {
        let context = Context::default();
        let result = try_unify(&context, &builder::integer_type_default(), &builder::boolean_type());
        assert!(
            result.is_none(),
            "an int and a bool should not unify at all, got: {:?}",
            result
        );
    }

    #[test]
    fn test_try_unify_some_empty_on_trivial_success() {
        let context = Context::default();
        let result = try_unify(
            &context,
            &builder::integer_type_default(),
            &builder::integer_type_default(),
        );
        assert_eq!(
            result,
            Some(vec![]),
            "two identical concrete types should unify with no new bindings"
        );
    }

    // G1: the same generic bound to two different, unrelated concrete types
    // within one composite unification (here: two function parameters both
    // typed `T`) must fail the whole unification instead of silently keeping
    // whichever binding was produced last.
    #[test]
    fn test_conflicting_generic_bindings_fail_function_unification() {
        let context = Context::default();
        let generic_fn = builder::function_type(
            &[builder::generic_type(), builder::generic_type()],
            builder::generic_type(),
        );
        let concrete_fn = builder::function_type(
            &[builder::integer_type_default(), builder::character_type_default()],
            builder::integer_type_default(),
        );
        let result = try_unify(&context, &generic_fn, &concrete_fn);
        assert!(
            result.is_none(),
            "T bound to both int and char should fail unification, got: {:?}",
            result
        );
    }

    #[test]
    fn test_consistent_generic_bindings_still_succeed() {
        let context = Context::default();
        let generic_fn = builder::function_type(
            &[builder::generic_type(), builder::generic_type()],
            builder::generic_type(),
        );
        let concrete_fn = builder::function_type(
            &[builder::integer_type_default(), builder::integer_type_default()],
            builder::integer_type_default(),
        );
        let result = try_unify(&context, &generic_fn, &concrete_fn);
        assert!(
            result.is_some(),
            "T bound consistently to int everywhere should unify, got: {:?}",
            result
        );
    }

    // G3: occurs check — `T` must not bind to a type that contains `T`
    // itself (e.g. `T ↦ [T]`), which would make substitution recurse
    // forever the next time `T` is resolved.
    #[test]
    fn test_occurs_check_rejects_self_referential_binding() {
        let context = Context::default();
        let t = builder::generic_type();
        let array_of_t = builder::array_type(builder::integer_type(0), t.clone());
        let result = try_unify(&context, &t, &array_of_t);
        assert!(
            result.is_none(),
            "T should not bind to [T] (occurs check), got: {:?}",
            result
        );
    }

    #[test]
    fn test_generic_binds_to_itself_without_occurs_check_failure() {
        let context = Context::default();
        let t1 = builder::generic_type();
        let t2 = builder::generic_type();
        let result = try_unify(&context, &t1, &t2);
        assert_eq!(result, Some(vec![]), "T unifying with T itself is trivial, not a cycle");
    }
}
