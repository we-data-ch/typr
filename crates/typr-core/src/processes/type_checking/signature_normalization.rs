//! Signature normalisation for implicit interface generics (`Lovable@A`).
//!
//! RFC `interface_generic_unification`, plan phase 2. One pass over a function
//! signature (`params` + return type) that:
//!
//! - gives every anonymous `I@_` a fresh id (`_0`, `_1`, …);
//! - desugars a bare interface alias `I` into `I@I` (decision D3-A: one
//!   variable shared by every bare occurrence of the same interface);
//! - reports the three ill-formed signatures of RFC §7.3: two different bounds
//!   for one id, an id colliding with a free generic of the same name, an id
//!   that only appears in the return type.
//!
//! The desugared signature is *returned* but not yet consumed: the body
//! (phase 3) and the call site (phase 4) still read the raw signature, so
//! only the errors are wired into `function()` for now.

use crate::components::error_message::typr_error::TypRError;
use crate::components::error_message::type_error::TypeError;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_system::TypeSystem;
use crate::processes::type_checking::facets;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::Context;
use crate::Type;

/// Id of an anonymous `I@_` before it is renamed.
const ANONYMOUS_ID: &str = "_";

pub struct NormalizedSignature {
    pub params: Vec<ArgumentType>,
    pub ret: Type,
    pub errors: Vec<TypRError>,
}

/// Rebuilds `typ` top-down: `f` may replace a node (and then its children are
/// not visited) or return `None` to descend into it. Mirrors the recursion
/// shape of `collect_generic_kind_occurrences` in `function.rs`.
pub(crate) fn rewrite(typ: &Type, f: &mut dyn FnMut(&Type) -> Option<Type>) -> Type {
    if let Some(replacement) = f(typ) {
        return replacement;
    }
    match typ {
        Type::Function(args, ret, h) => Type::Function(
            args.iter()
                .map(|a| a.clone().set_type(rewrite(&a.get_type(), f)))
                .collect(),
            Box::new(rewrite(ret, f)),
            h.clone(),
        ),
        Type::Vec(vt, len, body, h) => Type::Vec(vt.clone(), len.clone(), Box::new(rewrite(body, f)), h.clone()),
        Type::Record(fields, h) => Type::Record(
            fields
                .iter()
                .map(|a| a.clone().set_type(rewrite(&a.get_type(), f)))
                .collect(),
            h.clone(),
        ),
        Type::Tag(name, inner, h) => Type::Tag(name.clone(), Box::new(rewrite(inner, f)), h.clone()),
        Type::Multi(inner, h) => Type::Multi(Box::new(rewrite(inner, f)), h.clone()),
        Type::Operator(op, a, b, h) => {
            Type::Operator(op.clone(), Box::new(rewrite(a, f)), Box::new(rewrite(b, f)), h.clone())
        }
        Type::Params(ts, h) => Type::Params(ts.iter().map(|t| rewrite(t, f)).collect(), h.clone()),
        Type::Tuple(ts, h) => Type::Tuple(ts.iter().map(|t| rewrite(t, f)).collect(), h.clone()),
        Type::Alias(name, params, opaque, h) => Type::Alias(
            name.clone(),
            params.iter().map(|t| rewrite(t, f)).collect(),
            *opaque,
            h.clone(),
        ),
        other => other.clone(),
    }
}

/// Read-only walk with the same traversal as `rewrite`.
pub(crate) fn visit(typ: &Type, f: &mut dyn FnMut(&Type)) {
    rewrite(typ, &mut |t| {
        f(t);
        None
    });
}

/// Whether an `I@Id` variable occurs anywhere in `typ`.
pub(crate) fn has_bounded(typ: &Type) -> bool {
    let mut found = false;
    visit(typ, &mut |t| found |= matches!(t, Type::Bounded(..)));
    found
}

/// `(id, bound, position)` for every `Bounded` reachable in `typ`, in order.
fn bounded_occurrences(typ: &Type) -> Vec<(String, Type, crate::components::error_message::help_data::HelpData)> {
    let mut acc = Vec::new();
    visit(typ, &mut |t| {
        if let Type::Bounded(id, bound, h) = t {
            acc.push((id.clone(), (**bound).clone(), h.clone()));
        }
    });
    acc
}

/// Names of the free generics (`T`, and index generics `#N`) reachable in `typ`.
/// `@A` (`KindedGen`) is left out on purpose: RFC D1 lets `@A` and `I@A` name
/// the same variable.
fn free_generic_names(typ: &Type) -> Vec<String> {
    let mut acc = Vec::new();
    visit(typ, &mut |t| match t {
        Type::Generic(name, _) | Type::IndexGen(name, _) => acc.push(name.clone()),
        _ => {}
    });
    acc
}

fn rename_anonymous(params: &[ArgumentType], ret: &Type) -> (Vec<ArgumentType>, Type) {
    let mut counter = 0usize;
    let mut fresh = |t: &Type| match t {
        Type::Bounded(id, bound, h) if id == ANONYMOUS_ID => {
            let name = format!("_{}", counter);
            counter += 1;
            Some(Type::Bounded(name, bound.clone(), h.clone()))
        }
        _ => None,
    };
    let params = params
        .iter()
        .map(|p| p.clone().set_type(rewrite(&p.get_type(), &mut fresh)))
        .collect();
    let ret = rewrite(ret, &mut fresh);
    (params, ret)
}

/// `I` → `I@I` for every bare alias that reduces to an interface. Inline
/// `interface { … }` types have no name to share and are left alone, as is
/// anything already `Bounded`.
fn desugar_bare_interfaces(context: &Context, typ: &Type) -> Type {
    rewrite(typ, &mut |t| match t {
        Type::Bounded(..) => Some(t.clone()),
        Type::Alias(name, args, _, h) if args.is_empty() => facets::interface_facet(context, t)
            .is_some()
            .then(|| Type::Bounded(name.clone(), Box::new(t.clone()), h.clone())),
        _ => None,
    })
}

pub fn normalize_signature(context: &Context, params: &[ArgumentType], ret: &Type) -> NormalizedSignature {
    let (params, ret) = rename_anonymous(params, ret);
    let mut errors = Vec::new();

    let param_occurrences: Vec<_> = params.iter().flat_map(|p| bounded_occurrences(&p.get_type())).collect();
    let ret_occurrences = bounded_occurrences(&ret);

    // Two different bounds for one id. Aliases are compared by name (after
    // reduction, so `type L2 <- Lovable` isn't a spurious conflict).
    let all: Vec<_> = param_occurrences.iter().chain(ret_occurrences.iter()).collect();
    let mut reported: Vec<&String> = Vec::new();
    for (i, (id, bound, _)) in all.iter().enumerate() {
        if reported.contains(&&*id) {
            continue;
        }
        let reduced = reduce_type(context, bound);
        let clash = all[i + 1..]
            .iter()
            .find(|(other_id, other, _)| other_id == id && reduce_type(context, other) != reduced);
        if let Some((_, other, h)) = clash {
            reported.push(&*id);
            errors.push(TypRError::Type(TypeError::ConflictingBound(
                id.clone(),
                bound.pretty(),
                other.pretty(),
                h.clone(),
            )));
        }
    }

    // An id sharing its name with a free generic (`fn(a: T, b: Lovable@T)`).
    let mut free: Vec<String> = params.iter().flat_map(|p| free_generic_names(&p.get_type())).collect();
    free.extend(free_generic_names(&ret));
    let mut collided: Vec<&String> = Vec::new();
    for (id, _, h) in &all {
        if free.contains(id) && !collided.contains(&id) {
            collided.push(id);
            errors.push(TypRError::Type(TypeError::BoundCollidesWithGeneric(
                id.clone(),
                h.clone(),
            )));
        }
    }

    // An id that only appears in the return type has nothing to be inferred
    // from: same existential problem as a bare interface returned alone.
    let mut orphaned: Vec<&String> = Vec::new();
    for (id, bound, h) in &ret_occurrences {
        let anchored = param_occurrences.iter().any(|(pid, _, _)| pid == id);
        if !anchored && !orphaned.contains(&id) {
            orphaned.push(id);
            errors.push(TypRError::Type(TypeError::InterfaceReturnOnly(Type::Bounded(
                id.clone(),
                Box::new(bound.clone()),
                h.clone(),
            ))));
        }
    }

    let params = params
        .iter()
        .map(|p| p.clone().set_type(desugar_bare_interfaces(context, &p.get_type())))
        .collect();
    let ret = desugar_bare_interfaces(context, &ret);
    NormalizedSignature { params, ret, errors }
}

/// Outcome of matching a call's arguments against a signature that carries
/// `I@Id` variables (declared, or desugared from repeated bare interfaces).
pub enum CallInstance {
    /// Nothing to do: the signature has no shared interface variable.
    NotBounded,
    /// One argument does not satisfy its bound (or the shapes disagree).
    Rejected,
    /// Two arguments disagree on an id.
    Clash(IdClash),
    /// The signature with every `Bounded(id, _)` replaced by the type `id`
    /// was bound to. Generics left (`#N`, `T`) are for the usual filters.
    Instantiated(Vec<Type>, Type),
}

/// `id` was bound to the type of argument `first_arg`, then met `second_type`
/// at `second_arg` (0-based positions in the call).
pub struct IdClash {
    pub id: String,
    pub first_arg: usize,
    pub first_type: Type,
    pub second_arg: usize,
    pub second_type: Type,
}

/// Two arguments sharing an id must have the same type (RFC §7.2): mutual
/// subtyping, since `Type`'s `PartialEq` is too loose for generics.
fn same_type(a: &Type, b: &Type, context: &Context) -> bool {
    a.is_subtype_raw(b, context) && b.is_subtype_raw(a, context)
}

/// Binds every `Bounded` in `param` against the matching part of `concrete`.
/// Shapes that do not line up are left to the regular filters.
fn bind_ids(
    concrete: &Type,
    param: &Type,
    arg: usize,
    context: &Context,
    binds: &mut Vec<(String, Type, usize)>,
    clash: &mut Option<IdClash>,
) -> bool {
    match param {
        Type::Bounded(id, bound, _) => {
            if !concrete.is_subtype_raw(&reduce_type(context, bound), context) {
                return false;
            }
            match binds.iter().find(|(k, _, _)| k == id) {
                Some((_, previous, first_arg)) => {
                    let same = same_type(previous, concrete, context);
                    if !same && clash.is_none() {
                        *clash = Some(IdClash {
                            id: id.clone(),
                            first_arg: *first_arg,
                            first_type: previous.clone(),
                            second_arg: arg,
                            second_type: concrete.clone(),
                        });
                    }
                    same
                }
                None => {
                    binds.push((id.clone(), concrete.clone(), arg));
                    true
                }
            }
        }
        Type::Vec(_, _, elem_p, _) => match reduce_type(context, concrete) {
            Type::Vec(_, _, elem_c, _) => bind_ids(&elem_c, elem_p, arg, context, binds, clash),
            _ => true,
        },
        Type::Tuple(ps, _) => match reduce_type(context, concrete) {
            Type::Tuple(cs, _) if cs.len() == ps.len() => {
                cs.iter().zip(ps.iter()).all(|(c, p)| bind_ids(c, p, arg, context, binds, clash))
            }
            _ => true,
        },
        Type::Function(ps, rp, _) => match reduce_type(context, concrete) {
            Type::Function(cs, rc, _) if cs.len() == ps.len() => {
                cs.iter()
                    .zip(ps.iter())
                    .all(|(c, p)| bind_ids(&c.get_type(), &p.get_type(), arg, context, binds, clash))
                    && bind_ids(&rc, rp, arg, context, binds, clash)
            }
            _ => true,
        },
        _ => true,
    }
}

/// Plan phase 4 (D5). Normalises the signature, binds its ids against the
/// argument types, and substitutes them back. Variadic signatures are left to
/// the regular filters.
pub fn instantiate_at_call(
    context: &Context,
    params: &[Type],
    ret: &Type,
    variadic: bool,
    arg_types: &[Type],
) -> CallInstance {
    if variadic || params.len() != arg_types.len() {
        return CallInstance::NotBounded;
    }
    // Cheap exit: a bound or a bare interface is always an alias/`Bounded` node.
    let mut candidate = false;
    for p in params {
        visit(p, &mut |t| candidate |= matches!(t, Type::Alias(..) | Type::Bounded(..)));
    }
    if !candidate {
        return CallInstance::NotBounded;
    }
    let args: Vec<ArgumentType> = params.iter().map(|t| ArgumentType::new("_", t)).collect();
    let normalized = normalize_signature(context, &args, ret);
    let norm_params: Vec<Type> = normalized.params.iter().map(|p| p.get_type()).collect();
    let declared = params.iter().any(|p| !bounded_occurrences(p).is_empty());
    let occurrences: Vec<String> = norm_params
        .iter()
        .flat_map(|p| bounded_occurrences(p))
        .map(|(id, _, _)| id)
        .collect();
    let shared = occurrences
        .iter()
        .any(|id| occurrences.iter().filter(|other| *other == id).count() > 1);
    if !declared && !shared {
        return CallInstance::NotBounded;
    }

    let mut binds = Vec::new();
    let mut clash = None;
    if !arg_types
        .iter()
        .zip(norm_params.iter())
        .enumerate()
        .all(|(i, (arg, param))| bind_ids(arg, param, i, context, &mut binds, &mut clash))
    {
        return match clash {
            Some(c) => CallInstance::Clash(c),
            None => CallInstance::Rejected,
        };
    }
    let mut subst = |t: &Type| match t {
        Type::Bounded(id, bound, _) => Some(
            binds
                .iter()
                .find(|(k, _, _)| k == id)
                .map(|(_, concrete, _)| concrete.clone())
                .unwrap_or_else(|| (**bound).clone()),
        ),
        _ => None,
    };
    let new_params = norm_params.iter().map(|p| rewrite(p, &mut subst)).collect();
    let new_ret = rewrite(&normalized.ret, &mut subst);
    CallInstance::Instantiated(new_params, new_ret)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processes::parsing::parse2;
    use crate::processes::type_checking::type_checker::TypeChecker;

    const DECLS: &str = "type Lovable <- interface { love: (Self) -> char };\n\
                         type Printable <- interface { show: (Self) -> char };";

    /// Type-checks the declarations then `fn_src`, and returns the error codes.
    fn codes(fn_src: &str) -> Vec<&'static str> {
        let tc1 = TypeChecker::new(Context::empty()).typing_no_panic(&parse2(DECLS.into()).unwrap());
        let tc2 = tc1.typing_no_panic(&parse2(fn_src.into()).unwrap());
        tc2.get_errors().iter().map(|e| e.code()).collect()
    }

    #[test]
    fn well_formed_signatures_are_accepted() {
        assert!(codes("fn(a: Lovable@A, b: Lovable@B): bool { true }").is_empty());
        // A shared id is well-formed; typing its body is phase 3.
        assert!(codes("fn(a: Lovable@A, b: Lovable@A): bool { true }").is_empty());
    }

    #[test]
    fn conflicting_bounds_are_rejected() {
        assert!(codes("fn(a: Lovable@A, b: Printable@A): bool { true }").contains(&"T047"));
    }

    #[test]
    fn id_colliding_with_a_free_generic_is_rejected() {
        assert!(codes("fn(a: T, b: Lovable@T): bool { true }").contains(&"T048"));
    }

    #[test]
    fn id_only_in_return_position_is_rejected() {
        assert!(codes("fn(a: Lovable@A): Lovable@B { a }").contains(&"T017"));
    }

    #[test]
    fn anonymous_ids_are_fresh_and_never_conflict() {
        assert!(codes("fn(a: Lovable@_, b: Printable@_): bool { true }").is_empty());
    }

    #[test]
    fn bare_interfaces_desugar_to_a_shared_id() {
        let tc = TypeChecker::new(Context::empty()).typing_no_panic(&parse2(DECLS.into()).unwrap());
        let ctx = tc.get_context();
        let h = crate::components::error_message::help_data::HelpData::default();
        let bare = ArgumentType::new("a", &Type::Alias("Lovable".into(), vec![], false, h.clone()));
        let out = normalize_signature(&ctx, &[bare.clone(), bare], &Type::Any(h));
        assert!(out
            .params
            .iter()
            .all(|p| matches!(p.get_type(), Type::Bounded(ref id, _, _) if id == "Lovable")));
    }

    const WORLD: &str = "type Cat <- list { name: char };\n\
                         type Dog <- list { age: int };\n\
                         let love <- fn(c: Cat): char { c.name };\n\
                         let love <- fn(d: Dog): char { \"dog\" };\n\
                         let cat <- Cat:{ name = \"tom\" };\n\
                         let dog <- Dog:{ age = 3 };\n";

    /// Error codes of the declarations, `fn_src` and `call_src` typed as one
    /// program (`parse2` silently drops what the CLI rejects).
    fn call_codes(fn_src: &str, call_src: &str) -> Vec<&'static str> {
        let program = format!("{}\n{}\n{}\n{}", DECLS, WORLD, fn_src, call_src);
        let tc = TypeChecker::new(Context::default())
            .typing_no_panic(&crate::processes::parsing::parse_from_string(&program, "t.ty"));
        tc.get_errors().iter().map(|e| e.code()).collect()
    }

    const TWO_IDS: &str = "let f <- fn(a: Lovable@A, b: Lovable@B): Lovable@B { b };";
    const ONE_ID: &str = "let g <- fn(a: Lovable@X, b: Lovable@X): char { a.love() };";
    const BARE: &str = "let h <- fn(a: Lovable, b: Lovable): char { a.love() };";

    #[test]
    fn distinct_ids_accept_different_types_and_return_the_right_one() {
        assert!(call_codes(TWO_IDS, "let r: Dog <- f(cat, dog);").is_empty());
        assert!(!call_codes(TWO_IDS, "let r: Cat <- f(cat, dog);").is_empty());
    }

    #[test]
    fn shared_id_rejects_different_types() {
        assert!(call_codes(ONE_ID, "g(cat, cat);").is_empty());
        assert!(!call_codes(ONE_ID, "g(cat, dog);").is_empty());
    }

    #[test]
    fn bare_interface_repeated_behaves_as_a_shared_id() {
        assert!(call_codes(BARE, "h(cat, cat);").is_empty());
        assert!(!call_codes(BARE, "h(cat, dog);").is_empty());
    }

    #[test]
    fn bounded_identity_chains() {
        let id = "let id <- fn(a: Lovable@A): Lovable@A { a };";
        assert!(call_codes(id, "let r: Cat <- id(id(cat));").is_empty());
    }

    #[test]
    fn id_clash_at_a_call_names_the_id_and_both_types() {
        assert_eq!(call_codes(ONE_ID, "g(cat, dog);"), vec!["T049"]);
    }

    #[test]
    fn swap_returns_the_ids_in_swapped_positions() {
        let swap = "let swap <- fn(a: Lovable@A, b: Lovable@B): tuple{Lovable@B, Lovable@A} { :{b, a} };";
        assert!(call_codes(swap, "let r: tuple{Dog, Cat} <- swap(cat, dog);").is_empty());
        assert!(!call_codes(swap, "let r: tuple{Cat, Dog} <- swap(cat, dog);").is_empty());
    }

    #[test]
    fn nested_bound_in_an_array_is_typed_through_its_rigid() {
        let first = "let first <- fn(xs: [#N, Lovable@A]): Lovable@A { xs[1] };\nlet cs <- [cat, cat];";
        assert!(call_codes(first, "let r: Cat <- first(cs);").is_empty());
        assert!(!call_codes(first, "let r: Dog <- first(cs);").is_empty());
    }

    #[test]
    fn bounded_is_equal_to_itself() {
        let h = crate::components::error_message::help_data::HelpData::default();
        let b = || Type::Bounded("A".into(), Box::new(Type::Alias("L".into(), vec![], false, h.clone())), h.clone());
        assert_eq!(b(), b());
    }
}
