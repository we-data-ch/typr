//! Boundary check for refined types (`refined_types_plan.md`, Phase 5, D3/D5).
//!
//! `is_subtype` only ever answers `true` for a *proven* refinement. What it
//! cannot prove (`[int]` where `[3, int]` is expected, `int` where
//! `int & (> 0)` is expected) is neither accepted nor rejected there: the
//! boundary (`let` annotation, argument, return) asks [`coerce_to`], which
//! separates the three outcomes and returns, for the unproven case, the
//! residual the runtime has to check.

use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::refinement::RefinementSet;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;

/// Outcome of comparing what a boundary found with what it expects.
#[derive(Debug, Clone, PartialEq)]
pub enum Coercion {
    /// `found <: expected` is proven: nothing to check.
    Static,
    /// The base types agree and the refinements are compatible but not
    /// proven. The set is the part of `expected` that `found` does not imply.
    Runtime(RefinementSet),
    /// A plain type mismatch, or a refinement that provably cannot hold.
    Reject,
}

/// `t` with every refinement removed, including the length index of a `Vec`
/// (`[3, int]` gives `[any, int]`): what is left is the type the refinements
/// are properties *of*.
fn base_of(t: &Type) -> Type {
    match t {
        Type::Refined(base, _, _) => base_of(base),
        Type::Vec(_, _, _, h) => t.with_vec_length(Type::Any(h.clone())),
        t => t.clone(),
    }
}

/// Decide how `found` may flow into `expected` (both reduced here).
pub fn coerce_to(found: &Type, expected: &Type, context: &Context) -> Coercion {
    let found = found.reduce(context);
    let expected = expected.reduce(context);

    if found.is_subtype(&expected, context).0 {
        return Coercion::Static;
    }
    if !base_of(&found).is_subtype(&base_of(&expected), context).0 {
        return Coercion::Reject;
    }

    let have = found.refinements_of();
    let want = expected.refinements_of();
    // A vector's length is always integral; scalars follow their base type.
    let integral = matches!(base_of(&expected).unrefined(), Type::Integer(..));
    if have.contradicts(&want, integral) {
        return Coercion::Reject;
    }

    let residual = residual(&have, &want);
    if residual.is_trivial() {
        // The refinements are not what blocked the subtype (an index that is
        // not a literal, say): nothing runtime can repair that.
        Coercion::Reject
    } else {
        Coercion::Runtime(residual)
    }
}

/// The name of a record field, when it is a plain literal.
fn field_name(arg: &ArgumentType) -> Option<String> {
    match &arg.0 {
        Type::Char(Tchar::Val(name), _) => Some(name.to_string()),
        _ => None,
    }
}

/// Runtime obligations for a record literal flowing into a record type
/// (`let a: Account <- list { id: n, .. }`), one per *field expression*: the
/// literal is built right there, so each field is a boundary of its own and
/// the check sits on the value that needs it, not on the whole record.
///
/// `None` when `expr` is not a spread-free record literal, when the expected
/// type is not a record, when a field is missing or provably wrong, or when
/// no field needs a check (the caller then keeps its own verdict).
pub fn field_obligations(
    expr: &Lang,
    found: &Type,
    expected: &Type,
    context: &Context,
) -> Option<Vec<(HelpData, RefinementSet)>> {
    let Lang::List { value: fields, spreads, .. } = expr else { return None };
    if !spreads.is_empty() {
        return None;
    }
    let (Type::Record(have, _), Type::Record(want, _)) = (found.reduce(context), expected.reduce(context)) else {
        return None;
    };
    let mut out = Vec::new();
    for w in want.iter() {
        let name = field_name(w)?;
        let value = fields.iter().find(|f| f.get_argument() == name)?.get_value();
        let h = have.iter().find(|a| field_name(a).as_deref() == Some(name.as_str()))?;
        match coerce_to(&h.1, &w.1, context) {
            Coercion::Static => {}
            Coercion::Runtime(set) => out.push((value.get_help_data(), set)),
            // a nested literal is a boundary in turn
            Coercion::Reject => out.extend(field_obligations(&value, &h.1, &w.1, context)?),
        }
    }
    (!out.is_empty()).then_some(out)
}

/// `context` with each obligation of `field_obligations` recorded.
pub fn with_obligations(context: Context, obligations: Vec<(HelpData, RefinementSet)>) -> Context {
    obligations.into_iter().fold(context, |ctx, (h, set)| ctx.add_refinement_obligation(&h, set))
}

/// The measures of `want` that `have` does not already imply.
pub(crate) fn residual(have: &RefinementSet, want: &RefinementSet) -> RefinementSet {
    want.iter()
        .filter(|(m, iv)| !have.get(*m).map(|h| h.implies(iv)).unwrap_or(false))
        .fold(RefinementSet::empty(), |acc, (m, iv)| acc.with(*m, *iv))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::fluent_parser::FluentParser;

    fn ty(src: &str) -> (Type, Context) {
        let ctx = FluentParser::new().push(&format!("type T <- {};", src)).run().context.clone();
        let alias = Type::Alias("T".into(), vec![], false, HelpData::default());
        (alias.reduce(&ctx), ctx)
    }

    #[test]
    fn unproven_length_is_a_runtime_obligation() {
        let (found, ctx) = ty("[int]");
        let (expected, _) = ty("[3, int]");
        match coerce_to(&found, &expected, &ctx) {
            Coercion::Runtime(set) => assert_eq!(set.to_string(), "length(3)"),
            other => panic!("expected Runtime, got {:?}", other),
        }
    }

    #[test]
    fn proven_length_needs_no_check() {
        let (found, ctx) = ty("[3, int]");
        let (expected, _) = ty("[int]");
        assert_eq!(coerce_to(&found, &expected, &ctx), Coercion::Static);
        let (same, _) = ty("[3, int]");
        assert_eq!(coerce_to(&found, &same, &ctx), Coercion::Static);
    }

    #[test]
    fn contradicting_length_is_rejected() {
        let (found, ctx) = ty("[3, int]");
        let (expected, _) = ty("[5, int]");
        assert_eq!(coerce_to(&found, &expected, &ctx), Coercion::Reject);
    }

    #[test]
    fn base_mismatch_is_rejected() {
        let (found, ctx) = ty("[char]");
        let (expected, _) = ty("[3, int]");
        assert_eq!(coerce_to(&found, &expected, &ctx), Coercion::Reject);
    }

    #[test]
    fn scalar_needs_a_check_for_a_value_bound() {
        let (found, ctx) = ty("int");
        let (expected, _) = ty("int & (> 0)");
        match coerce_to(&found, &expected, &ctx) {
            Coercion::Runtime(set) => assert_eq!(set.to_string(), "(> 0)"),
            other => panic!("expected Runtime, got {:?}", other),
        }
    }

    #[test]
    fn refined_implies_weaker_bound() {
        let (found, ctx) = ty("int & (> 5)");
        let (expected, _) = ty("int & (> 0)");
        assert_eq!(coerce_to(&found, &expected, &ctx), Coercion::Static);
        let (tighter, _) = ty("int & (> 10)");
        assert!(matches!(coerce_to(&found, &tighter, &ctx), Coercion::Runtime(_)));
    }

    fn errors_of(src: &str) -> usize {
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::typing_with_errors;
        { let r = typing_with_errors(&Context::default(), &crate::processes::parsing::parse_from_string(src, "test")); r.get_errors().len() }
    }

    #[test]
    fn return_position_accepts_unproven_and_rejects_contradiction() {
        assert_eq!(errors_of("let f <- fn(v: [int]): [3, int] { v };"), 0);
        assert_eq!(errors_of("let f <- fn(v: [int], c: bool): [3, int] { if (c) { return v; }; [1, 2, 3] };"), 0);
        assert!(errors_of("let f <- fn(): [3, int] { [1, 2] };") > 0);
        assert!(errors_of("let f <- fn(v: [char]): [3, int] { v };") > 0);
    }

    #[test]
    fn argument_position_accepts_unproven_and_rejects_contradiction() {
        let decl = "let g <- fn(p: [2, num]): num { 1.0 };";
        assert_eq!(errors_of(&format!("{} let u: [num] <- [1.0, 2.0]; let r <- g(u);", decl)), 0);
        assert!(errors_of(&format!("{} let c: [bool] <- [true, false]; let r <- g(c);", decl)) > 0);
    }
}
