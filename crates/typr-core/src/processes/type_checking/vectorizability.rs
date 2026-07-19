#![allow(dead_code, unused_variables, unused_imports)]
//! Static vectorizability analysis of a user function's body (step 2 of the
//! vector/array unification plan): decides at declaration time whether the
//! transpiled R body is safe to call directly on an atomic vector — i.e. it
//! is built only out of operations R itself applies element-wise — or whether
//! a `Vec[N, T]` call site must keep the explicit `vapply` lift emitted by
//! the `Lang::VecFunctionApp` transpilation arm (step 1). Direct calls run at
//! native C speed (measured 20–100× faster than the closure-based lift), so
//! this is purely an optimization: the predicate must stay *conservative* —
//! a false `false` costs speed, a false `true` breaks soundness (e.g. any
//! `if` on a length > 1 condition is a fatal R error since 4.2).

use crate::components::context::Context;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;

/// Base-R functions whose R implementation is element-wise over atomic
/// vectors (NOT reductions like `max`/`min`/`sum`, and NOT scalar-only
/// helpers). A call to one of these inside a body keeps the body
/// vectorizable.
const NATIVELY_VECTORIZED_BUILTINS: &[&str] = &[
    "sqrt", "abs", "exp", "log", "log2", "log10", "log1p", "expm1", "sin", "cos", "tan", "asin", "acos", "atan",
    "sinh", "cosh", "tanh", "floor", "ceiling", "round", "trunc", "sign",
];

/// Binary operators that R applies element-wise (with scalar recycling).
/// `&&`/`||` (`Op::And2`/`Op::Or2`) are deliberately absent: they are
/// scalar-only in R (length > 1 operands are an error since 4.3).
fn is_vectorized_op(op: &Op) -> bool {
    matches!(
        op,
        Op::Add(_)
            | Op::Minus(_)
            | Op::Mul(_)
            | Op::Div(_)
            | Op::Modulo(_)
            | Op::Eq(_)
            | Op::NotEq(_)
            | Op::LesserThan(_)
            | Op::GreaterThan(_)
            | Op::LesserOrEqual(_)
            | Op::GreaterOrEqual(_)
            | Op::And(_)
            | Op::Or(_)
    )
}

/// Call-site variant of the whitelist above (step ③): is `name` a callee R
/// itself applies element-wise over atomic vectors — an element-wise binary
/// operator symbol or a natively vectorized builtin? Such a lifted call is
/// emitted as a direct call (native C speed) instead of a `vapply` lift.
pub fn is_natively_vectorized_callee(name: &str) -> bool {
    // Operator names reach the call site backtick-quoted (`` `*` ``, from
    // the ``@`*`:`` signature syntax) — match on the bare symbol.
    let name = name.trim_matches('`');
    NATIVELY_VECTORIZED_BUILTINS.contains(&name)
        || matches!(
            name,
            "+" | "-" | "*" | "/" | "%%" | "^" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&" | "|" | "!"
        )
}

fn is_primitive_scalar(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Integer(_, _) | Type::Number(_, _) | Type::Boolean(_, _) | Type::Char(_, _)
    )
}

/// True when `expr` — a statement inside the body's `Lang::Scope` — keeps the
/// body element-wise: either a plain vectorizable expression, or an
/// *un-annotated* local `let` binding a vectorizable expression (an annotated
/// `let` transpiles with a cast/annotation wrapper written for a scalar
/// value, so it is excluded).
fn is_vectorizable_statement(context: &Context, stmt: &Lang) -> bool {
    match stmt {
        Lang::Let { r#type, expression, .. } => r#type.is_empty() && is_vectorizable_expr(context, expression),
        _ => is_vectorizable_expr(context, stmt),
    }
}

fn is_vectorizable_expr(context: &Context, expr: &Lang) -> bool {
    match expr {
        Lang::Integer { .. } | Lang::Number { .. } | Lang::Bool { .. } | Lang::Char { .. } => true,
        Lang::Variable { .. } => true,
        // R's `!` is element-wise.
        Lang::Not { value, .. } => is_vectorizable_expr(context, value),
        Lang::Operator { operator, lhs, rhs, .. } => {
            is_vectorized_op(operator) && is_vectorizable_expr(context, lhs) && is_vectorizable_expr(context, rhs)
        }
        Lang::Scope { body, .. } => {
            !body.is_empty() && body.iter().all(|stmt| is_vectorizable_statement(context, stmt))
        }
        Lang::FunctionApp {
            identifier, arguments, ..
        } => {
            let callee_ok = Var::from_language(identifier.as_ref().clone())
                .map(|var| {
                    let name = var.get_name();
                    NATIVELY_VECTORIZED_BUILTINS.contains(&name.as_str()) || context.is_vectorizable_fn(&name)
                })
                .unwrap_or(false);
            callee_ok && arguments.iter().all(|arg| is_vectorizable_expr(context, arg))
        }
        // Everything else — `if`/`match` (scalar-only control flow),
        // indexation, pipes/dot/dollar access, loops, tags, records,
        // lambdas, `return`, ... — is not known element-wise.
        _ => false,
    }
}

/// Predicate applied at declaration time to a `Lang::Function` RHS: the call
/// site may replace the `vapply` lift with a direct call only when every
/// parameter and the return type are primitive scalars (the only shapes a
/// `Vec[N, T]` lift produces) and the body is element-wise throughout.
pub fn is_vectorizable_function(
    context: &Context,
    parameters: &[ArgumentType],
    return_type: &Type,
    body: &Lang,
) -> bool {
    !parameters.is_empty()
        && parameters.iter().all(|p| is_primitive_scalar(&p.get_type()))
        && is_primitive_scalar(return_type)
        && is_vectorizable_expr(context, body)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::fluent_parser::FluentParser;

    fn context_after(decl: &str) -> Context {
        FluentParser::new().push(decl).run().get_context()
    }

    #[test]
    fn arithmetic_body_is_vectorizable() {
        let ctx = context_after("let double <- fn(x: int): int { x * 2 };");
        assert!(ctx.is_vectorizable_fn("double"));
    }

    #[test]
    fn if_body_is_not_vectorizable() {
        let ctx = context_after("let is_small <- fn(n: int): bool { if (n < 3) { true } else { false } };");
        assert!(!ctx.is_vectorizable_fn("is_small"));
    }

    #[test]
    fn comparison_and_logical_ops_are_vectorizable() {
        let ctx = context_after("let in_range <- fn(x: int): bool { (x > 0) && (x < 10) };");
        // `&&` is scalar-only in R — must NOT be considered vectorizable.
        assert!(!ctx.is_vectorizable_fn("in_range"));
        let ctx2 = context_after("let in_range2 <- fn(x: int): bool { (x > 0) & (x < 10) };");
        assert!(ctx2.is_vectorizable_fn("in_range2"));
    }

    #[test]
    fn local_unannotated_let_is_vectorizable() {
        let ctx = context_after("let poly <- fn(x: num): num { let y <- x * 2.0; y + 1.0 };");
        assert!(ctx.is_vectorizable_fn("poly"));
    }

    #[test]
    fn call_to_vectorizable_user_fn_is_vectorizable() {
        let ctx = FluentParser::new()
            .push("let double <- fn(x: int): int { x * 2 };")
            .run()
            .push("let quad <- fn(x: int): int { double(double(x)) };")
            .run()
            .get_context();
        assert!(ctx.is_vectorizable_fn("quad"));
    }

    #[test]
    fn call_to_non_vectorizable_user_fn_is_not_vectorizable() {
        let ctx = FluentParser::new()
            .push("let is_small <- fn(n: int): bool { if (n < 3) { true } else { false } };")
            .run()
            .push("let wrap <- fn(n: int): bool { is_small(n) };")
            .run()
            .get_context();
        assert!(!ctx.is_vectorizable_fn("wrap"));
    }

    #[test]
    fn non_vectorizable_overload_downgrades_the_name() {
        let ctx = FluentParser::new()
            .push("let f <- fn(x: int): int { x * 2 };")
            .run()
            .push("let f <- fn(x: num): num { if (x > 0.0) { x } else { 0.0 } };")
            .run()
            .get_context();
        assert!(!ctx.is_vectorizable_fn("f"));
    }

    #[test]
    fn record_parameter_is_not_vectorizable() {
        let ctx = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run()
            .push("let getx <- fn(p: Point): int { 1 };")
            .run()
            .get_context();
        assert!(!ctx.is_vectorizable_fn("getx"));
    }
}
