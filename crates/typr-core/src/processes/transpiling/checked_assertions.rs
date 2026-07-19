//! `typr build --checked` (soundness_transpilation.md Phase A): wraps typed
//! transpilation boundaries (`let` annotations, function params/return,
//! constructor calls) in `typr_assert_type(...)` runtime checks. A no-op
//! when `Context::get_checked_mode()` is off, or when the boundary's type
//! has no reliable runtime class to check against (`Any`, unresolved
//! generics, interfaces, function types get a shallow `is.function` check).
//! Test-only oracle — never a production mode.

use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::Type;
use crate::processes::transpiling::escape_r_string;

/// Wrap `r_expr` (a transpiled R expression) in a `typr_assert_type(...)`
/// call checking it against `typ`, unless checked mode is off or `typ` has
/// no derivable runtime descriptor — in which case `r_expr` is returned
/// unchanged.
pub fn wrap_checked(context: &Context, r_expr: String, typ: &Type, loc: &HelpData, what: &str) -> String {
    if !context.get_checked_mode() {
        return r_expr;
    }
    match checked_descriptor(context, typ) {
        Some(descriptor) => format!(
            "typr_assert_type({}, {}, {}, {})",
            r_expr,
            descriptor,
            escape_r_string(&format_loc(loc)),
            escape_r_string(what)
        ),
        None => r_expr,
    }
}

/// Statement form for a function-body prologue: `Some("typr_assert_type(a,
/// ...)\n")`, or `None` when no assertion applies (nothing to emit — a bare
/// `a\n` no-op statement would just be noise).
pub fn param_assertion(context: &Context, param_name: &str, typ: &Type, loc: &HelpData) -> Option<String> {
    if !context.get_checked_mode() {
        return None;
    }
    let what = format!("param {}", param_name);
    let wrapped = wrap_checked(context, param_name.to_string(), typ, loc, &what);
    if wrapped == param_name {
        None
    } else {
        Some(format!("{}\n", wrapped))
    }
}

/// The R literal for `typr_assert_type`'s `expected` argument: either a
/// single `typeof`-style string (primitives, `is.function`), or a full
/// class vector `c(head, ...supertypes)` for named/record/union types —
/// mirrors the vector already generated for `as.X` casts (`get_class` +
/// `get_classes`), so it matches exactly what a properly-constructed value
/// of that type carries at runtime. `None` (pass-through, no assertion
/// emitted) for `Any`, unresolved generics, interfaces (structural, no
/// reliable runtime class), and any type whose class can't be resolved
/// (falls back to `'default'`/`'Any'` — asserting on that would produce
/// false positives, not a real check).
fn checked_descriptor(context: &Context, typ: &Type) -> Option<String> {
    match typ {
        Type::Integer(_, _) => Some("\"integer\"".to_string()),
        Type::Number(_, _) => Some("\"double\"".to_string()),
        Type::Char(_, _) => Some("\"character\"".to_string()),
        Type::Boolean(_, _) => Some("\"logical\"".to_string()),
        Type::Function(_, _, _) => Some("\"function\"".to_string()),
        Type::Any(_)
        | Type::Generic(_, _)
        | Type::IndexGen(_, _)
        | Type::LabelGen(_, _)
        | Type::KindedGen(_, _, _)
        | Type::Variable(_, _)
        | Type::Interface(_, _)
        | Type::UnknownFunction(_)
        | Type::Failed(_, _)
        | Type::Empty(_) => None,
        // `Foreign<T>`-family aliases (soundness_transpilation.md Phase D)
        // name genuinely external R values (S4/R6/RC instances, third-party
        // S3 objects, …) whose real runtime class TypR never controls and
        // must not assert on — same rationale as `get_type_anotation`
        // skipping the `as.X` cast for them (`vartype.rs`).
        Type::Alias(name, _, _, _) if context.resolves_to_foreign_alias(name) => None,
        _ => {
            // Atomic-representation arrays (step ③, unification_arrays.md)
            // are bare R atomic vectors: assert on `typeof`, exactly like the
            // scalar primitives above (the `typr_assert_type` `is.*` checks
            // are vector-safe). Composite (typed_vec) arrays fall through to
            // the class-vector check below.
            if let Some(elem) = context.atomic_array_elem(typ) {
                let descriptor = match elem {
                    Type::Integer(_, _) => "\"integer\"",
                    Type::Char(_, _) => "\"character\"",
                    Type::Boolean(_, _) => "\"logical\"",
                    _ => "\"double\"",
                };
                return Some(descriptor.to_string());
            }
            let head = context.get_class(typ);
            // `get_class`'s fallback for anything it can't resolve to a
            // registered alias is a literal `'default'`/`'Any'` — that's a
            // dispatch suffix, not a class any real value carries, so
            // asserting on it would be a guaranteed false positive.
            if head == "'default'" || head == "'Any'" {
                return None;
            }
            let rest = context.get_classes(typ).unwrap_or_else(|| "'None'".to_string());
            Some(format!("c({}, {})", head, rest))
        }
    }
}

/// `"file.ty:12"` when the line can be recovered from the registered
/// source, `"file.ty:offset <n>"` otherwise (synthetic/desugared nodes with
/// no cached source still get a usable, if coarser, location).
fn format_loc(loc: &HelpData) -> String {
    let file = loc.get_file_name();
    if file.is_empty() {
        return "<unknown>".to_string();
    }
    let line = loc.get_file_data().and_then(|(_, content)| {
        let offset = loc.get_offset().min(content.len());
        content.get(..offset).map(|prefix| prefix.matches('\n').count() + 1)
    });
    match line {
        Some(line) => format!("{}:{}", file, line),
        None => format!("{}:offset {}", file, loc.get_offset()),
    }
}
