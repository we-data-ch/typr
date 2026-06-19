//! Named type embedding (RFC: `type_embedding.md`).
//!
//! A record field prefixed with `embed`, e.g. `embed field: B` (parsed as
//! `ArgumentType::is_embedded()`, see `components/type/argument_type.rs` and the
//! `embed_keyword` parser in `processes/parsing/types.rs`), makes the enclosing
//! record type `A` automatically inherit every monomorphic function
//! `f(self: B, args...): B` defined on `B`, via a generated forwarding +
//! reconstruction function:
//!
//! ```text
//! let f <- fn(self: A, args...): A { A:{ field = f(self$field, args...), ...self } };
//! ```
//!
//! Forwarding candidates are discovered with the same structural lookup that
//! powers `Type::to_interface` (`Context::get_functions_from_type`), so this reuses
//! the exact mechanism that already answers "what functions does this type have".
//! Generated functions are rendered as real TypR source and parsed with the
//! top-level statement parser (`processes::parsing::parse2` — `Lang::from_str`
//! only covers single expressions, not `let` statements), then run through the
//! normal `typing()` entry point — they get validated, registered in the
//! context, and later transpiled exactly like hand-written functions (no new
//! transpiler code is needed).
//!
//! See `type_embedding.md` §12 for the v1 scope (monomorphic only, no refined
//! types, no variadics, direct record-literal fields only).

use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;
use crate::processes::parsing::parse2;
use crate::processes::type_checking::type_arithmetic::accepts_record_kind;
use crate::processes::type_checking::typing;
use std::collections::HashMap;
use std::collections::HashSet;

/// Extract `(field_name, embedded_type)` pairs for every `embed field: Type` in
/// a record's field set.
fn embedded_fields(fields: &HashSet<ArgumentType>) -> Vec<(String, Type)> {
    fields
        .iter()
        .filter(|f| f.is_embedded())
        .map(|f| (f.get_argument_str(), f.get_type()))
        .collect()
}

/// Render the TypR source for one auto-generated forwarding function. Returns
/// `None` for shapes out of v1 scope (no parameters, or a variadic parameter
/// after the first — see §12.1/§12.3 of the RFC).
fn render_forwarding_fn(
    a_name: &str,
    field_name: &str,
    method_name: &str,
    fn_type: &Type,
) -> Option<String> {
    let Type::Function(params, _ret, _) = fn_type else {
        return None;
    };
    let (first, rest) = params.split_first()?;
    if rest.iter().any(|p| p.is_variadic()) {
        return None;
    }
    let first_name = first.get_argument_str();
    let mut sig_parts = vec![format!("{}: {}", first_name, a_name)];
    // `$` is the record field-access operator in TypR (`Op::Dollar`); `.` is
    // reserved for UFCS function-call sugar (`Op::Dot`) and would not resolve to
    // a field here.
    let mut call_args = vec![format!("{}${}", first_name, field_name)];
    for p in rest {
        let name = p.get_argument_str();
        sig_parts.push(format!("{}: {}", name, p.get_type()));
        call_args.push(name);
    }
    Some(format!(
        "let {method} <- fn({sig}): {a} {{ {a}:{{ {field} = {method}({args}), ...{first} }} }};",
        method = method_name,
        sig = sig_parts.join(", "),
        a = a_name,
        field = field_name,
        args = call_args.join(", "),
        first = first_name,
    ))
}

/// For the record type `a_name` with field set `fields`, discover every function
/// inherited through `@Type`-embedded fields, generate the corresponding
/// forwarding `Lang::Let` functions, type-check them, and register their
/// provenance in the returned `Context` (for E-EMBED-003 detection in
/// `let_expression.rs`).
///
/// Returns the synthesized `Lang` nodes (already type-checked), the resulting
/// `Context`, and any embedding-specific errors (E-EMBED-001/002).
pub fn synthesize_embedding(
    context: &Context,
    a_name: &str,
    fields: &HashSet<ArgumentType>,
    h: &HelpData,
) -> (Vec<Lang>, Context, Vec<TypRError>) {
    let embeds = embedded_fields(fields);
    if embeds.is_empty() {
        return (Vec::new(), context.clone(), Vec::new());
    }

    let mut errors = Vec::new();

    // (method_name, field_name, fn_type) candidates, after the kind (E-EMBED-002)
    // and "returns exactly B" (RFC §5.1) checks.
    let mut candidates: Vec<(String, String, Type)> = Vec::new();
    for (field_name, b_type) in &embeds {
        if !accepts_record_kind(b_type) {
            errors.push(TypRError::type_error(TypeError::EmbedNonRecord(
                field_name.clone(),
                b_type.clone(),
                h.clone(),
            )));
            continue;
        }
        for (var, fn_type) in context.get_functions_from_type(b_type) {
            if let Type::Function(_, ret, _) = &fn_type {
                if ret.as_ref() == b_type {
                    candidates.push((var.get_name(), field_name.clone(), fn_type));
                }
            }
        }
    }

    // E-EMBED-001: the same method name reachable from more than one distinct
    // embedded field is a collision.
    let mut fields_by_method: HashMap<String, Vec<String>> = HashMap::new();
    for (method, field, _) in &candidates {
        let entry = fields_by_method.entry(method.clone()).or_default();
        if !entry.contains(field) {
            entry.push(field.clone());
        }
    }
    let colliding: HashSet<&String> = fields_by_method
        .iter()
        .filter(|(_, fs)| fs.len() > 1)
        .map(|(name, _)| name)
        .collect();
    for name in &colliding {
        errors.push(TypRError::type_error(TypeError::EmbedCollision(
            (*name).clone(),
            fields_by_method[*name].clone(),
            h.clone(),
        )));
    }

    let mut acc_context = context.clone();
    let mut synthesized = Vec::new();
    let mut generated: HashSet<String> = HashSet::new();
    for (method_name, field_name, fn_type) in candidates {
        if colliding.contains(&method_name) || !generated.insert(method_name.clone()) {
            continue;
        }
        let Some(src) = render_forwarding_fn(a_name, &field_name, &method_name, &fn_type) else {
            continue;
        };
        let parsed: Lang = match parse2(src.as_str().into()) {
            Ok(lang) => lang,
            // Defensive: the template above always produces valid TypR source for
            // the (non-variadic, ≥1 parameter) shapes it accepts.
            Err(_) => continue,
        };
        let tc = typing(&acc_context, &parsed);
        errors.extend(tc.errors.clone());
        acc_context = tc.context.push_embedded_method(
            a_name.to_string(),
            method_name.clone(),
            field_name.clone(),
        );
        synthesized.push(tc.lang);
    }

    (synthesized, acc_context, errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processes::type_checking::type_checker::TypeChecker;

    /// Parse and type-check `src` as a sequence of top-level statements,
    /// threading context across them exactly like a real source file.
    fn run_program(src: &[&str]) -> TypeChecker {
        let lines: Vec<Lang> = src
            .iter()
            .map(|s| {
                parse2((*s).into()).unwrap_or_else(|e| panic!("parse error in {:?}: {}", s, e))
            })
            .collect();
        let program = Lang::Lines {
            value: lines,
            help_data: HelpData::default(),
        };
        TypeChecker::new(Context::default()).typing_no_panic(&program)
    }

    const POSITION_AND_MOVE: [&str; 2] = [
        "type Position <- list { x: int, y: int };",
        "let move <- fn(self: Position, dx: int, dy: int): Position { Position:{ x = self$x + dx, y = self$y + dy } };",
    ];

    #[test]
    fn test_happy_path_generates_forwarding_method() {
        let mut src = POSITION_AND_MOVE.to_vec();
        src.push("type Player <- list { embed coords: Position, name: char };");
        let tc = run_program(&src);

        assert!(!tc.has_errors(), "unexpected errors: {:?}", tc.get_errors());
        let r = tc.transpile();
        assert!(
            r.contains("move.Player"),
            "expected a generated `move.Player` forwarding method, got:\n{}",
            r
        );
    }

    #[test]
    fn test_e_embed_002_non_record_embedded_type_is_an_error() {
        let tc = run_program(&["type Bad <- list { embed n: int };"]);

        assert!(tc.has_errors());
        assert!(
            tc.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::EmbedNonRecord(..)))),
            "expected EmbedNonRecord, got: {:?}",
            tc.get_errors()
        );
    }

    #[test]
    fn test_e_embed_001_collision_between_two_embedded_fields() {
        let mut src = POSITION_AND_MOVE.to_vec();
        src.extend([
            "type Speed <- list { x: int, y: int };",
            "let move <- fn(self: Speed, dx: int, dy: int): Speed { Speed:{ x = self$x + dx, y = self$y + dy } };",
            "type Player <- list { embed coords: Position, embed vel: Speed };",
        ]);
        let tc = run_program(&src);

        assert!(tc.has_errors());
        assert!(
            tc.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::EmbedCollision(..)))),
            "expected EmbedCollision, got: {:?}",
            tc.get_errors()
        );
    }

    #[test]
    fn test_e_embed_003_conflict_with_later_explicit_definition() {
        let mut src = POSITION_AND_MOVE.to_vec();
        src.extend([
            "type Player <- list { embed coords: Position, name: char };",
            "let move <- fn(self: Player, dx: int, dy: int): Player { Player:{ coords = move(self$coords, dx, dy), ...self } };",
        ]);
        let tc = run_program(&src);

        assert!(tc.has_errors());
        assert!(
            tc.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::EmbedConflict(..)))),
            "expected EmbedConflict, got: {:?}",
            tc.get_errors()
        );
    }
}
