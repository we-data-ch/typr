//! `Lang::Match`: pattern matching over tags, type patterns, tuples, and
//! record literals. Each branch pattern determines what bindings (if any) it
//! adds to that branch's typing context before the branch body is typed.
use crate::components::context::Context;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::Type;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::type_checking::typing;
use crate::utils::builder;

/// Recursively search a (possibly-union) type for the `Type::Tag` variant
/// named `tag_name`, returning its payload type. Mirrors the tree shape built
/// by `builder::union_type`/`TypeOperator::build_type`: a left-associative
/// binary tree of `Type::Operator(Union, ...)` nodes with `Type::Tag` leaves.
/// Works uniformly for user unions and the stdlib `Option<T>` alike, once
/// `match_type` has already been through `reduce_type` (which substitutes any
/// alias generics and unwraps the alias down to that tree).
fn find_tag_variant(typ: &Type, tag_name: &str) -> Option<Type> {
    match typ {
        Type::Tag(name, inner, _) if name == tag_name => Some((**inner).clone()),
        Type::Operator(TypeOperator::Union, t1, t2, _) => {
            find_tag_variant(t1, tag_name).or_else(|| find_tag_variant(t2, tag_name))
        }
        _ => None,
    }
}

/// Enumerate every `(tag_name, has_payload)` pair reachable from `typ`'s
/// union tree (empty if `typ` isn't tag/union-shaped at all).
fn collect_tag_variants(typ: &Type, out: &mut Vec<(String, bool)>) {
    match typ {
        Type::Tag(name, inner, _) => {
            out.push((name.clone(), !matches!(inner.as_ref(), Type::Empty(_))));
        }
        Type::Operator(TypeOperator::Union, t1, t2, _) => {
            collect_tag_variants(t1, out);
            collect_tag_variants(t2, out);
        }
        _ => {}
    }
}

/// Builds the branch-local context for one `match` arm: binds whatever
/// variables `pattern` introduces, using `match_type` (the scrutinee's
/// *already-reduced* type) to resolve their types where possible. Any shape
/// this checker can't make sense of pushes an explicit error into `errors`
/// rather than silently dropping bindings or falling back to `Any`.
fn build_match_branch_context(
    context: &Context,
    pattern: &Lang,
    match_type: &Type,
    errors: &mut Vec<TypRError>,
) -> Context {
    match pattern {
        // For tag patterns with bindings, we extract the inner type
        Lang::Tag {
            name: tag_name,
            value: inner,
            help_data,
        } => {
            let found = find_tag_variant(match_type, tag_name);
            // Only raise "unknown variant" against a genuine multi-variant
            // union (function param/annotated `let` typed with the full
            // alias): a bare, single `Type::Tag` scrutinee is how TypR infers
            // an *unannotated* `let x <- .Circle(3);` — it only carries the
            // one concrete variant the literal happened to have, not the
            // declared alias's full variant set, so a different tag name
            // there isn't necessarily wrong (it's the pre-existing type
            // narrowing behavior, not a match_expression bug).
            if found.is_none() && matches!(match_type, Type::Operator(TypeOperator::Union, ..)) {
                errors.push(TypRError::Type(TypeError::PatternTypeMismatch(
                    format!(".{}(...)", tag_name),
                    match_type.clone(),
                    help_data.clone(),
                )));
            }
            let inner_type = found.unwrap_or_else(builder::any_type);
            match inner.as_ref() {
                Lang::Variable { name: var_name, .. } if var_name == "_" => context.clone(),
                Lang::Variable { name: var_name, .. } => {
                    let var = Var::from_name(var_name);
                    context.clone().push_var_type(var, inner_type, context)
                }
                Lang::Empty(_) => context.clone(),
                other => {
                    errors.push(TypRError::Type(TypeError::UnsupportedPattern(
                        format!("nested pattern inside `.{}(...)`", tag_name),
                        other.get_help_data(),
                    )));
                    context.clone()
                }
            }
        }
        // For type patterns `x as int`, bind variable to that type
        Lang::TypePattern {
            variable_name: var_name,
            matched_type: typ,
            ..
        } => {
            let var = Var::from_name(var_name);
            context.clone().push_var_type(var, typ.clone(), context)
        }
        // For tuple patterns `:{a, b, c}`, bind each variable by position
        Lang::Tuple {
            value: elements,
            help_data,
        } => {
            let tuple_types = match match_type {
                Type::Tuple(types, _) => Some(types.clone()),
                _ => None,
            };
            if tuple_types.is_none() && !matches!(match_type, Type::Any(_)) {
                errors.push(TypRError::Type(TypeError::PatternTypeMismatch(
                    "tuple pattern".to_string(),
                    match_type.clone(),
                    help_data.clone(),
                )));
            }
            elements.iter().enumerate().fold(
                context.clone(),
                |ctx: Context, (i, elem)| match elem {
                    Lang::Variable { name: var_name, .. } if var_name == "_" => ctx,
                    Lang::Variable { name: var_name, .. } => {
                        let elem_type = tuple_types
                            .as_ref()
                            .and_then(|types| types.get(i).cloned())
                            .unwrap_or_else(builder::any_type);
                        let var = Var::from_name(var_name);
                        ctx.push_var_type(var, elem_type, context)
                    }
                    other => {
                        errors.push(TypRError::Type(TypeError::UnsupportedPattern(
                            "nested pattern inside tuple pattern".to_string(),
                            other.get_help_data(),
                        )));
                        ctx
                    }
                },
            )
        }
        // For list/record patterns `:{nom: n, age: a}`, bind each variable
        Lang::List {
            value: fields,
            help_data,
            ..
        } => {
            // Extract record field types from the matched expression if available
            let record_fields = match match_type {
                Type::Record(field_types, _) => Some(field_types.clone()),
                _ => None,
            };
            if record_fields.is_none() && !matches!(match_type, Type::Any(_)) {
                errors.push(TypRError::Type(TypeError::PatternTypeMismatch(
                    "record pattern".to_string(),
                    match_type.clone(),
                    help_data.clone(),
                )));
            }
            fields
                .iter()
                .fold(
                    context.clone(),
                    |ctx: Context, arg_val: &ArgumentValue| match &arg_val.get_value() {
                        Lang::Variable { name: var_name, .. } => {
                            let field_type = record_fields
                                .as_ref()
                                .and_then(|fields| {
                                    fields
                                        .iter()
                                        .find(|at| at.get_argument_str() == arg_val.get_argument())
                                })
                                .map(|at| at.get_type())
                                .unwrap_or_else(builder::any_type);
                            let var = Var::from_name(var_name);
                            ctx.push_var_type(var, field_type, context)
                        }
                        other => {
                            errors.push(TypRError::Type(TypeError::UnsupportedPattern(
                                "nested pattern inside record pattern".to_string(),
                                other.get_help_data(),
                            )));
                            ctx
                        }
                    },
                )
        }
        // Wildcard `_` binds nothing; a named catch-all binds the whole
        // scrutinee type to that name.
        Lang::Variable { name: var_name, .. } if var_name == "_" => context.clone(),
        Lang::Variable { name: var_name, .. } => {
            let var = Var::from_name(var_name);
            context
                .clone()
                .push_var_type(var, match_type.clone(), context)
        }
        _ => context.clone(),
    }
}

pub fn match_expression(
    context: &Context,
    expr: &Lang,
    match_exp: &Lang,
    branches: &[(Lang, Box<Lang>)],
) -> TypeContext {
    let match_tc = typing(context, match_exp);
    let mut errors = match_tc.errors;
    let match_type = reduce_type(context, &match_tc.value);

    let branch_tcs: Vec<_> = branches
        .iter()
        .map(|(pattern, bexp)| {
            let mut pattern_errors = Vec::new();
            let new_context =
                build_match_branch_context(context, pattern, &match_type, &mut pattern_errors);
            let mut tc = typing(&new_context, bexp);
            tc.errors.extend(pattern_errors);
            tc
        })
        .collect();

    errors.extend(branch_tcs.iter().flat_map(|tc| tc.errors.clone()));

    // Exhaustiveness (M1): only meaningful when the scrutinee actually
    // resolves to a genuine multi-variant union (see the comment on the
    // `PatternTypeMismatch` check above for why a bare single `Type::Tag`
    // scrutinee must be excluded), and only when no catch-all pattern
    // (`_ => ...` or `name => ...`) already makes every case reachable.
    let has_catch_all = branches
        .iter()
        .any(|(pattern, _)| matches!(pattern, Lang::Variable { .. }));
    if !has_catch_all {
        let mut variants = Vec::new();
        collect_tag_variants(&match_type, &mut variants);
        if variants.len() > 1 {
            let existing: std::collections::HashSet<&str> = branches
                .iter()
                .filter_map(|(pattern, _)| match pattern {
                    Lang::Tag { name, .. } => Some(name.as_str()),
                    _ => None,
                })
                .collect();
            let missing: Vec<String> = variants
                .iter()
                .filter(|(name, _)| !existing.contains(name.as_str()))
                .map(|(name, _)| name.clone())
                .collect();
            if !missing.is_empty() {
                errors.push(TypRError::Type(TypeError::NonExhaustiveMatch(
                    missing,
                    expr.get_help_data(),
                )));
            }
        }
    }

    let types: Vec<_> = branch_tcs.iter().map(|tc| tc.value.clone()).collect();

    let output_type = if types.len() == 1 {
        types[0].clone()
    } else {
        builder::union_type(&types)
    };
    TypeContext::new(output_type, expr.clone(), context.clone()).with_errors(errors)
}

#[cfg(test)]
mod tests {
    use crate::components::context::Context;
    use crate::processes::parsing::parse2;
    use crate::processes::type_checking::type_checker::TypeChecker;

    /// Parses and type-checks each statement **separately**, chaining the
    /// resulting context/errors through `TypeChecker::typing_no_panic` —
    /// `parse2` only parses a single top-level statement (trailing input is
    /// silently dropped), so a multi-statement program concatenated into one
    /// string here would silently lose everything after the first `;` and
    /// make every "expect no error" assertion pass vacuously.
    fn typecheck(stmts: &[&str]) -> TypeChecker {
        stmts
            .iter()
            .fold(TypeChecker::new(Context::default()), |tc, stmt| {
                let code = parse2((*stmt).into()).unwrap();
                tc.typing_no_panic(&code)
            })
    }

    // NOTE: these tests deliberately type the scrutinee through a *function
    // parameter* (`fn(c: Color): ... { match c { ... } }`), not an
    // unannotated top-level `let c <- .Red;`. An unannotated `let` infers the
    // narrow, single-variant `Type::Tag("Red", ...)` for its RHS literal —
    // TypR doesn't widen it to the declared alias's full union without an
    // explicit annotation — so it only ever carries the *one* concrete
    // variant that literal happened to construct, not the whole enum. Since
    // M1 (exhaustiveness) and M4 (unknown-variant) are only sound against a
    // scrutinee whose type is a genuine multi-variant union, they're gated
    // to skip that narrow-Tag case entirely (see the comments in
    // `build_match_branch_context`/`match_expression`) — a function
    // parameter typed with the alias (like the pre-existing `Option<int>`
    // parameter case below) is what actually reduces to the full union tree.

    /// M2: a tag pattern over a *user-defined* union (not just the hardcoded
    /// `Option`) must bind its variable to the real payload type, not `Any`.
    #[test]
    fn edge_m2_user_union_tag_binding_is_typed() {
        let tc = typecheck(&[
            "type Shape <- .Circle(int) | .Square(int);",
            "fn(s: Shape): int { match s { .Circle(r) => r + 1, .Square(sq) => sq } }",
        ]);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    /// M2 (still): binding through a tag pattern against a mistyped payload
    /// use should now surface as a real type error instead of silently
    /// typing the binding `Any`.
    #[test]
    fn edge_m2_user_union_tag_binding_catches_type_errors() {
        let tc = typecheck(&[
            "type Shape <- .Circle(int) | .Square(char);",
            "fn(s: Shape): int { match s { .Circle(r) => r, .Square(sq) => sq } }",
        ]);
        assert!(
            tc.has_errors(),
            "Expected a type error: `sq` is bound to `char` (Square's real payload) but used where `int` was expected, got none"
        );
    }

    /// M2/M5: record pattern against an aliased record scrutinee must see
    /// the real field types (through `reduce_type`), not fall back to `Any`.
    #[test]
    fn edge_m5_record_pattern_through_alias_is_typed() {
        let tc = typecheck(&[
            "type Point <- list { x: int, y: int };",
            "fn(p: Point): int { match p { :{x: xx, y: yy} => xx + 1 } }",
        ]);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    /// M1: a match over a 3-variant union covering only 2 must be rejected.
    #[test]
    fn edge_m1_non_exhaustive_match_is_an_error() {
        let tc = typecheck(&[
            "type Color <- .Red | .Green | .Blue;",
            "fn(c: Color): int { match c { .Red => 1, .Green => 2 } }",
        ]);
        assert!(
            tc.has_errors(),
            "Expected a non-exhaustive match error, got none"
        );
    }

    /// M1 (still): a catch-all branch makes the match exhaustive even
    /// without naming every variant.
    #[test]
    fn edge_m1_catch_all_branch_is_exhaustive() {
        let tc = typecheck(&[
            "type Color <- .Red | .Green | .Blue;",
            "fn(c: Color): int { match c { .Red => 1, _ => 0 } }",
        ]);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    /// M4: a tag pattern naming a variant the scrutinee's union doesn't have
    /// must be rejected instead of silently typing to `Any`.
    #[test]
    fn edge_m4_unknown_tag_variant_is_an_error() {
        let tc = typecheck(&[
            "type Color <- .Red | .Green;",
            "fn(c: Color): int { match c { .Red => 1, .Green => 2, .Blue => 3 } }",
        ]);
        assert!(
            tc.has_errors(),
            "Expected an error for the unknown `.Blue` variant, got none"
        );
    }

    /// A bare-name catch-all pattern (not just `_`) must bind the scrutinee
    /// type to that name rather than leaving it undefined.
    #[test]
    fn edge_named_catch_all_pattern_binds_scrutinee_type() {
        let tc = typecheck(&[
            "type Color <- .Red | .Green;",
            "let c <- .Red;",
            "fn(): int { match c { .Red => 1, other => 0 } }",
        ]);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    /// The generic `Option<T>` still resolves through the same, no-longer-
    /// hardcoded code path (regression guard for removing the `"Option"`
    /// special case).
    #[test]
    fn edge_option_tag_binding_still_works_generically() {
        let tc = typecheck(&[
            "fn(o: Option<int>, f: (int) -> int): int { match o { .Some(x) => f(x), .None => 0 } }",
        ]);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }
}
