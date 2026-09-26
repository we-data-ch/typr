# reassign-widen-literal

Source: found while updating `docs/reference/bindings-mutation.md` on `typr.github.io` — its
"Reassignment & mutation" example (`let x <- 0; x <- 10; x <- x + 1; x |> f() |> g()!;`) no
longer type-checks.

## Ce qui devrait se passer

Reassigning an already-bound `let`-declared variable (`x <- 10;`, no `let`) to a new value of the
same base kind should keep working across any number of reassignments — that's the entire point
of `docs/reference/bindings-mutation.md`'s "Reassignment & mutation" section and of the `expr!;`
implicit-mutation sugar (`ai_context/in_place.md`), which desugars to exactly this form
(`lhs <- expr`).

## Anomalies

`observed.txt` shows three failures for one linear sequence of reassignments to a single `int`
variable:

1. `let x <- 0;` (no annotation) infers `x` as the *singleton* literal type `Integer(Val(0))` —
   confirmed deliberate elsewhere (`test_let_expression0`,
   `crates/typr-core/src/processes/type_checking/let_expression.rs`: `let a <- 5;` types `a` as
   `integer_type(5)`). Reassigning to a *different* literal (`x <- 10;`) then fails the subtype
   check outright: "type 0 doesn't match type 10".
2. Even past that (e.g. with an explicit `let x: int <- 0;` instead), the *first* successful
   assignment rebinds `x`'s type in context to the literal RHS type (`Integer(Val(10))`) rather
   than keeping it widened — so the *next* reassignment fails just the same:
   "type 10 doesn't match type int" — `x`'s type keeps narrowing one literal at a time instead
   of staying at its base kind.
3. The `!;` implicit-mutation sugar (`x |> f() |> g()!;`) desugars to the same `Assign` node
   (`ai_context/in_place.md` §5.2), so it inherits the same failure once `x` has narrowed.

Root cause: `Lang::Assign`'s typing arm (`processes/type_checking/mod.rs`) stores the RHS's raw
type as the variable's new type on every successful assignment, instead of widening a literal
singleton to its base kind the way `Type::generalize()` already does elsewhere (e.g.
`cases/0017-char-if-widening`).
