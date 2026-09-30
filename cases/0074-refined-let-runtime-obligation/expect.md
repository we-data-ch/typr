# 0074 — refined `let`: boundary check (refined_types_plan.md, Phase 5/6)

`let a: [3, int] <- v` with `v: [int]` used to be a static type error. `is_subtype` still says
`false` (it only answers `true` for a *proven* refinement), but `coerce_to`
(`type_checking/refinement_check.rs`) sees that the base types agree and the refinements are
compatible, so `let_expression` records an obligation in `Context::refinement_obligations` and
the transpiler wraps the initialiser in `typr_refine_length(...)` / `typr_refine_value(...)`
(prelude `std.R`). A literal or an already-refined value that proves the property gets no check.
