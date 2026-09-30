# 0080 — refined record fields (refined_types_plan.md, Phase 5, record constructors)

`let a: Account <- list { id: n, tags: v }` used to be a static type error: the literal's type
`list{id: int, tags: [int]}` is not a subtype of `Account` (whose fields are refined), and
`coerce_to` on the whole record only knows about refinements of the value itself.

`field_obligations` (`type_checking/refinement_check.rs`) handles a spread-free record literal
against a record type: each field is a boundary of its own, so the obligation is recorded on the
*field expression* and the transpiler wraps just that value. Wired into `let_expression`, the
return position (`typing`) and `relax_refined_arguments`. A field that is provably wrong
(`id: -3`) is still a static error.
