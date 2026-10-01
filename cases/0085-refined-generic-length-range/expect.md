A generic base keeps its refinements at a call: `[#N, T] & length(> 0)` unifies `T` like `[#N, T]`,
and the refinement is then decided against the argument, since unification alone never looks at it.

- `[int]` passed in: runtime check (`typr_refine_length_range`) on the argument.
- inside `if (length(v) > 0)`, or `[3, char]`: proven, no check.
- `[0, char]`: rejected statically (no signature matches).

Code: `type_checking/mod.rs::get_gen_type` and `unification.rs` (`Refined` arms),
`function_application.rs::generic_refinement_obligations`, `type_arithmetic.rs::declared_refinements`.
