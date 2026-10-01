`[T] & length(> 0)` and `num & (>= 0)` parse, type check and are enforced like the point forms
(`length(5)`, `(> 0)`): the interval model already covered ranges, only the surface syntax and
`Vec` indexing through a `Refined` were missing.

- `[int]` passed where `length(> 0)` is expected: runtime check (`typr_refine_length_range`).
- inside `if (length(v) > 0)`: proven by the condition, no check.
- `[3, int]`: implies the range, no check. `[0, int]` is rejected statically.
- `v[1]` on a `[int] & length(> 0)` parameter type-checks like on `[int]`.

Code: `parsing/types.rs::refinement_property`, `type_arithmetic.rs::apply_refinements`,
`type_checking/mod.rs` (`Lang::ArrayIndexing`).
