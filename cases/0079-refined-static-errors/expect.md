# 0079 — refinement errors are static (refined_types_plan.md, Phase 3/8)

Two families of errors, both raised while the `let` annotation is reduced
(`norm_refinement` / `apply_refinements` in `type_checking/type_arithmetic.rs`):

- **`UnsatisfiableRefinement` (T046)**: the properties leave an empty interval
  (`(> 10) & (< 5)`), or a second `length(n)` disagrees with the length already stored in the
  `Vec` index (D4: `[int] & length(5)` is `[5, int]`).
- **`InvalidRefinement` (T045)**: the base lacks the capability — `length` on `int`,
  `(> c)` on `chr`.

Neither may panic, and neither may reach the transpiler.
