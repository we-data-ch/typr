# 0081 — a fixed-length parameter is not lifted to the argument's length

`Type::lift` (vectorised calls) used to rewrite the length of a `[2, num]` parameter to `3` whenever an
argument had 3 elements, so `dist(a)` with `a: [3, num]` type-checked. A parameter that already fixes
its length now keeps it (only scalars and unsized/generic-length vectors are lifted), so the call is
rejected statically like the equivalent `let` annotation. Found while working on refined types
(`refined_types_plan.md`, Phase 9 prerequisite).
