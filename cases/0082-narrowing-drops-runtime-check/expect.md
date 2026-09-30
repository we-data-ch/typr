# 0082 — a branch condition narrows the variable (refined_types_plan.md, Phase 9, §24)

`if (length(v) == 2) { dist(v) }` needs no `typr_refine_length`: the condition is the check. The
facts read from the condition (`length(x) <op> n`, `x <op> c` on scalars, `&&`, `||` in the else
branch, `!`) are intersected with the variable's type inside the matching branch only
(`type_checking/narrowing.rs`). `!(n < 1)` is `n >= 1`, and `[1, +inf)` lies inside `(0, +inf)`, so that call is proven too.
