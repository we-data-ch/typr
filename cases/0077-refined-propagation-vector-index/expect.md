# 0077 — propagation of lengths (refined_types_plan.md, Phase 7)

Before: `a[1:3]` was rejected as a wrong index (a vector index on a vector) and, when accepted,
typed `int`; `a > 2` was rejected outright for a vector `a`; `length(a)` was `any`.

Now `a:b` on a one-dimensional vector gives `[b-a+1, T]` when both bounds are literals, a
selection by mask gives `[T]` (length unknown), a comparison is element-wise (`[n, bool]`) and
`length(x)` with `x : [5, T]` is the literal type `5`. The witness is the generated R: the
proven `[3, int]` selections carry no `typr_refine_length`, the mask selection (`[int]` →
`[3, int]`) carries one.
