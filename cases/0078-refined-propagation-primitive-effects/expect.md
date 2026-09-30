# 0078 — length effects of `rev`, `head`, `tail` (refined_types_plan.md, Phase 7)

`rev`, `head` and `tail` come from `base.ty` (doc-only), so the compiler saw them as `any` and
every refined binding downstream needed a runtime check. Their effect on the length is now
hard-coded in `known_effect_call`: `rev(x)` keeps `x`'s type, `head/tail(x, k)` with a literal
`k` give `[min(N, k), T]` (`[max(N + k, 0), T]` for negative `k`). The witness is the generated
R: no `typr_refine_length` around them, while the call to an undeclared function `idf` still
carries one (default rule: no declared effect, no refinement).
