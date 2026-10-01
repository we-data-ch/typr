# 0075 — refined argument: caller-side check (refined_types_plan.md, Phase 5, §37)

No signature accepts `[num]` for `[2, num]` strictly, so `apply_from_variable_inner` retries with
`relax_refined_arguments`: an argument that only fails to *prove* a refinement stands in as the
parameter type, and the residual is recorded as an obligation on the argument expression. The
transpiler wraps that argument. Arguments proven by their type (literals) get no check.
