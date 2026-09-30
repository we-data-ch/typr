# 0076 — refined return type (refined_types_plan.md, Phase 5)

`Context::return_position` marks the expressions whose value is the function's result: the
trailing expression, both `if` branches, the argument of `return`. `typing()` checks such a value
against `expected_return_type` with `coerce_to`; an unproven-but-compatible value records an
obligation on that expression, so an early `return` or one branch is checked on its own and a
proven branch is not. Obligations survive the sub-contexts of `if` and `function()` through
`Context::absorb_obligations`.
