## Should happen

`describe(m)` must dispatch correctly on `m`'s real S4 `dgeMatrix` class
even though `describe` is declared inside `module Reporter { ... }`.
`print(x)` inside `describe` should show the usual `Matrix::Matrix` print
output.

## Localisation (#@case)

Part of Phase D.2 of `soundness_plan.md`. Same module-boundary `.default`
export gap as `0039-foreign-s3-module-boundary` — see that case's expect.md
for the full mechanism. This case additionally confirms the fix doesn't
special-case S3 vs. S4: the same `describe.default` re-export path handles
both.
