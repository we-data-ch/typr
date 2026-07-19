## Should happen

`describe(m)` must dispatch correctly on `m`'s real ordered-`factor` class
even though `describe` is declared inside `module Reporter { ... }`.
`print(x)` inside `describe` should show the usual `factor` print output
(`Levels: ...`).

## Localisation (#@case)

Part of Phase D.2 of `soundness_plan.md`. Same module-boundary `.default`
export gap as `0039-foreign-s3-module-boundary` — see that case's expect.md
for the full mechanism.
