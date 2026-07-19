## Should happen

`c.value` on a `Foreign<T>`-typed value must be rejected at compile time
with a clear error, not silently produce `NULL`/a wrong value. Same
documented gap as `0021-foreign-lm-dot-access-rejected`: `Foreign<T>`
reduces to `Any`, which carries no structural field information for the
dot-access typing rule to resolve against — TypR has no way to know a real
S7 object has a `value` property without an explicit `@extern` accessor
(e.g. `@extern S7::prop`).

## Localisation (#@case)

Cell recorded as a known hole in `interop_matrix.md` (Phase D,
`soundness_transpilation.md`), `status = "wontfix"` — same rationale as
every other row's column f cells.
