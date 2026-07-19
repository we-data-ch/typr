## Should happen

`counter.value` on a `Foreign<T>`-typed value must be rejected at compile
time with a clear error, not silently produce `NULL`/a wrong value. Same
documented gap as `0021-foreign-lm-dot-access-rejected`.

## Localisation (#@case)

Cell recorded as a known hole in `interop_matrix.md` (Phase D,
`soundness_transpilation.md`), `status = "wontfix"` — same rationale as the
S3/S4/factor/RC rows' column f cells.
