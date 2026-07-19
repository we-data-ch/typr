## Should happen

Same as `0044-foreign-s3-record-field`, for the S4 row: `w.model` must show
the real `Matrix::Matrix` print output (`dgeMatrix`), confirming the
per-field pass-through isn't S4-specific-ly broken the way the annotation
cast was (`0022-foreign-s4-matrix-let-annotation`).

## Localisation (#@case)

Part of Phase D.4 of `soundness_plan.md`. See `0044`'s expect.md for the
full mechanism (`Wrapper` constructor doesn't cast the `model` field).
