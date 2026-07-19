## Should happen

Same as `0050-foreign-s3-match`, for the S4 row. Also a deliberate,
documented hole.

## Localisation (#@case)

Part of Phase D.4 of `soundness_plan.md` (column e) — same root cause as
`0050-foreign-s3-match`, different R-level symptom: an S4 object doesn't
support `[[` indexing at all (`this S4 class is not subsettable`), so
match's `match_val__[[1]]` desugaring fails even earlier/louder than the
S3 row.
