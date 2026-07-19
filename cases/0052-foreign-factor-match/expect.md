## Should happen

Same as `0050-foreign-s3-match`, for the base-attributed-vector row. Also a
deliberate, documented hole — but with a **markedly worse** failure mode
than the other two rows, worth flagging on its own: **silent no-op**
instead of a loud crash.

## Localisation (#@case)

Part of Phase D.4 of `soundness_plan.md` (column e) — same root cause as
`0050-foreign-s3-match`, but a distinctly dangerous R-level symptom.
`w[[1]]` on an ordered `factor` returns its *first element as a
length-1 factor* (`"lo"`), not an error. Comparing that against the tag
strings `'Model'`/`'Empty'` (neither of which are levels of this factor)
evaluates cleanly to `FALSE` for both — factor's `==` operator returns
`FALSE`, not `NA`, when compared against a string outside its levels
(confirmed directly with a standalone `Rscript` check against the same
fixture). Neither `if` branch fires, there's no `else`, and execution
simply continues past the `match` block with **no error, no output, exit
code 0** — confirmed by adding a trailing `print("reached end")` after the
`match` in this repro: it's the *only* thing printed. A caller relying on
one of the two arms having run (e.g. for a side effect) would never notice
anything went wrong. This is worse than `0050`/`0051`'s loud crash and is
the strongest argument in this grid for treating `@extern ... ->
UnionType` (bypassing `Foreign<T>`) as something worth rejecting at
compile time in a future pass, rather than leaving it purely as a
documented hole.
