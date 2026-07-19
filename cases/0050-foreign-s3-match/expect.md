## Should happen

Ideally, `match w { .Model(inner) => ..., .Empty => ... }` on a union
`Wrapped <- .Model(LmModel) | .Empty` whose value actually came from
`@extern base::readRDS: (path: char) -> Wrapped;` would dispatch correctly.
In practice this is a deliberate, documented hole, not a fixable bug.

## Localisation (#@case)

Part of Phase D.4 of `soundness_plan.md` (column e), confirming the risk
the plan itself predicted before any case was built: declaring `@extern`'s
return type *directly* as the union `Wrapped` (rather than `Foreign<Any>`)
bypasses the whole `Foreign<T>` mechanism — TypR believes `w` is a properly
tagged union value, but the real `readRDS`'d `lm` object never carries
TypR's synthetic union/tag class vector (`c("Model", "Wrapped", "Tag",
"list")`). Match desugars to `if (match_val__[[1]] == 'Model') { ... }
else if (match_val__[[1]] == 'Empty') { ... }` (see the generated
`R/main.R`) — for an `lm` object, `[[1]]` is a real list access that
returns the `coefficients` element (a length-2 numeric vector), and
comparing a length-2 vector to a string in an `if` condition is itself an R
error: `la condition est de longueur > 1`.

Closing this gap would mean either (a) rejecting `@extern ... -> UnionType`
declarations at compile time when the union isn't itself
`Foreign<T>`-backed (a real, defensible fix — TypR currently has no way to
know the external function won't actually return a properly-tagged value,
so this can't be caught statically without a broader `@extern` type-safety
model), or (b) making `match`'s runtime dispatch defensive against
`[[1]]` not being a plain tag string (defeats the performance/simplicity of
the current desugaring for the overwhelmingly common case of TypR-
constructed union values). Both are out of scope for Phase D.

See `0052-foreign-factor-match` for a same-shaped case with a markedly
worse failure mode: silent no-op instead of a loud crash.
