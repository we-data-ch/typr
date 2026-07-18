## Should happen

`type FactorExt <- Foreign<Any>;` with a real base `factor(..., ordered =
TRUE)` (deliberately not TypR's own opaque `Factor<L>` stdlib type, see
`factor.ty`) read back through `@extern base::readRDS` and bound with an
explicit `let f: FactorExt <- ...` annotation must keep its `levels`
attribute and `class = c("ordered", "factor")` intact: `nlevels(f)` should
print `3`.

## Localisation (#@case)

Same upstream prerequisites and class-append mechanism as
`0018-foreign-lm-let-annotation` / `0022-foreign-s4-matrix-let-annotation`.
For a plain attributed base vector (not S4), the appended TypR classes are
non-destructive the same way they are for `lm` — `unique(c(class(x),
new_class))` prepends the real classes first, so `nlevels`'s S3 dispatch
(`nlevels.factor`, part of base R... actually `nlevels()` is a *plain*, not
S3-dispatched, function — see the `nlevels.default` note in CLAUDE.md's
`std.R` hard rule for the unrelated, historical TypR-side `nlevels` bug)
would have kept working regardless. Kept as a case anyway: the class vector
was still being silently mutated (a `factor` object gaining
`"FactorExt"`/`"Foreign0"`/`"Any"` classes it never should have), which is
wrong on principle even where it happens not to break dispatch — exactly
the kind of latent bug the interop matrix exists to surface before it
becomes a real one (e.g. the moment a downstream `@extern` call or a
user's own S3 method keys off `class(f)[1]`).
