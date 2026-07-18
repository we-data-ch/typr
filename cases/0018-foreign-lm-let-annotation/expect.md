## Should happen

`type LmModel <- Foreign<Any>;` is exactly the pattern documented in
`crates/typr-cli/configs/std/foreign.ty`'s own usage comment. Reading a real
`stats::lm` fit back through `@extern base::readRDS` and binding it with an
explicit `let m: LmModel <- ...` annotation must not touch the value at all:
printing `m` should dispatch through R's real `print.lm` and show the
`lm` object's usual `Call:`/`Coefficients:` output.

## Localisation (#@case)

Found while implementing Phase D of `soundness_transpilation.md` (the
interop matrix). Two upstream bugs blocked this case from even building,
fixed alongside it:

- `type LmModel <- Foreign<Any>;` raised a bogus `AliasArityMismatch`
  ("Foreign expects 0 type argument, found 1") — `Foreign<T>` is declared
  `opaque` in `foreign.ty`, and `collect_undefined_aliases`
  (`processes/type_checking/let_expression.rs`) skips arity checking for a
  reference written `opaque X` at its *own* use site, but not for a
  reference to an alias that is *itself* declared opaque elsewhere — exactly
  `Foreign<T>`'s case. Fixed by resolving the target alias's own opacity
  before enforcing `params.len() == generics.len()`.
- `@extern base::readRDS: ...;` (no package-qualified call site needed since
  `readRDS` is a bare base-R name) got flagged by the Phase C r-name lint
  and — worse — `generic_functions.R` emitted a `readRDS <- function(x, ...)
  UseMethod('readRDS', x)` stub that would have shadowed the real
  `base::readRDS` the moment any `@extern` call reached it. Fixed by
  excluding `context.extern_fns`/`import_from_fns` names from
  `Context::get_all_generic_functions` (`components/context/mod.rs`).

Once those two were fixed, this specific cell (column c) still corrupted the
value: `let x: T <- expr`'s annotation pipe called `as.LmModel(x)`, i.e.
`x |> struct(c('LmModel', 'Foreign0', 'Foreign0', 'Any'))` — appending TypR
classes onto the object's real class vector. For `lm` (S3) this happens to
be non-destructive (the appended classes land after `"lm"`, so `print.lm`
dispatch still finds `"lm"` first) — see
`0022-foreign-s4-matrix-let-annotation` for the row where the same cast is
actively destructive (S4). Fixed in `VarType::get_type_anotation`
(`components/context/vartype.rs`): a type that resolves through the alias
chain to `Foreign<T>` now gets `identity()` instead of `as.X()`.
