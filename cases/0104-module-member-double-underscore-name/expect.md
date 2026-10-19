# module-member-double-underscore-name

`__` in a name stands for `.` in R (`my__v` → `my.v`), and a member's *definition* inside the
module's `local({...})` already applied it (the `Lang::Variable` arm of `transpiling/mod.rs`).
But the module's export lines (`M$name <- name`, `@export` re-exports, `registerS3method`, the
`--test` `M$.test_name` aliases and the test preamble) were built from the raw TypR name, and
`M::my__v` transpiled to `M$my__v`. A name starting with `_` was not quoted either (`M$_u`).

## Ce qui devrait se passer

Every module-side reference uses the name the definition actually binds:
`transpiling/mod.rs::module_member_name` (`__` → `.`, `_x` quoted) and `module_member_binding`
(plus the S3 class suffix, which a `__` name never gets — like its definition). `M::x` on a
module goes through the same conversion (`Op::Dollar` arm, `Type::Module` case). Prints `2`,
`5`, `7`, `2`.

## Anomalies (pre-fix)

```
M$my__v <- my__v              # but the binding is `my.v`
M$as__pt.Pt <- as__pt.Pt      # but the binding is `as.pt`, with no S3 suffix
registerS3method("as__pt", "Pt", as__pt.Pt)
print(M$my__v)
```
→ `objet 'my__v' introuvable`.
