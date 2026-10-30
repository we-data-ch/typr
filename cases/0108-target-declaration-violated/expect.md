No `typr.toml`, so the project builds for `r` only. The declaration is still checked: it promises
`rust`, the body uses an `R { ... }` block. The error (T053) is reported on the declaration, not at
a call site (rfcs/0000-compilation-targets.md §4).
