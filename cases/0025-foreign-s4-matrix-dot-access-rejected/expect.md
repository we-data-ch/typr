## Should happen

`mat.x` on a `Foreign<T>`-typed S4 value must be rejected at compile time,
same rationale as `0021-foreign-lm-dot-access-rejected`. This also means
TypR's `.`/`$` syntax can never reach a real S4 `@`-slot either way — an
`@extern methods::slot: (obj: Foreign<Any>, name: char) -> T;` accessor
(as demonstrated indirectly via `base::nrow` in `0022`-`0024`) is the only
way in today.

## Localisation (#@case)

Cell recorded as a known hole in `interop_matrix.md`, `status = "wontfix"`
— same rationale as `0021-foreign-lm-dot-access-rejected`.
