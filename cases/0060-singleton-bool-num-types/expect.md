# singleton-bool-num-types

Source: `comp_correction.md` §A at the TypR workspace root, written 2026-09-09 after running the
docs site's 45 `noplayground` blocks (`docs/reference/types.md:27`, "Literal types") through
`typr check` on 0.5.10. `syntaxe.md:100` lists all four literal kinds — `3`, `3.14`, `true`,
`"chat"` — as valid singleton types.

## Ce qui devrait se passer

Every literal used in type position (a singleton type) binds to that exact literal value, and
type inference after binding reports the singleton (`true`, not `bool`; `3.14`, not `num`) —
symmetrically for all four literal kinds.

## Vérification (2026-09-10)

Re-tested the four claimed-broken/working lines directly against current source (0.5.10,
`develop` @ `90f6e30`, 11 commits ahead of the tag comp_correction.md tested, none touching the
type checker):

| Line | comp_correction.md | Actual (current source) |
|---|---|---|
| `let x: 3 = 3;` | compiles | compiles |
| `let name: "hello" = "hello";` | compiles | compiles |
| `let flag: true = true;` | ❌ `type true doesn't match type bool` | ✅ compiles |
| `let x: 3.14 = 3.14;` | ❌ `type 3.14 doesn't match type num` | ✅ compiles |

The reported defect does not reproduce. `boolean_literal`/`number_literal` in
`crates/typr-core/src/processes/parsing/types.rs` (`primitive_types`) are tried before the
generic `boolean`/`number` alternatives, same as `integer_literal`/`character_literal`, and
`is_subtype`/`get_covariant_type` (`components/type/mod.rs`) already handle `Val`/`Unknown` for
all four kinds. Negative cases (`let flag: true = false;`, `let x: 3.14 = 3.0;`) are correctly
rejected.

## Statut

Kept as a regression net rather than dropped: this is the exact positive repro from
`comp_correction.md` §A, so if singleton bool/num typing ever regresses, this case catches it.
See `doc_correction.md` (side A) for the doc-side follow-up — the `noplayground` tag on
`docs/reference/types.md:27` can be reconsidered independently.
