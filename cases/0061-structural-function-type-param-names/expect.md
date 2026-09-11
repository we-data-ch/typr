# structural-function-type-param-names

Source: `comp_correction.md` §B at the TypR workspace root, written 2026-09-09 after running the
docs site's 45 `noplayground` blocks (`docs/reference/functions.md:131`, "Closures") through
`typr check` on 0.5.10.

## Ce qui devrait se passer

A function type is structural: `(int) -> int` desugars to `fn(a: int) -> int` (parameter name
`a` picked by the desugaring), but that name must not leak into type comparison. `fn(z: int):
int { z }` must be accepted anywhere `fn(a: int): int { a }` is, since only arity, parameter
*types* and return type carry meaning.

## Vérification (2026-09-10)

Re-tested against current source (0.5.10, `develop` @ `90f6e30`, 11 commits ahead of the tag
comp_correction.md tested, none touching the type checker):

| Line | comp_correction.md | Actual (current source) |
|---|---|---|
| `fn(a: int): int { a }` against `(int) -> int` | ✅ compiles | ✅ compiles |
| `fn(z: int): int { z }` against `(int) -> int` | ❌ type error (param name mismatch) | ✅ compiles |
| doc's `make_adder`/`add5` closure block | (same bug, longer form) | ✅ compiles |

The reported defect does not reproduce. `is_subtype_raw`'s `Function` arm and `strict_subtype`
(`crates/typr-core/src/components/type/mod.rs`, ~line 207 and ~line 977) already compare arity,
parameter types and return type only, with an explicit comment recording this as deliberate.
Negative cases (mismatched parameter type, mismatched return type, mismatched arity) are
correctly rejected.

## Statut

Kept as a regression net: this is the exact positive repro from `comp_correction.md` §B (plus
the doc's own closure example), so if function-type structural comparison ever regresses to
comparing parameter names, this case catches it. See `doc_correction.md` (side B) for the
doc-side follow-up — the `noplayground` tag on `docs/reference/functions.md:131` can be
reconsidered independently, without touching the example itself.
