# `$` / `...` on a vector alias: misleading "Unknown element" error

## Repro

`Position` is a vector alias (`[2, int]`), but `move` treats it as a record:
`self$position` and `...self`. The code is genuinely wrong (the fix on the user
side is `self + other`); the bug is in the diagnostics.

## Observed (see `observed.txt`)

```
× Type error: Unknown element `elf$position + other,` in `TypR/position.ty` at `13`
```

Three defects, which together made a type error read like a parse error:

1. `TypeError::WrongExpression` reused the wording of `SyntaxError::UnknownElement`
   ("Unknown element `…` in `file` at `line`") and named neither the field nor the type.
2. Every `Var`'s `HelpData` offset was one byte past the identifier's start:
   `variable_exp` (`processes/parsing/elements.rs`) took the position from
   `starting_char`'s *remaining* span → `elf$position`. The LSP compensated with
   `saturating_sub(1)` in `inlay_hints.rs` / `code_actions.rs`.
3. The line in the message was `text[..offset].lines().count() + 1`, one too many
   (`lines()` already counts the current, partial line).

## Expected

- `e$field` where `e` isn't a record / module / data frame →
  `TypeError::FieldAccessOnNonRecord` (T050): "Cannot access field 'position': type
  Position (= [2, int]) is not a record", underlining the field name
  (`dollar_access.rs::non_record_access_error`).
- `...e` where `e` isn't a record → `TypeError::SpreadNonRecord` (T051), in both the
  record literal (`type_checking/mod.rs`) and constructor (`constructor_call.rs`) paths.
- Var positions start at the identifier's first byte (LSP `-1` compensations removed).
- The remaining generic `WrongExpression` reads "This expression doesn't type-check"
  and underlines the node's own range — no file/line duplicated in the text.
