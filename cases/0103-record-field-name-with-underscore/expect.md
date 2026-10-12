# record-field-name-with-underscore

`name = value` fields (record literals, constructors, keyword records) were read by
`parsing/elements.rs::argument_val` with `alphanumeric1`, which stops at `_`. So `list{ a_b = 1 }`
was rejected (`list{...}` requires named fields), and `Pt:{ x_y = 1 }` fell back to another
parse — a `ForgottenSemicolon` warning and a `Tuple` instead of a `Pt`.

## Ce qui devrait se passer

`argument_name` accepts any identifier (`a_b`, hidden `.b`/`__b`) and still the bare
alphanumerics accepted before (`Name`). `p.x_y + r.a_b` prints `3`.

## Anomalies (pre-fix)

`Syntax error: list{...} requires named fields`, and `struct(list(x_y = ...), 'Tuple')` for `Pt`.
