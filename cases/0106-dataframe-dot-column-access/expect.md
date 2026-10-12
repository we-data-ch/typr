# dataframe-dot-column-access

`dot_pipe_access.rs` only knew `Type::Record` receivers for plain field access, so on a typed
dataframe (`Vec(DataFrame, N, Record{...})`) `t.colx` fell through to typing `colx` as a free
variable: `Undefined variable 'colx'`, then `This expression doesn't type-check`. `t$colx`
worked, through a dedicated arm of `dollar_access.rs`.

## Ce qui devrait se passer

`t.colx` types exactly like `t$colx`: a plain vector of the column's element type with the
dataframe's row count (`[2, int]`). The rule lives in one place,
`field_access::dataframe_column_or_push_error`, used by both operators (it replaces the two
duplicated `$` arms, identifier and string literal). An unknown column is a `FieldNotFound`
error on the dataframe type. In R, `t.colx` is `t[['colx']]`, which reads the column.

Prints `[1] 1 2`, `[1] "a" "b"`, `[1] 6`.

## Anomalies (pre-fix)

```
× Type error: Undefined variable 'colx'
× Type error: This expression doesn't type-check
```
