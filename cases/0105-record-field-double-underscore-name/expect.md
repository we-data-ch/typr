# record-field-double-underscore-name

`__` in a name stands for `.` in R. Field *access* through `.` already applied it
(`r.a__b` → `r[['a.b']]`), but every other place a field name reaches R used the raw TypR
name: record literals and constructor calls (`list(a__b = ...)`, `Qq(a__b = ...)`), the record
alias constructor/validator (`function(a__b)`, `explicit[["a__b"]]`, `required_fields`), `$`
access (`r$a__b`), nominal spreads (`a__b = q$a__b`) and record patterns in `match`. So the
value was stored under `a__b` and read back under `a.b` → `NULL`.

## Ce qui devrait se passer

A field `a__b` is `a.b` in R everywhere. Two helpers in `components/language/mod.rs`:
`r_name_string` (`__` → `.`, for string positions: `[['a.b']]`, `c("a.b")`) and
`r_name_symbol` (same, plus backticks for a leading `_`, for symbol positions:
`list(a.b = )`, `function(a.b)`, `x$a.b`). Used by `ArgumentValue::to_r`, the `Op::Dot` /
`Op::Dollar` arms, the record/dataframe alias generators and the `match` record patterns of
`transpiling/mod.rs`. A `_u` field is now also read right through `.` (`d[['_u']]`, it used to
go through the symbol path and become `` d[['`_u`']] ``).

Prints `1 1 4 2 1 2 4 3`, one per line.

## Anomalies (pre-fix)

```
`r` <- list(a__b = 1L, ...)
print(r[['a.b']])     # NULL
print(r$a__b)         # 1
```
