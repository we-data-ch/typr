## Should happen

A TypR function declared to return a `Foreign<T>`-family S4 type
(`get_matrix(): MatrixS4`) must hand the value back as a genuine, intact S4
object: `nrow(get_matrix())` should print `2`.

## Localisation (#@case)

The S4 counterpart of `0020-foreign-lm-function-return`: this is the row
where the shared `get_type_anotation` class-append bug (see
`0022-foreign-s4-matrix-let-annotation` for the full write-up) was actively
destructive rather than harmless — a returned S4 object used to lose its
S4-ness at the function-return boundary exactly as it did at the
`let`-annotation boundary, since both paths call the same helper. Fixed in
`VarType::get_type_anotation` (`components/context/vartype.rs`).
