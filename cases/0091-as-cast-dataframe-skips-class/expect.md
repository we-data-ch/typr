`raw as! Sales` with `Sales` a `dataframe[N]{...}` alias is emitted as `validate_Sales(raw)`. That
checks the columns but returns the data frame without setting the class, so a later
`with_vat(s)` (UFCS, S3 dispatch on `Sales`) finds no method. Other aliases cast through
`as.<Type>`, which validates and sets the class. The same happens for an `@extern` returning a
dataframe type. Expected `as.Sales(raw)`.
