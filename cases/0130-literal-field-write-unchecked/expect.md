# writing a field of a literal-typed record keeps the stale literal type

`let x <- list(a = 1, b = true); x$a <- 5;` puis `let z: list{a: 1, b: bool} <- x;` est accepté :
`x` garde le type `list{a: 1, b: true}` alors que `a` vaut 5. L'écriture de champ doit être
vérifiée contre le type du champ, ou le type de `x` doit être élargi.
