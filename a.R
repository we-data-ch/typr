source('std.R', echo = FALSE)
source('generic_functions.R')
source('types.R')


`n` <- 5L |> as.Integer() |> as.Integer()

`a` <- validate_Age(n) |> as.Age()

print(a)
`p` <- typr_refine_value(
  n,
  0,
  TRUE,
  Inf,
  FALSE,
  "/tmp/claude-1000/-home-fabrice-sessions-projet-master-TypR-typR-typr/bd8842db-e81a-4d5d-97c7-38453d52fbe8/scratchpad/p1/a.ty:5"
) |>
  as.Generic()

print(p)
