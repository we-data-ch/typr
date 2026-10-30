#' @include std.R
#' @include generic_functions.R
#' @include types.R

#' @method inc integer
`inc.integer` <- (function(x) {
  {
    x + 1L |> as.Integer()
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method big integer
`big.integer` <- (function(x) {
  {
    x > 2L |> as.Integer()
  } |>
    as.Boolean()
}) |>
  as.Generic()

#' @method add2 integer
`add2.integer` <- (function(a, b) {
  {
    a + b
  } |>
    as.Integer()
}) |>
  as.Function0()

`xs` <- c(
  1L |> as.Integer(),
  2L |> as.Integer(),
  3L |> as.Integer(),
  4L |> as.Integer()
) |>
  identity()

print(map(xs, inc.integer))
print(filter(xs, big.integer))
print(c(reduce(xs, add2.default)) |> identity())
print(c(fold(xs, 10L |> as.Integer(), add2.default)) |> identity())
print(extend(xs, 5L |> as.Integer()))
print(
  c(startsWith("hello" |> as.Character(), "hel" |> as.Character())) |>
    identity()
)
print(
  c(endsWith("hello" |> as.Character(), "hel" |> as.Character())) |> identity()
)
