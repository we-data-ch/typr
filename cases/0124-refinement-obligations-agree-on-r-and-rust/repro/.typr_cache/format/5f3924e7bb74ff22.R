#' @include std.R
#' @include generic_functions.R
#' @include types.R

`v` <- c(1L |> as.Integer(), 2L |> as.Integer(), 3L |> as.Integer()) |>
  identity() |>
  identity()

`a` <- typr_refine_length(v, 3L, "TypR/main.ty:2") |> identity()

print(a)
`n` <- 5L |> as.Integer() |> as.Integer()

#' @method pos default
`pos.default` <- (function(x) {
  {
    x
  } |>
    as.Integer()
}) |>
  as.Generic()

print(c(pos(n), 6L |> as.Integer()) |> identity())
#' @method mk integer
`mk.integer` <- (function(w) {
  {
    typr_refine_length(w, 3L, "TypR/main.ty:7")
  } |>
    identity()
}) |>
  as.Generic()

print(mk(v))
`short` <- c(4L |> as.Integer(), 5L |> as.Integer()) |> identity() |> identity()

print(mk(short))
print(a)
