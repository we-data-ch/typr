#' @include std.R
#' @include generic_functions.R
#' @include types.R

`_x` <- 3L |> as.Integer()

#' @method _inc integer
`_inc.integer` <- (function(a) {
  {
    a + 1L |> as.Integer()
  } |>
    as.Integer()
}) |>
  as.Function0()

print(`_inc`(`_x`))
