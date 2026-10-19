#' @include std.R
#' @include generic_functions.R
#' @include types.R

#' @method pick logical
`pick.logical` <- (function(c, a, b) {
  {
    if (c) {
      a
    } else {
      b
    }
  } |>
    as.State()
}) |>
  as.Generic()

print(pick(
  FALSE |> as.Boolean(),
  c(1L |> as.Integer(), 2L |> as.Integer()) |> identity(),
  c(3L |> as.Integer(), 4L |> as.Integer()) |> identity()
))
