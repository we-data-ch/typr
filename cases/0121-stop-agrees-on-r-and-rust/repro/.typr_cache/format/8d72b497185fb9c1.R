#' @include std.R
#' @include generic_functions.R
#' @include types.R

#' @method check integer
`check.integer` <- (function(x) {
  {
    if (x > 2L |> as.Integer()) {
      stop("too big" |> as.Character())
    } else {
      print(c(x) |> identity())
    }
  } |>
    as.Empty0()
}) |>
  as.Generic()

check(1L |> as.Integer())
check(2L |> as.Integer())
check(5L |> as.Integer())
check(0L |> as.Integer())
