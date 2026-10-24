#' @include std.R
#' @include generic_functions.R
#' @include types.R

#' @method total data.frame
`total.data.frame` <- (function(sales) {
  {
    sum(sales$units * sales$price)
  } |>
    as.Number()
}) |>
  as.Generic()

`sales` <- data.frame(
  units = c(1 |> as.Number(), 2 |> as.Number(), 3 |> as.Number()),
  price = c(5 |> as.Number(), 6 |> as.Number(), 7 |> as.Number())
) |>
  as.Array0()

total(sales)
