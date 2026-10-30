#' @include std.R
#' @include generic_functions.R
#' @include types.R

#' @method rows data.frame
`rows.data.frame` <- (function(d) {
  {
    nrow(d)
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method via data.frame
`via.data.frame` <- (function(d) {
  {
    rows(typr_refine_length_range(d, 0, TRUE, Inf, FALSE, "TypR/main.ty:2"))
  } |>
    as.Integer()
}) |>
  as.Generic()

print(via(
  data.frame(a = c(4.5 |> as.Number(), 5.5 |> as.Number()) |> identity()) |>
    as.Generic()
))
