#' @include std.R
#' @include generic_functions.R
#' @include types.R

#' @method first_row Array0
`first_row.Array0` <- (function(d) {
  {
    a[[1L |> as.Integer()]][[]]
    d
  } |>
    as.Number()
}) |>
  as.Generic()

#' @method via Array1
`via.Array1` <- (function(d) {
  {
    first_row(typr_refine_length_range(
      d,
      0,
      TRUE,
      Inf,
      FALSE,
      "TypR/main.ty:2"
    ))
  } |>
    as.Number()
}) |>
  as.Generic()

print(via(
  data.frame(a = c(4.5 |> as.Number(), 5.5 |> as.Number()) |> identity()) |>
    as.Generic()
))
