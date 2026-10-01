#' @include std.R
#' @include generic_functions.R
#' @include types.R

# A generic base keeps its refinements: `[#N, T] & length(> 0)` is checked like `[int] & length(> 0)`.
#' @method first Array0
`first.Array0` <- (function(v) {
  {
    v[[1L |> as.Integer()]]
  } |>
    as.Generic()
}) |>
  as.Generic()

#' @method narrowed integer
`narrowed.integer` <- (function(v) {
  {
    if (length(v) > 0L |> as.Integer()) {
      first(v)
    } else {
      0L |> as.Integer()
    }
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method unchecked integer
`unchecked.integer` <- (function(v) {
  {
    first(typr_refine_length_range(v, 0, TRUE, Inf, FALSE, "TypR/main.ty:6"))
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method sized character
`sized.character` <- (function(v) {
  {
    first(v)
  } |>
    as.Character()
}) |>
  as.Generic()
