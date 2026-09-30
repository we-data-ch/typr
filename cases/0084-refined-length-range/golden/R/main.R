#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Range bounds: `length(> 0)`, `(>= 0)`. Checked where unproven, absent where proven.
#' @method first integer
`first.integer` <- (function(v) {
  {
    v[[1L |> as.Integer()]]
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method nonneg numeric0
`nonneg.numeric0` <- (function(x) {
  {
    x
  } |>
    as.Number()
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
    first(typr_refine_length_range(v, 0, TRUE, Inf, FALSE, "TypR/main.ty:7"))
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method sized integer
`sized.integer` <- (function(v) {
  {
    first(v)
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method scalar numeric
`scalar.numeric` <- (function(n) {
  {
    nonneg(typr_refine_value(n, 0, FALSE, Inf, FALSE, "TypR/main.ty:9"))
  } |>
    as.Number()
}) |>
  as.Generic()
