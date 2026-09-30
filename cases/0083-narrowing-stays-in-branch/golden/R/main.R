#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Narrowing must not leak: the else-branch, code after the `if`, and `||` (then) prove nothing.
#' @method dist numeric
`dist.numeric` <- (function(p) {
  {
    0 |> as.Number()
  } |>
    as.Number()
}) |>
  as.Generic()

#' @method f numeric
`f.numeric` <- (function(v) {
  {
    if (length(v) == 2L |> as.Integer()) {
      1 |> as.Number()
    } else {
      dist(typr_refine_length(v, 2L, "TypR/main.ty:4"))
    }
    dist(typr_refine_length(v, 2L, "TypR/main.ty:5"))
    if (
      {
        length(v) == 2L |> as.Integer()
      } ||
        {
          length(v) == 3L |> as.Integer()
        }
    ) {
      dist(typr_refine_length(v, 2L, "TypR/main.ty:6"))
    } else {
      0 |> as.Number()
    }
    0 |> as.Number()
  } |>
    as.Number()
}) |>
  as.Generic()
