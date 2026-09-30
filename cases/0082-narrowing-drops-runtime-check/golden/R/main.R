#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Condition narrowing: inside the branch the condition proves the refinement.
#' @method dist numeric
`dist.numeric` <- (function(p) {
  {
    0 |> as.Number()
  } |>
    as.Number()
}) |>
  as.Generic()

#' @method pos default
`pos.default` <- (function(x) {
  {
    x
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method f numeric
`f.numeric` <- (function(v, n) {
  {
    if (length(v) == 2L |> as.Integer()) {
      dist(v)
    } else {
      0 |> as.Number()
    }
    if (2L |> as.Integer() == length(v)) {
      dist(v)
    } else {
      0 |> as.Number()
    }
    if (
      {
        length(v) > 1L |> as.Integer()
      } &&
        {
          length(v) < 3L |> as.Integer()
        }
    ) {
      dist(v)
    } else {
      0 |> as.Number()
    }
    if (n > 0L |> as.Integer()) {
      pos(n)
    } else {
      0L |> as.Integer()
    }
    if (n <= 0L |> as.Integer()) {
      0L |> as.Integer()
    } else {
      pos(n)
    }
    if (
      !{
        n < 1L |> as.Integer()
      }
    ) {
      pos(n)
    } else {
      0L |> as.Integer()
    }
    if (length(v) == 2L |> as.Integer()) {
      if (n > 0L |> as.Integer()) {
        dist(v)
      } else {
        0 |> as.Number()
      }
    } else {
      0 |> as.Number()
    }
    0 |> as.Number()
  } |>
    as.Number()
}) |>
  as.Generic()
