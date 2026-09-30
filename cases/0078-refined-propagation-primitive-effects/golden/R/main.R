#' @include std.R
#' @include generic_functions.R
#' @include types.R

# rev/head/tail: declared length effects. `head(a, 9)` clamps to 5; a call to an
# unrelated function gets no refinement, so its result is checked at the boundary.

`a` <- c(
  1L |> as.Integer(),
  2L |> as.Integer(),
  3L |> as.Integer(),
  4L |> as.Integer(),
  5L |> as.Integer()
) |>
  identity() |>
  identity()

`r` <- rev(a) |> identity()

`h` <- head(a, 2L |> as.Integer()) |> identity()

`t` <- tail(a, 3L |> as.Integer()) |> identity()

`d` <- head(a, -2L |> as.Integer()) |> identity()

`all` <- head(a, 9L |> as.Integer()) |> identity()

`n` <- length(rev(a)) |> as.Integer()

#' @method idf integer
`idf.integer` <- (function(x) {
  {
    x
  } |>
    identity()
}) |>
  as.Generic()

`k` <- typr_refine_length(idf(a), 5L, "TypR/main.ty:13") |> identity()
