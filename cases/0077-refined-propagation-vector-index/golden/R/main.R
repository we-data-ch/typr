#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Index/comparison/length results carry their length: a proven `[3, int]` needs no check.
`a` <- c(
  1L |> as.Integer(),
  2L |> as.Integer(),
  3L |> as.Integer(),
  4L |> as.Integer(),
  5L |> as.Integer()
) |>
  identity() |>
  identity()

`head3` <- a[seq(1L |> as.Integer(), 3L |> as.Integer(), 1L |> as.Integer())] |>
  identity()

`mid` <- a[seq(2L |> as.Integer(), 4L |> as.Integer(), 1L |> as.Integer())] |>
  identity()

`big` <- typr_refine_length(a[a > 2L |> as.Integer()], 3L, "TypR/main.ty:5") |>
  identity()

`n` <- length(a) |> as.Integer()
