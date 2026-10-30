#' @include std.R
#' @include generic_functions.R
#' @include types.R

Box <- function(v, .spread = NULL) {
  explicit <- list()
  if (!missing(v)) {
    explicit[["v"]] <- v
  }
  x <- typr_spread_record(explicit, .spread)
  as.Box(x)
}
as.Box <- function(x) {
  if (!inherits(x, "Box")) {
    class(x) <- c("Box", "list")
  }
  x <- validate_Box(x)
  x <- validate(x)
  x
}
validate_Box <- function(x) {
  required_fields <- c("v")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Box: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  x
}
#' @method pick logical
`pick.logical` <- (function(c, a, b) {
  {
    if (c) {
      a
    } else {
      b
    }
  } |>
    as.State()
}) |>
  as.Generic()

`flag` <- TRUE |> as.Boolean()

`x` <- if (flag) {
  c(1L |> as.Integer(), 2L |> as.Integer()) |> identity()
} else {
  c("a" |> as.Character()) |> identity() |> as.State()
}

print(x)
`p` <- pick(
  FALSE |> as.Boolean(),
  c(1L |> as.Integer(), 2L |> as.Integer()) |> identity(),
  c(3L |> as.Integer(), 4L |> as.Integer()) |> identity()
)

print(p |> identity())
`bs` <- typed_vec(
  Box(v = c(1L |> as.Integer()) |> identity()),
  Box(v = c("a" |> as.Character()) |> identity()),
  dim = c(2)
) |>
  as.Array2()

print(bs[[2L |> as.Integer()]]$v)
`n` <- x |> identity()

print(n)
