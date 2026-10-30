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
#' @method id default
`id.default` <- (function(x) {
  {
    x
  } |>
    as.Generic()
}) |>
  as.Generic()

#' @method first Array0
`first.Array0` <- (function(xs) {
  {
    xs[[1L |> as.Integer()]]
  } |>
    as.Generic()
}) |>
  as.Generic()

#' @method unbox Box
`unbox.Box` <- (function(b) {
  {
    b$v
  } |>
    as.Generic()
}) |>
  as.Generic()

#' @method apply default
`apply.default` <- (function(f, x) {
  {
    f(x)
  } |>
    as.Generic()
}) |>
  as.Generic()

#' @method adder integer
`adder.integer` <- (function(n) {
  {
    (function(x) {
      {
        x + n
      } |>
        as.Integer()
    }) |>
      as.Function4()
  } |>
    as.Function4()
}) |>
  as.Generic()

print(c(id(1L |> as.Integer())) |> identity())
print(c(id("s" |> as.Character())) |> identity())
print(
  c(first(c(1.5 |> as.Number(), 2.5 |> as.Number()) |> identity())) |>
    identity()
)
print(c(unbox(Box(v = 7L |> as.Integer()))) |> identity())
print(c(unbox(Box(v = "boxed" |> as.Character()))) |> identity())
print(c(apply(adder(1L |> as.Integer()), 10L |> as.Integer())) |> identity())
print(
  c(apply(
    (function(x) {
      {
        "r" |> as.Character()
      } |>
        as.Character()
    }) |>
      as.Generic(),
    3L |> as.Integer()
  )) |>
    identity()
)
