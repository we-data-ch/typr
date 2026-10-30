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
Nothing <- function(x) {
  v <- list("Nothing", body = x)
  as.Nothing(v)
}
as.Nothing <- function(x) {
  if (!inherits(x, "Nothing")) {
    class(x) <- c("Nothing", "Opt", "Tag", "list")
  }
  x <- validate_Nothing(x)
  x <- validate(x)
  x
}
validate_Nothing <- function(x) {
  if (x[[1]] != 'Nothing') {
    stop("Validation failed for type Nothing: expected tag 'Nothing'")
  }

  if (is.null(x[["body"]])) {
    stop("Validation failed for type Nothing: missing 'body' field")
  }
  if (!is.character(x[["body"]])) {
    stop("Validation failed for type Nothing: body must be char")
  }
  x
}
Some <- function(x) {
  v <- list("Some", body = x)
  as.Some(v)
}
as.Some <- function(x) {
  if (!inherits(x, "Some")) {
    class(x) <- c("Some", "Opt", "Tag", "list")
  }
  x <- validate_Some(x)
  x <- validate(x)
  x
}
validate_Some <- function(x) {
  if (x[[1]] != 'Some') {
    stop("Validation failed for type Some: expected tag 'Some'")
  }

  if (is.null(x[["body"]])) {
    stop("Validation failed for type Some: missing 'body' field")
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

#' @method get_or Opt
`get_or.Opt` <- (function(o, d) {
  {
    {
      match_val__ <- o
      if (match_val__[[1]] == 'Some') {
        x <- match_val__[["body"]]
        x
      } else if (match_val__[[1]] == 'Nothing') {
        why <- match_val__[["body"]]
        d
      }
    }
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
      as.Function5()
  } |>
    as.Function5()
}) |>
  as.Generic()

`a` <- structure(
  list('Some', body = 4L |> as.Integer()),
  class = c('Some', 'Union1', 'Tag', 'list')
) |>
  as.Opt()

`b` <- structure(
  list('Nothing', body = "none" |> as.Character()),
  class = c('Nothing', 'Union1', 'Tag', 'list')
) |>
  as.Opt()

print(c(id(1L |> as.Integer())) |> identity())
print(c(id("s" |> as.Character())) |> identity())
print(
  c(first(c(1.5 |> as.Number(), 2.5 |> as.Number()) |> identity())) |>
    identity()
)
print(c(unbox(Box(v = 7L |> as.Integer()))) |> identity())
print(c(unbox(Box(v = "boxed" |> as.Character()))) |> identity())
print(c(get_or(a, 0L |> as.Integer())) |> identity())
print(c(get_or(b, "dflt" |> as.Character())) |> identity())
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
