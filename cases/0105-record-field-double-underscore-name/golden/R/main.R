#' @include std.R
#' @include generic_functions.R
#' @include types.R

Qq <- function(a.b, c, .spread = NULL) {
  explicit <- list()
  if (!missing(a.b)) {
    explicit[["a.b"]] <- a.b
  }
  if (!missing(c)) {
    explicit[["c"]] <- c
  }
  x <- typr_spread_record(explicit, .spread)
  as.Qq(x)
}
as.Qq <- function(x) {
  if (!inherits(x, "Qq")) {
    class(x) <- c("Qq", "list")
  }
  x <- validate_Qq(x)
  x <- validate(x)
  x
}
validate_Qq <- function(x) {
  required_fields <- c("a.b", "c")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Qq: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[["a.b"]], "integer")) {
    stop("Validation failed for type Qq: field 'a.b' must be of class integer")
  }
  if (!inherits(x[["c"]], "integer")) {
    stop("Validation failed for type Qq: field 'c' must be of class integer")
  }
  x
}
`r` <- list(a.b = 1L |> as.Integer(), c = 2L |> as.Integer()) |> as.Record0()

`q` <- Qq(a.b = 2L |> as.Integer(), c = 3L |> as.Integer())

`q2` <- Qq(c = 4L |> as.Integer(), a.b = q$a.b)

`s` <- spread(r, list(d = 1L |> as.Integer()))

#' @method get_ab Qq
`get_ab.Qq` <- (function(x) {
  {
    x[['a.b']]
  } |>
    as.Integer()
}) |>
  as.Generic()

print(r[['a.b']])
print(r$a.b)
print(`+`(q[['a.b']], q$a.b))
print(q2[['a.b']])
print(s[['a.b']])
print(get_ab(q))
`d` <- list(a.b = 8L |> as.Integer(), `_u` = 1L |> as.Integer()) |> as.Record2()

d$a.b <- 3L |> as.Integer()
print(`+`(d[['a.b']], d[['_u']]))
`m` <- {
  match_val__ <- d
  if (is.list(match_val__) && !is.null(match_val__[["a.b"]])) {
    v <- match_val__[["a.b"]]
    v
  } else {
    0L |> as.Integer()
  }
}

print(m)
