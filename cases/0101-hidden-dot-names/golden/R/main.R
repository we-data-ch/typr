#' @include std.R
#' @include generic_functions.R
#' @include types.R

Pt <- function(.secret, x, .spread = NULL) {
  explicit <- list()
  if (!missing(.secret)) {
    explicit[[".secret"]] <- .secret
  }
  if (!missing(x)) {
    explicit[["x"]] <- x
  }
  x <- typr_spread_record(explicit, .spread)
  as.Pt(x)
}
as.Pt <- function(x) {
  if (!inherits(x, "Pt")) {
    class(x) <- c("Pt", "list")
  }
  x <- validate_Pt(x)
  x <- validate(x)
  x
}
validate_Pt <- function(x) {
  required_fields <- c(".secret", "x")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Pt: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[[".secret"]], "integer")) {
    stop(
      "Validation failed for type Pt: field '.secret' must be of class integer"
    )
  }
  if (!inherits(x[["x"]], "integer")) {
    stop("Validation failed for type Pt: field 'x' must be of class integer")
  }
  x
}
#' @method .norm Pt
`.norm.Pt` <- (function(p) {
  {
    p[['x']] + p[['.secret']]
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method .twice integer
`.twice.integer` <- (function(a) {
  {
    a * 2L |> as.Integer()
  } |>
    as.Integer()
}) |>
  as.Function1()

`.cache` <- 20L |> as.Integer()

`p` <- Pt(x = 1L |> as.Integer(), .secret = 41L |> as.Integer())

print(.norm(p))
print(.norm(p))
print(.twice(.cache))
Seg <- function(a, b, .spread = NULL) {
  explicit <- list()
  if (!missing(a)) {
    explicit[["a"]] <- a
  }
  if (!missing(b)) {
    explicit[["b"]] <- b
  }
  x <- typr_spread_record(explicit, .spread)
  as.Seg(x)
}
as.Seg <- function(x) {
  if (!inherits(x, "Seg")) {
    class(x) <- c("Seg", "list")
  }
  x <- validate_Seg(x)
  x <- validate(x)
  x
}
validate_Seg <- function(x) {
  required_fields <- c("a", "b")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Seg: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[["a"]], "integer")) {
    stop("Validation failed for type Seg: field 'a' must be of class integer")
  }
  if (!inherits(x[["b"]], "integer")) {
    stop("Validation failed for type Seg: field 'b' must be of class integer")
  }
  x
}
#' @method .norm Seg
`.norm.Seg` <- (function(s) {
  {
    s[['b']] - s[['a']]
  } |>
    as.Integer()
}) |>
  as.Generic()

print(.norm(Seg(a = 2L |> as.Integer(), b = 9L |> as.Integer())))
