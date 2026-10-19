#' @include std.R
#' @include generic_functions.R
#' @include types.R

Pt <- function(x, y, .spread = NULL) {
  explicit <- list()
  if (!missing(x)) {
    explicit[["x"]] <- x
  }
  if (!missing(y)) {
    explicit[["y"]] <- y
  }
  x <- typr_spread_record(explicit, .spread)
  as.Pt(x)
}
as.Pt <- function(x) {
  if (!inherits(x, "Pt")) {
    class(x) <- c("Pt", "Sized", "list")
  }
  x <- validate_Pt(x)
  x <- validate(x)
  x
}
validate_Pt <- function(x) {
  required_fields <- c("x", "y")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Pt: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[["x"]], "integer")) {
    stop("Validation failed for type Pt: field 'x' must be of class integer")
  }
  if (!inherits(x[["y"]], "integer")) {
    stop("Validation failed for type Pt: field 'y' must be of class integer")
  }
  x
}
Ci <- function(r, .spread = NULL) {
  explicit <- list()
  if (!missing(r)) {
    explicit[["r"]] <- r
  }
  x <- typr_spread_record(explicit, .spread)
  as.Ci(x)
}
as.Ci <- function(x) {
  if (!inherits(x, "Ci")) {
    class(x) <- c("Ci", "Sized", "list")
  }
  x <- validate_Ci(x)
  x <- validate(x)
  x
}
validate_Ci <- function(x) {
  required_fields <- c("r")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Ci: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[["r"]], "integer")) {
    stop("Validation failed for type Ci: field 'r' must be of class integer")
  }
  x
}

#' @method size Pt
`size.Pt` <- (function(p) {
  {
    p$x + p$y
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method size Ci
`size.Ci` <- (function(c) {
  {
    c$r
  } |>
    as.Integer()
}) |>
  as.Generic()

#' @method total Sized
`total.Sized` <- (function(s) {
  {
    size(s) + 1L |> as.Integer()
  } |>
    as.Integer()
}) |>
  as.Generic()
#' @method total default
`total.default` <- `total.Sized`

print(
  c(total(Pt(x = 1L |> as.Integer(), y = 2L |> as.Integer()))) |> identity()
)
print(c(total(Ci(r = 10L |> as.Integer()))) |> identity())
print(c(size(Ci(r = 3L |> as.Integer()))) |> identity())
