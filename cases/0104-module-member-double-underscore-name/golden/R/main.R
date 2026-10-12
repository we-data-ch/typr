#' @include std.R
#' @include generic_functions.R
#' @include types.R

Pt <- function(x, .spread = NULL) {
  explicit <- list()
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
  required_fields <- c("x")
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
  x
}
f <- function(x, ...) UseMethod("f")
M <- new.env(parent = emptyenv())
local({
  `my.v` <- 2L |> as.Integer()

  #' @method as pt
  `as.pt` <- (function(p) {
    {
      p[['x']]
    } |>
      as.Integer()
  }) |>
    as.Generic()

  `_u` <- 7L |> as.Integer()

  #' @method f Pt
  `f.Pt` <- (function(p) {
    {
      p[['x']] + 1L |> as.Integer()
    } |>
      as.Integer()
  }) |>
    as.Generic()

  M$my.v <- my.v
  M$as.pt <- as.pt
  M$`_u` <- `_u`
  M$f.Pt <- f.Pt
  registerS3method("f", "Pt", f.Pt)
})
M$f <- f
f.Pt <- M$f.Pt
print(M$my.v)
print(M$as.pt(Pt(x = 5L |> as.Integer())))
print(M$`_u`)
print(M$f(Pt(x = 1L |> as.Integer())))
