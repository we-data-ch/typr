#' @include std.R
#' @include generic_functions.R
#' @include types.R

Pt <- function(x_y, .spread = NULL) {
  explicit <- list()
  if (!missing(x_y)) {
    explicit[["x_y"]] <- x_y
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
  required_fields <- c("x_y")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Pt: missing fields: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[["x_y"]], "integer")) {
    stop("Validation failed for type Pt: field 'x_y' must be of class integer")
  }
  x
}
`p` <- Pt(x_y = 1L |> as.Integer())

`r` <- list(a_b = 2L |> as.Integer()) |> as.Record0()

print(`+`(p[['x_y']], r[['a_b']]))
