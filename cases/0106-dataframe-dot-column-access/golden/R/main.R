#' @include std.R
#' @include generic_functions.R
#' @include types.R

Tab <- function(col.y, colx, .spread = NULL) {
  explicit <- list()
  if (!missing(col.y)) {
    explicit[["col.y"]] <- col.y
  }
  if (!missing(colx)) {
    explicit[["colx"]] <- colx
  }
  x <- typr_spread_record(explicit, .spread)
  as.Tab(do.call(data.frame, c(x, list(stringsAsFactors = FALSE))))
}
as.Tab <- function(x) {
  if (!inherits(x, "Tab")) {
    class(x) <- c("Tab", "data.frame", "list")
  }
  x <- validate_Tab(x)
  x <- validate(x)
  x
}
validate_Tab <- function(x) {
  if (!is.data.frame(x)) {
    stop("Validation failed for type Tab: expected a data.frame")
  }
  required_fields <- c("col.y", "colx")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Validation failed for type Tab: missing columns: ",
      paste(missing_fields, collapse = ", ")
    ))
  }
  if (!inherits(x[["col.y"]], "character")) {
    stop(
      "Validation failed for type Tab: column 'col.y' must be of class character"
    )
  }
  if (!inherits(x[["colx"]], "integer")) {
    stop(
      "Validation failed for type Tab: column 'colx' must be of class integer"
    )
  }
  if (nrow(x) != 2) {
    stop(paste0(
      "Validation failed for type Tab: expected 2 rows, got ",
      nrow(x)
    ))
  }
  x
}
`t` <- Tab(
  colx = c(1L |> as.Integer(), 2L |> as.Integer()) |> identity(),
  col.y = c("a" |> as.Character(), "b" |> as.Character()) |> identity()
)

`a` <- t[['colx']] |> identity()

print(a)
print(t[['col.y']])
print(`+`(sum(t[['colx']]), sum(t$colx)))
