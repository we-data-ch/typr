source('std.R', echo = FALSE)
source('generic_functions.R')
source('types.R')

Todo <- function(done, name, .spread = NULL) {
  explicit <- list()
  if (!missing(done)) explicit[["done"]] <- done
  if (!missing(name)) explicit[["name"]] <- name
  x <- typr_spread_record(explicit, .spread)
  as.Todo(x)
}
as.Todo <- function(x) {
  if (!inherits(x, "Todo")) class(x) <- c("Todo", "list")
  x <- validate_Todo(x)
  x <- validate(x)
  x
}
validate_Todo <- function(x) {
  required_fields <- c("done", "name")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Todo: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["done"]], "logical")) stop("Validation failed for type Todo: field 'done' must be of class logical")
  if (!inherits(x[["name"]], "character")) stop("Validation failed for type Todo: field 'name' must be of class character")
  x
}
`print2.Todo` <- (function(self) {
cat("Todo:" |> as.Character(), self$name, self$done, "\n" |> as.Character())
} |> as.Empty0()) |> as.Generic()

`t` <- Todo(name = "a" |> as.Character(), done = TRUE |> as.Boolean())

print2(t)