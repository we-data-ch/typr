source('std.R', echo = FALSE)
source('generic_functions.R')
source('types.R')

Person <- function(age, name, .spread = NULL) {
  explicit <- list()
  if (!missing(age)) explicit[["age"]] <- age
  if (!missing(name)) explicit[["name"]] <- name
  x <- typr_spread_record(explicit, .spread)
  as.Person(x)
}
as.Person <- function(x) {
  if (!inherits(x, "Person")) class(x) <- c("Person", "list")
  x <- validate_Person(x)
  x <- validate(x)
  x
}
validate_Person <- function(x) {
  required_fields <- c("age", "name")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Person: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["age"]], "integer")) stop("Validation failed for type Person: field 'age' must be of class integer")
  if (!inherits(x[["name"]], "character")) stop("Validation failed for type Person: field 'name' must be of class character")
  x
}
`plus_one_year.Person` <- (function(self) {
Person(name = {
self$name
}, age = {
self$age + 1L |> as.Integer()
})
}) |> as.Generic()
