source('std.R', echo = FALSE)
source('generic_functions.R')
source('types.R')

plus_one_year <- function(x, ...) UseMethod("plus_one_year")
person <- new.env(parent = emptyenv())
local({
Person <- function(age, name) {
  x <- list(age = age, name = name)
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
`plus_one_year.Record0` <- (function(self) {
Person(name = {
self$name
}, age = {
self$age + 1L |> as.Integer()
})
}) |> as.Generic()

person$Person <- Person
person$plus_one_year.Record0 <- plus_one_year.Record0
registerS3method("plus_one_year", "Record0", plus_one_year.Record0)
})
person$plus_one_year <- plus_one_year
Person <- person$Person
plus_one_year <- person$plus_one_year
plus_one_year(Person(age = 12L |> as.Integer(), name = "Bob" |> as.Character()))