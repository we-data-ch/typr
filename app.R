source('b_generic_functions.R')
source('c_types.R')
source('a_std.R', echo = FALSE)

new_person <- function(x, ...) UseMethod("new_person")
is_minor <- function(x, ...) UseMethod("is_minor")
person <- new.env(parent = emptyenv())
local({

`new_person.character` <- (function(name, age) {
list(name = name,
 age = age) |> Generic()
} |> Person()) |> Generic()

`is_minor.Person` <- (function(p) {
p$age < 18L |> Integer()
} |> Boolean()) |> Generic()

person$Person <- Person
person$new_person.character <- new_person.character
registerS3method("new_person", "character", new_person.character)
person$is_minor.Person <- is_minor.Person
registerS3method("is_minor", "Person", is_minor.Person)
})
person$new_person <- new_person
person$is_minor <- is_minor
new_person <- person$new_person
`p` <- new_person('Anna' |> Character(), 32L |> Integer())

p