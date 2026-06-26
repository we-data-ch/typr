source('std.R', echo = FALSE)
source('generic_functions.R')
source('types.R')

source('person.R')
Person <- person$Person
plus_one_year <- person$plus_one_year
plus_one_year(Person(age = 12L |> as.Integer(), name = "Bob" |> as.Character()))