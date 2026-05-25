source('a_std.R', echo = FALSE)
source('b_generic_functions.R')
source('c_types.R')


`double.Incrementable` <- (function(i) {
incr(incr(i))
} |> Incrementable()) |> Generic()

`incr.integer` <- (function(s) {
s + 1L |> Integer()
} |> Integer()) |> Generic()

`a` <- double(double(3L |> Integer()))

a