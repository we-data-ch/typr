source('b_generic_functions.R')
source('c_types.R')
source('a_std.R', echo = FALSE)


`double.Addable` <- (function(a) {
a + a
} |> Addable()) |> Generic()

double(5L |> Integer())