source('b_generic_functions.R')
source('c_types.R')
source('a_std.R', echo = FALSE)

`x` <- typed_vec('a' |> Character(), 'b' |> Character(), 'c' |> Character(), dim = c(3)) |> Array0()

x