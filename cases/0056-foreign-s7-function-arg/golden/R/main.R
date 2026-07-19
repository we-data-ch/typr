#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 5 (S7, `S7::new_class`) x column a (argument of a TypR function) of
# interop_matrix.md.
Counter7 <- Foreign

#' @method describe Foreign0
`describe.Foreign0` <- (function(x) {
print(x)
1L |> as.Integer()
} |> as.Integer()) |> as.Generic()
#' @method describe default
`describe.default` <- `describe.Foreign0`

`c` <- base::readRDS(to_native("fixtures/s7.rds" |> as.Character()))

print(describe(c))