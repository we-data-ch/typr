#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 7 (base vector with attributes) x column b (return value of a TypR
# function) of interop_matrix.md.
FactorExt <- Foreign


#' @method get_factor
`get_factor` <- (function() {
base::readRDS(to_native("fixtures/factor_ext.rds" |> as.Character()))
} |> identity()) |> as.Generic()

`f` <- get_factor()

print(from_int(base::nlevels(to_native(f))))