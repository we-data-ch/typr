#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 7 (base vector with attributes) x column a (argument of a TypR
# function) of interop_matrix.md.
FactorExt <- Foreign


#' @method count_levels Foreign0
`count_levels.Foreign0` <- (function(f) {
from_int(base::nlevels(to_native(f)))
} |> as.Integer()) |> as.Generic()
#' @method count_levels default
`count_levels.default` <- `count_levels.Foreign0`

`f` <- base::readRDS(to_native("fixtures/factor_ext.rds" |> as.Character()))

print(count_levels(f))