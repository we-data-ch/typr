#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 1 (S3 external) x column a (argument of a TypR function) of
# interop_matrix.md.
LmModel <- Foreign



#' @method count_coefficients Foreign0
`count_coefficients.Foreign0` <- (function(m) {
from_int(base::length(to_native(stats::coef(to_native(m)))))
} |> as.Integer()) |> as.Generic()
#' @method count_coefficients default
`count_coefficients.default` <- `count_coefficients.Foreign0`

`m` <- base::readRDS(to_native("fixtures/lm.rds" |> as.Character()))

print(count_coefficients(m))