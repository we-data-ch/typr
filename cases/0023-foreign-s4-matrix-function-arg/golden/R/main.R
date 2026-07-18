#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 2 (S4) x column a (argument of a TypR function) of interop_matrix.md.
MatrixS4 <- Foreign


#' @method get_nrow Foreign0
`get_nrow.Foreign0` <- (function(m) {
from_int(base::nrow(to_native(m)))
} |> as.Integer()) |> as.Generic()
#' @method get_nrow default
`get_nrow.default` <- `get_nrow.Foreign0`

`mat` <- base::readRDS(to_native("fixtures/matrix_s4.rds" |> as.Character()))

print(get_nrow(mat))