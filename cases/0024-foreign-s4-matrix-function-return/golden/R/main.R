#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 2 (S4) x column b (return value of a TypR function) of
# interop_matrix.md.
MatrixS4 <- Foreign


#' @method get_matrix
`get_matrix` <- (function() {
base::readRDS(to_native("fixtures/matrix_s4.rds" |> as.Character()))
} |> identity()) |> as.Generic()

`mat` <- get_matrix()

print(from_int(base::nrow(to_native(mat))))