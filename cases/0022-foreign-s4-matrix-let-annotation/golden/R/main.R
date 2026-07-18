#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 2 (S4) x column c (`let x: T <- expr`) of interop_matrix.md.
MatrixS4 <- Foreign


`mat` <- base::readRDS(to_native("fixtures/matrix_s4.rds" |> as.Character())) |> identity()

print(from_int(base::nrow(to_native(mat))))