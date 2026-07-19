#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 2 (S4, `Matrix::Matrix`) x column g (pipe) of interop_matrix.md.
MatrixS4 <- Foreign

`m` <- base::readRDS(to_native("fixtures/matrix_s4.rds" |> as.Character()))

print(m)