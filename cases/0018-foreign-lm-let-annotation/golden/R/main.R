#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 1 (S3 external) x column c (`let x: T <- expr`) of interop_matrix.md.
LmModel <- Foreign

`m` <- base::readRDS(to_native("fixtures/lm.rds" |> as.Character())) |> identity()

print(m)