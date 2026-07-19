#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 5 (S7, `S7::new_class`) x column c (`let x: T <- expr`) of
# interop_matrix.md.
Counter7 <- Foreign

`c` <- base::readRDS(to_native("fixtures/s7.rds" |> as.Character())) |> identity()

print(c)