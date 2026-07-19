#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 4 (R6 external) x column c (`let x: T <- expr`) of interop_matrix.md.
# See 0035's comment for why inherits() is used as the oracle instead of
# print().
Counter6 <- Foreign


`counter` <- base::readRDS(to_native("fixtures/r6.rds" |> as.Character())) |> identity()

print(from_bool(base::inherits(to_native(counter), to_native("Counter6" |> as.Character()))))