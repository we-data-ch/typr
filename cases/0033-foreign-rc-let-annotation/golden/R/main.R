#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 3 (RC external, setRefClass) x column c (`let x: T <- expr`) of
# interop_matrix.md. See 0031's comment for why isS4() is used as the oracle
# instead of print().
Counter <- Foreign


`counter` <- base::readRDS(to_native("fixtures/rc.rds" |> as.Character())) |> identity()

print(from_bool(base::isS4(to_native(counter))))