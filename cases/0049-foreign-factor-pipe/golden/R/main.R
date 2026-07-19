#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 7 (base vector w/ attributes, ordered `factor`) x column g (pipe) of
# interop_matrix.md.
FactorExt <- Foreign

`m` <- base::readRDS(to_native("fixtures/factor_ext.rds" |> as.Character()))

print(m)