#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 1 (S3 external) x column b (return value of a TypR function) of
# interop_matrix.md.
LmModel <- Foreign

#' @method get_model
`get_model` <- (function() {
base::readRDS(to_native("fixtures/lm.rds" |> as.Character()))
} |> identity()) |> as.Generic()

`m` <- get_model()

print(m)