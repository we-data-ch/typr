#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 5 (S7, `S7::new_class`) x column b (return value of a TypR function)
# of interop_matrix.md.
Counter7 <- Foreign

#' @method get_counter
`get_counter` <- (function() {
base::readRDS(to_native("fixtures/s7.rds" |> as.Character()))
} |> identity()) |> as.Generic()

`c` <- get_counter()

print(c)