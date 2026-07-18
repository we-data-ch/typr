#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 7 (base vector with attributes) x column c (`let x: T <- expr`) of
# interop_matrix.md. A base::factor(), deliberately NOT TypR's own opaque
# Factor<L> stdlib type (factor.ty) -- this exercises a genuinely foreign
# value with attributes (levels, class = c("ordered","factor")).
FactorExt <- Foreign


`f` <- base::readRDS(to_native("fixtures/factor_ext.rds" |> as.Character())) |> identity()

print(from_int(base::nlevels(to_native(f))))