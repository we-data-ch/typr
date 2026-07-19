#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 4 (R6 external) x column a (argument of a TypR function) of
# interop_matrix.md.
#
# Uses base::inherits rather than print() as the oracle: print.R6's real
# formatting requires the R6 package to be attached/imported for method
# dispatch, which a bare readRDS()'d fixture (not sourced through an
# @extern pkg::fn call to a real R6-exporting package) doesn't guarantee --
# a pure-R session/package-loading quirk unrelated to the soundness
# property under test. inherits() needs no package and is exactly what a
# corrupting as.X()/struct() cast would still preserve/break predictably.
Counter6 <- Foreign


#' @method describe Foreign0
`describe.Foreign0` <- (function(x) {
from_bool(base::inherits(to_native(x), to_native("Counter6" |> as.Character())))
} |> as.Boolean()) |> as.Generic()
#' @method describe default
`describe.default` <- `describe.Foreign0`

`counter` <- base::readRDS(to_native("fixtures/r6.rds" |> as.Character()))

print(describe(counter))