#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 3 (RC external, setRefClass) x column a (argument of a TypR function)
# of interop_matrix.md.
#
# Uses base::isS4 rather than print() as the oracle: a bare readRDS()'d RC
# instance has no defining generator registered in a fresh R session (the
# generator was never serialized, only the instance data), so its
# reference-class print dispatch is unavailable regardless of TypR -- a
# pure-R quirk unrelated to the soundness property under test. isS4()
# survives round-tripping and is exactly what a corrupting as.X()/struct()
# cast would flip to FALSE (see 0033's expect.md).
Counter <- Foreign


#' @method describe Foreign0
`describe.Foreign0` <- (function(x) {
from_bool(base::isS4(to_native(x)))
} |> as.Boolean()) |> as.Generic()
#' @method describe default
`describe.default` <- `describe.Foreign0`

`counter` <- base::readRDS(to_native("fixtures/rc.rds" |> as.Character()))

print(describe(counter))