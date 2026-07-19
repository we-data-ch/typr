#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Found while attempting Phase D.3 of soundness_plan.md (column j: generic
# instantiation, `id(foreign_val)`). NOT Foreign<T>-specific: any top-level
# generic function declaration in project mode hits this, foreign value or
# not. Reproduces from the *definition* alone -- id is never called here.
#' @method id Generic
`id.Generic` <- (function(x) {
x
} |> as.Generic()) |> as.Generic()

print("built ok" |> as.Character())