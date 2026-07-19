#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Column i (module boundary) x row 2 (S4, `Matrix::Matrix`) of interop_matrix.md.
# soundness_plan.md D.2: a @pub fn whose dispatch param is Foreign<T> must be
# reachable from OUTSIDE the module that declares it.
MatrixS4 <- Foreign

describe <- function(x, ...) UseMethod("describe")
Reporter <- new.env(parent = emptyenv())
local({
#' @method describe Foreign0
`describe.Foreign0` <- (function(x) {
print(x)
1L |> as.Integer()
} |> as.Integer()) |> as.Generic()
#' @method describe default
`describe.default` <- `describe.Foreign0`

Reporter$describe.Foreign0 <- describe.Foreign0
registerS3method("describe", "Foreign0", describe.Foreign0)
Reporter$describe.default <- describe.default
})
Reporter$describe <- describe
describe.Foreign0 <- Reporter$describe.Foreign0
describe.default <- Reporter$describe.default
describe <- Reporter$describe
`m` <- base::readRDS(to_native("fixtures/matrix_s4.rds" |> as.Character()))

print(describe(m))