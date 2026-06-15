#' @include std.R
#' @include generic_functions.R
#' @include types.R
leaf <- new.env(parent = emptyenv())
local({
#' @method g
`g` <- (function() {
1L |> as.Integer()
} |> as.Integer()) |> as.Function0()

leaf$g <- g
})