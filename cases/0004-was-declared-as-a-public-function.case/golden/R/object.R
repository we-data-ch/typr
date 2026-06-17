#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include circle.R
object <- new.env(parent = emptyenv())
local({
Circle <- circle$Circle
Object <- Circle
object$Object <- Object
})