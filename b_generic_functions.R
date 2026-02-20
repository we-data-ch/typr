#' @export
reduce <- function(x, ...) UseMethod('reduce', x)
#' @export
expect_equal <- function(x, ...) UseMethod('expect_equal', x)
#' @export
sum <- function(x, ...) UseMethod('sum', x)
#' @export
expect_false <- function(x, ...) UseMethod('expect_false', x)
#' @export
double <- function(x, ...) UseMethod('double', x)
