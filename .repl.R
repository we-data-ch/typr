source('a_std.R')
Array0 <- function(x) x |> struct(c('Array0', 'Array0', 'Any'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'Any'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'Any'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'Any'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'Any'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'Any'))
Function3 <- function(x) x |> struct(c('Function3', 'Function3', 'Any'))
Function3 <- function(x) x |> struct(c('Function3', 'Function3', 'Any'))
Function3 <- function(x) x |> struct(c('Function3', 'Function3', 'Any'))
Empty0 <- function(x) x |> struct(c('Empty0', 'Empty0', 'Any'))
Function1 <- function(x) x |> struct(c('Function1', 'Function1', 'Any'))
Function1 <- function(x) x |> struct(c('Function1', 'Function1', 'Any'))
Function1 <- function(x) x |> struct(c('Function1', 'Function1', 'Any'))
Function0 <- function(x) x |> struct(c('Function0', 'Function0', 'Any'))
Generic <- function(x) x |> struct(c('Generic', 'Generic', 'Any'))
Integer <- function(x) x |> struct(c('Integer', 'integer', 'Any'))
Character <- function(x) x |> struct(c('Character', 'character', 'Any'))
Number <- function(x) x |> struct(c('Number', 'numeric', 'Any'))
Boolean <- function(x) x |> struct(c('Boolean', 'logical', 'Any'))#' @export
expect_equal <- function(x, ...) UseMethod('expect_equal', x)
#' @export
reduce <- function(x, ...) UseMethod('reduce', x)
#' @export
sum <- function(x, ...) UseMethod('sum', x)
#' @export
expect_false <- function(x, ...) UseMethod('expect_false', x)


vec_apply(`+`, 1L |> Integer(), typed_vec(1L |> Integer(), 2L |> Integer(), 3L |> Integer()) |> Generic())