source('a_std.R')
Function3 <- function(x) x |> struct(c('Function3', 'Function3', 'any'))
Function3 <- function(x) x |> struct(c('Function3', 'Function3', 'any'))
Empty0 <- function(x) x |> struct(c('Empty0', 'Empty0', 'integer', 'any', 'Function0'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'any'))
Function2 <- function(x) x |> struct(c('Function2', 'Function2', 'any'))
Vector0 <- function(x) x |> struct(c('Vector0', 'Vec[#M, T]', 'any'))
Function0 <- function(x) x |> struct(c('Function0', 'Function0', 'any'))
Generic <- function(x) x |> struct(c('Generic', 'Generic', 'any'))
Vector1 <- function(x) x |> struct(c('Vector1', 'Vec[#M, T]', 'any'))
Function1 <- function(x) x |> struct(c('Function1', 'Function1', 'any'))
Function1 <- function(x) x |> struct(c('Function1', 'Function1', 'any'))
Function1 <- function(x) x |> struct(c('Function1', 'fn(Vec[#M, T], Vec[#M, T]) -> Vec[#M, T]', 'any'))
Function4 <- function(x) x |> struct(c('Function4', 'Function4', 'any'))
Function4 <- function(x) x |> struct(c('Function4', 'Function4', 'any'))
Array0 <- function(x) x |> struct(c('Array0', 'Array0', 'any'))
Vector1 <- function(x) x |> struct(c('Vector1', 'Vec[#M, T]', 'any'))
Function3 <- function(x) x |> struct(c('Function3', 'Function3', 'any'))
Integer <- function(x) x |> struct(c('Integer', 'integer', 'any'))
Character <- function(x) x |> struct(c('Character', 'character', 'any'))
Number <- function(x) x |> struct(c('Number', 'numeric', 'any'))
Boolean <- function(x) x |> struct(c('Boolean', 'logical', 'any'))#' @export
my_func <- function(x, ...) UseMethod('my_func', x)

my_func <- (function(a, b) {
a + b
} |> Integer()) |> Function0()
my_func <- (function(a, b) {
a + b
} |> Integer()) |> Function0()