source('b_generic_functions.R')
source('c_types.R')
source('a_std.R', echo = FALSE)

Point <- function(x, y) {
  structure(list(x = x, y = y), class = c("Point", "list"))
}
`incr.Point` <- (function(p) {
Point(x = {
p$x + 1L |> Integer()
}, y = {
p$y + 1L |> Integer()
})
}) |> Generic()

incr(Point(x = 5L |> Integer(), y = 12L |> Integer()))