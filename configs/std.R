add <- function(a, b) {a + b}
minus <- function(a, b) {a - b}
mul <- function(a, b) {a * b}
div <- function(a, b) {a / b}
map <- sapply
filter <- function(vec, condition) {
  vec[condition(vec)]
}
into <- function(element, vecteur) {
  return(element %in% vecteur)
}
dot <- function(a, b) {
	a %*% b
}
