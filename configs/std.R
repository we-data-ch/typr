add.integer <- function(a, b) {a + b}
add.numeric <- function(a, b) {a + b}
minus.integer <- function(a, b) {a - b}
minus.numeric <- function(a, b) {a - b}
mul.integer <- function(a, b) {a * b}
mul.numeric <- function(a, b) {a * b}
div.integer <- function(a, b) {a / b}
div.numeric <- function(a, b) {a / b}
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

get <- function(var, label) {
	var[[label]]
}

set <- function(var, label, val) {
	var[[label]] <- val;
	return(var)
}
