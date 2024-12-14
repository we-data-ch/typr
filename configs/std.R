add <- function(a, b) {a + b}
map <- sapply
filter <- function(vec, condition) {
  vec[condition(vec)]
}
