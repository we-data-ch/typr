add <- function(a, b) {a + b}
minus <- function(a, b) {a - b}
mul <- function(a, b) {a * b}
div <- function(a, b) {a / b}
map <- function(x, ...) { UseMethod('map') }
map.default <- sapply
filter.default <- function(vec, condition) {
  vec[condition(vec)]
}

into.default <- function(element, vecteur) {
  return(element %in% vecteur)
}

dot <- function (x, ...) UseMethod("dot")

dot.default <- function(a, b) {
	a %*% b
}

get <- function(var, label) {
	var[[label]]
}

set <- function(var, label, val) {
	var[[label]] <- val;
	return(var)
}

join <- function(vec, sep) {
	paste(vec, collapse=sep)
}

sys.info <- function() { Sys.info() }
sys.getenv <- function() { Sys.getenv() }
sys.setenv <- function(var, val) { Sys.setenv(var = val) }
sys.time <- function() { Sys.time() }
sys.date <- function() { Sys.Date() }
sys.sleep <- function(n) { Sys.sleep(n) }
sys.which <- function(s) { Sys.which(s) }
sys.timezone <- function() { Sys.timezone() }
sys.setlocale <- function() { Sys.setlocale() }

