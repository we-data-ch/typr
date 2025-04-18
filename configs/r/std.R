add <- function(x, ...) { UseMethod('add') }
minus <- function(x, ...) { UseMethod('minus') }
mul <- function(x, ...) { UseMethod('mul') }
div <- function(x, ...) { UseMethod('div') }
map <- function(x, ...) { UseMethod('map') }
into <- function(x, ...) { UseMethod('into') }
dot <- function(x, ...) { UseMethod('dot') }
add.integer <- function(a, b) {a + b}
add.numeric <- function(a, b) {a + b}
minus.integer <- function(a, b) {a - b}
minus.numeric <- function(a, b) {a - b}
mul.integer <- function(a, b) {a * b}
mul.numeric <- function(a, b) {a * b}
div.integer <- function(a, b) {a / b}
div.numeric <- function(a, b) {a / b}
map.default <- sapply
filter.default <- function(vec, condition) {
  vec[condition(vec)]
}

into.default <- function(element, vecteur) {
  return(element %in% vecteur)
}

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

