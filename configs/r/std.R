add <- function (x, ...) UseMethod("add")
minus <- function (x, ...) UseMethod("minus")
mul <- function (x, ...) UseMethod("mul")
div <- function (x, ...) UseMethod("div")
add.default <- function(a, b) {a + b}
minus.default <- function(a, b) {a - b}
mul.default <- function(a, b) {a * b}
div.default <- function(a, b) {a / b}
map <- function(x, ...) { UseMethod('map') }
map.default <- sapply
filter.default <- function(vec, condition) {
  vec[condition(vec)]
}

append <- function(x, ...) { UseMethod('append') }
append.default <- function(a, e) { c(a, e) }

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


struct <- function(x, new_class) {
	class(x) <- union(class(x), new_class)
	#class(x) <- new_class
	return(x)
}

let_type <- function(x, new_class) {
  class(x) <- ""
  class(x) <- x |> new_class()
  return(x)
}

