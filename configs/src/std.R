add <- function (x, ...) UseMethod("add")
minus <- function (x, ...) UseMethod("minus")
mul <- function (x, ...) UseMethod("mul")
div <- function (x, ...) UseMethod("div")
add.default <- function(a, b) {a + b}
minus.default <- function(a, b) {a - b}
mul.default <- function(a, b) {a * b}
div.default <- function(a, b) {a / b}

# For sequence
append <- function(x, ...) { UseMethod('append') }
append.default <- function(a, e) { c(a, list(e)) }

modify <- function(x, ...) { UseMethod('modify') }
modify.default <- function(a, i, v) { a[[i]] <- v; v }

remove <- function(x, ...) { UseMethod('remove') }
remove.default <- function(a, i) { a <- a[-i]; a }

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

concatenate_s3 <- function(...) {
  # Récupérer tous les arguments
  objects <- list(...)
  
  # Vérifier qu'il y a au moins un objet
  if (length(objects) == 0) {
    stop("At least one object must be provided")
  }
  
  # Vérifier que tous les objets ont la même classe
  first_class <- class(objects[[1]])
  for (i in seq_along(objects)) {
    if (!identical(class(objects[[i]]), first_class)) {
      stop("Each object should have exactly the same class")
    }
  }
  
  # Sauvegarder la classe
  original_class <- first_class
  
  # Combiner tous les objets avec do.call et c()
  result <- do.call(c, objects)
  
  # Restaurer la classe
  class(result) <- original_class
  
  return(result)
}
