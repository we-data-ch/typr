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

# Fonction générique de concaténation
concat <- function(..., dim) {
  UseMethod("concat")
}

# Méthode par défaut pour les vecteurs classiques
concat.default <- function(..., dim) {
  objets <- list(...)
  
  # Si un seul objet, le retourner tel quel
  if (length(objets) == 1) {
    return(objets[[1]])
  }
  
  # Récupérer toutes les classes uniques
  classes <- unique(unlist(lapply(objets, class)))
  
  # Utiliser c() pour les vecteurs
  resultat <- do.call(c, objets)

  resultat <- array(resultat, dim = dim)
  
  # Ajouter les classes sur le résultat
  class(resultat) <- c(classes, class(resultat))
  
  return(resultat)
}

# Méthode pour data.frame
concat.data.frame <- function(..., dim) {
  objets <- list(...)
  
  # Si un seul objet, le retourner tel quel
  if (length(objets) == 1) {
    return(objets[[1]])
  }
  
  classes <- class(objets[[1]])
  
  # Utiliser rbind pour combiner les data.frames
  resultat <- do.call(rbind, objets)
  
  # Ajouter les classes sur le résultat (data.frame reste à la fin)
  if (length(classes) > 0) {
    class(resultat) <- c(classes, "data.frame")
  }
  
  return(resultat)
}
