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
  if (is.null(x)) {
	  return(x)
  }
  
  old <- oldClass(x)
  
  if (is.null(old)) {
    class(x) <- new_class
  } else {
    class(x) <- union(old, new_class)
  }
  
  return(x)
}

let_type <- function(x, new_class) {
  class(x) <- ""
  class(x) <- x |> new_class()
  return(x)
}

typed_vec <- function(...) {
  x <- list(...)
  
  # Vérifier si tous les arguments héritent de "typed_vec"
  all_typed <- all(vapply(x, function(item) inherits(item, "typed_vec"), logical(1)))
  
  if (all_typed && length(x) > 0) {
    # Combiner les paramètres data de chaque typed_vec
    combined_data <- unlist(lapply(x, function(item) item$data), recursive = FALSE)
    
    return(structure(
      list(data = combined_data),
      class = "typed_vec"
    ))
  }
  
  # Sinon, retourner la structure normale
  structure(
    list(data = x),
    class = "typed_vec"
  )
}

length.typed_vec <- function(x) {
  length(x$data)
}

`[[.typed_vec` <- function(x, i) {
  x$data[[i]]
}


vec_apply <- function(f, ...) {
  args <- list(...)
  
  # Appliquer typed_vec sur les arguments qui n'héritent pas de "typed_std"
  args <- lapply(args, function(x) {
    if (!inherits(x, "typed_vec")) {
      typed_vec(x)
    } else {
      x
    }
  })

  lengths <- vapply(args, length, integer(1))
  n <- max(lengths)

  if (any(lengths == 0)) {
    return(structure(
      list(data = list()),
      class = "typed_vec"
    ))
  }

  # Optionnel : sécurité façon R
  if (any(n %% lengths != 0)) {
    stop("Incompatible vector lengths")
  }

  # Recyclage
  recycled <- lapply(args, function(x) {
    if (length(x) == n) {
      x$data
    } else {
      rep(x$data, length.out = n)
    }
  })

	results <- vector("list", n)
	for (i in seq_len(n)) {
	  # Extraire les éléments à la position i de chaque argument
	  elements <- lapply(recycled, `[[`, i)
	  # Appeler f qui fera son propre dispatch S3
	  results[[i]] <- do.call(f, elements)
	}


  # Vérifier si tous les arguments héritent de "typed_vec"
  all_typed <- all(vapply(results, function(item) inherits(item, "typed_vec"), logical(1)))
  
  if (all_typed && length(results) > 0) {
    # Combiner les paramètres data de chaque typed_vec
    combined_data <- unlist(lapply(results, function(item) item$data), recursive = FALSE)
    
    return(structure(
      list(data = combined_data),
      class = "typed_vec"
    ))
  }

  structure(
    list(
      data = results
      #data = do.call(Map, c(list(f), recycled))
    ),
    class = "typed_vec"
  )
}

vec_apply_fun <- function(fun_vec, ...) {
  # Appliquer typed_vec sur fun_vec s'il n'hérite pas de "typed_vec"
  if (!inherits(fun_vec, "typed_vec")) {
    fun_vec <- typed_vec(fun_vec)
  }
  
  args <- list(...)
  
  # Appliquer typed_vec sur les arguments qui n'héritent pas de "typed_vec"
  args <- lapply(args, function(x) {
    if (!inherits(x, "typed_vec")) {
      typed_vec(x)
    } else {
      x
    }
  })

  # Toutes les longueurs
  lengths <- c(length(fun_vec), vapply(args, length, integer(1)))
  n <- max(lengths)

  if (any(lengths == 0)) {
    return(structure(
      list(data = list()),
      class = "typed_vec"
    ))
  }

  # Sécurité optionnelle
  if (any(n %% lengths != 0)) {
    stop("Incompatible vector lengths")
  }

  # Recyclage
  funs <- if (length(fun_vec) == n)
    fun_vec$data
  else
    rep(fun_vec$data, length.out = n)

  recycled_args <- lapply(args, function(x) {
    if (length(x) == n) x$data
    else rep(x$data, length.out = n)
  })

  # Application élément-wise avec results intermédiaires
  results <- vector("list", n)
  for (i in seq_len(n)) {
    f <- funs[[i]]
    params <- lapply(recycled_args, `[[`, i)
    # Appeler f qui fera son propre dispatch S3
    results[[i]] <- do.call(f, params)
  }

  # Vérifier si tous les éléments de results héritent de "typed_vec"
  all_typed <- all(vapply(results, function(item) inherits(item, "typed_vec"), logical(1)))
  
  if (all_typed && length(results) > 0) {
    # Combiner les paramètres data de chaque typed_vec
    combined_data <- unlist(lapply(results, function(item) item$data), recursive = FALSE)
    
    return(structure(
      list(data = combined_data),
      class = "typed_vec"
    ))
  }

  structure(
    list(data = results),
    class = "typed_vec"
  )
}

reduce.typed_vec <- function(vec, f, init = NULL) {
  # Appliquer typed_vec sur vec s'il n'hérite pas de "typed_vec"
  if (!inherits(vec, "typed_vec")) {
    vec <- typed_vec(vec)
  }
  
  n <- length(vec)
  
  # Si le vecteur est vide
  if (n == 0) {
    if (is.null(init)) {
      stop("Cannot reduce empty vector without initial value")
    }
    return(init)
  }
  
  # Déterminer la valeur initiale de l'accumulateur
  if (is.null(init)) {
    # Commencer avec le premier élément
    accumulator <- vec$data[[1]]
    start_index <- 2
  } else {
    # Commencer avec la valeur initiale fournie
    accumulator <- init
    start_index <- 1
  }
  
  # Si on a déjà tout consommé
  if (start_index > n) {
    return(accumulator)
  }
  
  # Réduction itérative
  for (i in start_index:n) {
    # Appeler f qui fera son propre dispatch S3
    accumulator <- f(accumulator, vec$data[[i]])
    if (inherits(accumulator, "typed_vec")) {
      accumulator <- accumulator$data[[1]]
    }
  }
  
  return(structure(
    list(data = list(accumulator)),
    class = "typed_vec"
  ))
}

sum.typed_vec <- function(x, ...) {
	reduce(x, `+`)
}

print.typed_vec <- function(x, ...) {
  n <- length(x$data)

  # Cas spécial : liste vide
  if (n == 0) {
    cat("Empty typed_vec\n")
    return(invisible(x))
  }

  # Cas spécial : longueur 1, afficher directement le contenu
  if (n == 1) {
    el <- x$data[[1]]
    
    if (is.function(el)) {
      cat("<function>\n")
    } else {
      print(el)
    }
    
    return(invisible(x))
  }

  # Cas général : longueur > 1
  cat("typed_vec [", n, "]\n", sep = "")

  for (i in seq_len(n)) {
    cat("[", i, "] ", sep = "")
    el <- x$data[[i]]

    # Délégation au print S3 de l'élément
    if (is.function(el)) {
      # Affichage plus compact pour les fonctions
      fname <- tryCatch(
        deparse(substitute(el)),
        error = function(e) "<function>"
      )
      cat("<function>\n")
    } else {
      print(el)
    }

    if (i < n) cat("\n")
  }

  invisible(x)
}

get.typed_vec <- function(a, name) {
	a$data[[1]][[name]]
}

get.data <- function(a, name) {
	a$data[[1]]
}

get.list <- function(a, name) {
	a$data[[1]][[name]]
}

get.any <- function(a, name) {
	a[[name]]
}

print.Integer <- function(i) {
	cat(unclass(i))
	invisible(i)
}

print.Character <- function(c) {
	cat(unclass(c))
	invisible(c)
}

print.Boolean <- function(b) {
	cat(unclass(b))
	invisible(b)
}

print.Number <- function(n) {
	cat(unclass(n))
	invisible(n)
}

`%==%.default` <- function(x, y) {
	unclass(x) == unclass(y)
}

