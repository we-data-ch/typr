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
    stop("Cannot assign class to NULL object")
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
  if (inherits(x, "typed_vec")) return(x)

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

  structure(
    list(
      data = do.call(Map, c(list(f), recycled))
    ),
    class = "typed_vec"
  )
}


vec_apply_fun <- function(fun_vec, ...) {
  # Appliquer typed_vec sur fun_vec s'il n'hérite pas de "typed_std"
  if (!inherits(fun_vec, "typed_vec")) {
    fun_vec <- typed_vec(fun_vec)
  }
  
  args <- list(...)
  
  # Appliquer typed_vec sur les arguments qui n'héritent pas de "typed_std"
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

  # Application élément-wise
  result <- vector("list", n)
  for (i in seq_len(n)) {
    f <- funs[[i]]
    params <- lapply(recycled_args, `[[`, i)
    result[[i]] <- do.call(f, params)
  }

  structure(
    list(data = result),
    class = "typed_vec"
  )
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
    cat("[[", i, "]]\n", sep = "")
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

get.list <- function(a, name) {
	a[[name]]
}

get.any <- function(a, name) {
	a[[name]]
}


print.Integer <- function(i) {
	print(unclass(i))
	invisible(i)
}

print.Character <- function(c) {
	print(unclass(c))
	invisible(c)
}

print.Boolean <- function(b) {
	print(unclass(b))
	invisible(b)
}

print.Number <- function(n) {
	print(unclass(n))
	invisible(n)
}
