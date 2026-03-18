# --- Fonctions Système ---

sys.info      <- function() Sys.info()
sys.getenv    <- function() Sys.getenv()
sys.setenv    <- function(var, val) Sys.setenv(var = val)
sys.time      <- function() Sys.time()
sys.date      <- function() Sys.Date()
sys.sleep     <- function(n) Sys.sleep(n)
sys.which     <- function(s) Sys.which(s)
sys.timezone  <- function() Sys.timezone()
sys.setlocale <- function() Sys.setlocale()

# --- Utilitaires de Typage ---

#' @title Ajout d'une structure de classe
struct <- function(x, new_class, typed_dim = NULL) {
  if (is.null(x)) return(NULL)
  
  # Fusion propre des classes sans doublons
  class(x) <- unique(c(new_class, class(x)))
  
  if (!is.null(typed_dim)) {
    attr(x, "typed_dim") <- typed_dim
  }
  
  x
}

#' @title Changement forcé de type
let_type <- function(x, new_class) {
  # On retire les classes actuelles puis on applique le nouveau constructeur
  unclass(x) |> new_class()
}

# --- Génériques S3 ---

get <- function(a, ...) UseMethod("get")
apply <- function(X, ...) UseMethod("apply")
reduce <- function(vec, ...) UseMethod("reduce")

# --- Méthodes par défaut et types de base ---

apply.default <- function(X, ...) {
  # Si MARGIN est présent, c'est probablement un appel à base::apply
  args <- list(...)
  if (is.numeric(X) || is.array(X) || is.matrix(X)) {
    return(do.call(base::apply, c(list(X), args)))
  }
  base::apply(X, ...)
}

get.default <- function(a, name, ...) {
  if (is.list(a)) a[[name]] else base::get(name, pos = a, ...)
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

Integer <- function(x) {
  dim <- attr(x, "typed_dim") %||% length(x)
  struct(x, c("Integer", "integer", "Any", "Generic"), typed_dim = dim)
}

Number <- function(x) {
  dim <- attr(x, "typed_dim") %||% length(x)
  struct(x, c("Number", "numeric", "Any", "Generic"), typed_dim = dim)
}

Character <- function(x) {
  dim <- attr(x, "typed_dim") %||% length(x)
  struct(x, c("Character", "character", "Any", "Generic"), typed_dim = dim)
}

Boolean <- function(x) {
  dim <- attr(x, "typed_dim") %||% length(x)
  struct(x, c("Boolean", "logical", "Any", "Generic"), typed_dim = dim)
}

# Opérateur de coalescence nulle utilitaire
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- Affichage des types de base ---

print.Integer   <- function(x, ...) { cat(unclass(x), fill = TRUE); invisible(x) }
print.Character <- function(x, ...) { cat(unclass(x), fill = TRUE); invisible(x) }
print.Boolean   <- function(x, ...) { cat(unclass(x), fill = TRUE); invisible(x) }
print.Number    <- function(x, ...) { cat(unclass(x), fill = TRUE); invisible(x) }

# --- Opérateurs ---

`%==%.default` <- function(x, y) {
  unclass(x) == unclass(y)
}

# --- Classe typed_vec ---

typed_vec <- function(..., dim = 0) {
  x <- list(...)

  if (length(x) == 0) {
    return(structure(
      list(data = list()),
      class = "typed_vec",
      typed_dim = dim
    ))
  }

  # Vérification si tous les éléments sont déjà des typed_vec
  all_typed <- vapply(x, inherits, logical(1), what = "typed_vec")

  if (all(all_typed)) {
    combined_data <- unlist(lapply(x, `[[`, "data"), recursive = FALSE)
    return(structure(
      list(data = combined_data),
      class = "typed_vec",
      typed_dim = dim
    ))
  }

  structure(
    list(data = x),
    class = "typed_vec",
    typed_dim = dim
  )
}

length.typed_vec <- function(x) {
  length(x$data)
}

`[[.typed_vec` <- function(x, i) {
  if (is.character(i)) {
    lapply(x$data, `[[`, i)
  } else {
    x$data[[i]]
  }
}

get.typed_vec <- function(a, name) {
  a$data[[1]][[name]]
}

apply.typed_vec <- function(X, FUN, ...) {
  # Appliquer la fonction à chaque élément de data et ré-emballer
  typed_vec(lapply(X$data, FUN, ...))
}

vec_apply <- function(f, ...) {
  args <- list(...)
  
  # Normalisation en typed_vec
  args <- lapply(args, function(x) {
    if (!inherits(x, "typed_vec")) typed_vec(x) else x
  })

  lengths <- vapply(args, length, integer(1))
  n <- max(lengths)

  if (any(lengths == 0)) {
    return(typed_vec(dim = 0))
  }

  if (any(n %% lengths != 0)) {
    stop("Incompatible vector lengths for recycling")
  }

  # Recyclage des données
  recycled <- lapply(args, function(x) {
    if (length(x) == n) x$data else rep(x$data, length.out = n)
  })

  results <- vector("list", n)
  for (i in seq_len(n)) {
    elements <- lapply(recycled, `[[`, i)
    results[[i]] <- do.call(f, elements)
  }

  # Si tous les résultats sont des typed_vec, on les combine
  all_typed <- vapply(results, inherits, logical(1), what = "typed_vec")
  
  if (all(all_typed) && length(results) > 0) {
    combined_data <- unlist(lapply(results, `[[`, "data"), recursive = FALSE)
    return(structure(
      list(data = combined_data),
      class = "typed_vec",
      typed_dim = n
    ))
  }

  structure(
    list(data = results),
    class = "typed_vec",
    typed_dim = n
  )
}

vec_apply_fun <- function(fun_vec, ...) {
  if (!inherits(fun_vec, "typed_vec")) fun_vec <- typed_vec(fun_vec)
  
  args <- lapply(list(...), function(x) {
    if (!inherits(x, "typed_vec")) typed_vec(x) else x
  })

  lengths <- c(length(fun_vec), vapply(args, length, integer(1)))
  n <- max(lengths)

  if (any(lengths == 0)) return(typed_vec(dim = 0))
  if (any(n %% lengths != 0)) stop("Incompatible vector lengths")

  funs <- if (length(fun_vec) == n) fun_vec$data else rep(fun_vec$data, length.out = n)
  recycled_args <- lapply(args, function(x) {
    if (length(x) == n) x$data else rep(x$data, length.out = n)
  })

  results <- vector("list", n)
  for (i in seq_len(n)) {
    f <- funs[[i]]
    params <- lapply(recycled_args, `[[`, i)
    results[[i]] <- do.call(f, params)
  }

  all_typed <- vapply(results, inherits, logical(1), what = "typed_vec")
  
  if (all(all_typed) && length(results) > 0) {
    combined_data <- unlist(lapply(results, `[[`, "data"), recursive = FALSE)
    return(structure(
      list(data = combined_data),
      class = "typed_vec",
      typed_dim = n
    ))
  }

  structure(
    list(data = results),
    class = "typed_vec",
    typed_dim = n
  )
}

reduce.typed_vec <- function(vec, f, init = NULL) {
  if (!inherits(vec, "typed_vec")) vec <- typed_vec(vec)
  
  n <- length(vec)
  if (n == 0) {
    if (is.null(init)) stop("Cannot reduce empty vector without initial value")
    return(init)
  }
  
  # Utilisation de la fonction Reduce de base pour plus de robustesse
  res <- if (is.null(init)) {
    Reduce(f, vec$data)
  } else {
    Reduce(f, vec$data, init = init)
  }
  
  # Si le résultat est un typed_vec, on extrait la donnée (comportement original)
  if (inherits(res, "typed_vec")) res <- res$data[[1]]
  
  structure(
    list(data = list(res)),
    class = "typed_vec",
    typed_dim = 1
  )
}

sum.typed_vec <- function(x, ...) {
  reduce(x, `+`)
}

print.typed_vec <- function(x, ...) {
  dims <- attr(x, "typed_dim")
  data <- x$data
  n <- length(data)

  if (n == 0) {
    cat("typed_vec(0)\n")
    return(invisible(x))
  }

  # Si pas de dimensions ou 1D, affichage linéaire amélioré
  if (is.null(dims) || length(dims) <= 1 || (length(dims) == 1 && dims == 0)) {
    cat("typed_vec [", n, "]\n", sep = "")
    for (i in seq_len(n)) {
      cat("[", i, "] ", sep = "")
      el <- data[[i]]
      if (is.function(el)) cat("<function>\n") else print(el)
    }
    return(invisible(x))
  }

  # --- Affichage Multidimensionnel ---
  
  # Helper pour formater un élément individuel
  format_el <- function(el) {
    if (is.function(el)) return("<fun>")
    if (inherits(el, "typed_vec")) return(paste0("<vec:", length(el), ">"))
    s <- format(el)
    if (length(s) > 1) paste0(s[1], "..") else s
  }

  # Helper pour afficher une tranche 2D (Matrice)
  print_slice2d <- function(slice_data, nr, nc, header = "") {
    if (nchar(header) > 0) cat(header, "\n")
    
    # Pré-formatage pour l'alignement
    formatted <- vapply(slice_data, format_el, character(1))
    
    # Remplissage par ligne (row first)
    mat_form <- matrix(formatted, nrow = nr, ncol = nc, byrow = TRUE)
    
    # On utilise explicitement base::apply pour éviter le conflit S3
    col_widths <- base::apply(mat_form, 2, function(col) max(nchar(col), 4))
    
    # En-tête des colonnes
    cat("     ")
    for (j in 1:nc) {
      cat(sprintf("[%*s,%d]", col_widths[j] - 2, "", j), " ")
    }
    cat("\n")
    
    # Lignes
    for (i in 1:nr) {
      cat(sprintf("[%d,] ", i))
      for (j in 1:nc) {
        cat(sprintf("%*s", col_widths[j], mat_form[i, j]), " ")
      }
      cat("\n")
    }
  }

  if (length(dims) == 2) {
    print_slice2d(data, dims[1], dims[2])
  } else {
    # Pour 3D et plus : on itère sur les dimensions supérieures
    slice_size <- dims[1] * dims[2]
    num_slices <- n / slice_size
    
    high_dims <- dims[3:length(dims)]
    
    for (s in 1:num_slices) {
      # Calcul des indices pour les dimensions > 2 (basé sur le stockage column-major de R)
      # On utilise arrayInd pour retrouver les coordonnées multi-D à partir de l'index linéaire
      coords <- arrayInd(s, high_dims)
      header <- paste0(", , ", paste(coords, collapse = ", "))
      
      start_idx <- (s - 1) * slice_size + 1
      end_idx <- s * slice_size
      
      print_slice2d(data[start_idx:end_idx], dims[1], dims[2], header = header)
      if (s < num_slices) cat("\n")
    }
  }

  invisible(x)
}
