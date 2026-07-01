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
  class(x) <- unique(c(class(x), new_class))
  
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

#' @title Fusion shallow pour le spread operator (`{ ...x, a = 1 }`)
spread <- function(base, override) {
  out <- base
  for (nm in names(override)) {
    out[[nm]] <- override[[nm]]
  }
  out
}

#' @title Fusion explicite/spread pour les constructeurs record (`TypeName:{ ...x }`)
typr_spread_record <- function(explicit, spread) {
  if (is.null(spread)) {
    return(explicit)
  }
  if (!is.list(spread)) {
    stop("`.spread` must be a list (record) in TypR constructors.")
  }
  merged <- spread
  merged[names(explicit)] <- explicit
  merged
}

# --- Génériques S3 ---

get <- function(a, ...) UseMethod("get")
apply <- function(X, ...) UseMethod("apply")
reduce <- function(vec, ...) UseMethod("reduce")
as_vec <- function(x, ...) UseMethod("as_vec")
max <- function(a, ...) UseMethod("max")
min <- function(a, ...) UseMethod("min")
replace <- function(a, ...) UseMethod("replace")
append <- function(a, ...) UseMethod("append")
plot <- function(a, ...) UseMethod("plot")
expect_equal <- function(a, ...) UseMethod("expect_equal")
set <- function(a, ...) UseMethod("set")
update <- function(a, ...) UseMethod("update")
version <- function(a, ...) UseMethod("version")
state <- function(a, ...) UseMethod("state")
map <- function(a, ...) UseMethod("map")
derive <- function(a, ...) UseMethod("derive")
unwrap <- function(a, ...) UseMethod("unwrap")
unwrap_or <- function(a, ...) UseMethod("unwrap_or")
expect <- function(a, ...) UseMethod("expect")
is_some <- function(a, ...) UseMethod("is_some")
is_none <- function(a, ...) UseMethod("is_none")
factor <- function(a, ...) UseMethod("factor")
annotate_factor <- function(a, ...) UseMethod("annotate_factor")
nlevels <- function(a, ...) UseMethod("nlevels")

# User validator hook for record types: `as.T` calls validate() after the
# internal validate_T. Default is identity; users extend it with validate.T.
validate <- function(x, ...) UseMethod("validate")
validate.default <- function(x) x

# --- Méthodes par défaut et types de base ---

max.default <- function(a, ...) base::max(a, ...)
min.default <- function(a, ...) base::min(a, ...)
replace.default <- function(a, ...) base::replace(a, ...)
append.default <- function(a, ...) base::append(a, ...)
plot.default <- function(a, ...) base::plot(a, ...)
expect_equal.default <- function(a, ...) testthat::expect_equal(a, ...)

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

# Arrays (and atomic vectors) are already iterable: `as_vec` is the identity,
# R iterates them natively in `for (x in ...)`.
as_vec.default <- function(x, ...) x

set_at.default <- function(a, i, val) replace(a, i, val)

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

as.Generic <- function(x) x |> struct(c('Generic', 'Any'))

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

sum.default <- function(x, ...) base::sum(x, ...)

sum.typed_vec <- function(x, ...) {
  reduce(x, `+`)
}

vec_reduce <- function(vec, f, init = NULL) reduce(vec, f, init)

extend <- function(vec, ...) UseMethod("extend")

extend.typed_vec <- function(vec, x) {
  structure(
    list(data = c(vec$data, list(x))),
    class = "typed_vec",
    typed_dim = length(vec) + 1L
  )
}

vec_extend <- function(vec, x) extend(vec, x)

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

# --- Option ---
# .Some(v)/.None transpile to structure(list('Some', body = v), class =
# c('Some', 'Option', 'Tag', 'list')) / structure(list('None'), class =
# c('None', 'Option', 'Tag', 'list')) — see the union variant constructor
# pipeline. Both variants share the 'Option' class, so a single method
# dispatching on it (rather than separate .Some/.None methods) covers both.

unwrap.Option <- function(value, ...) {
  if (inherits(value, "Some")) value$body else stop("The value is not unwrappable.")
}

expect.Option <- function(value, msg, ...) {
  if (inherits(value, "Some")) value$body else stop(msg)
}

unwrap_or.Option <- function(value, alternative, ...) {
  if (inherits(value, "Some")) value$body else alternative
}

is_some.Option <- function(value, ...) inherits(value, "Some")

is_none.Option <- function(value, ...) inherits(value, "None")

# --- Factor ---

# Standard constructor: looks up `x` in `levels` and returns the matching
# 1-based index (delegates the bounds check to annotate_factor).
# `factor` is also a base-R name; without `.default`, generic_functions.R's
# auto-generated UseMethod stub would shadow it with no fallback method.
factor.default <- function(x, levels, ...) {
  idx <- match(x, levels)
  if (is.na(idx)) {
    stop(paste0("'", x, "' is not among Factor levels: [", paste(levels, collapse = ", "), "]"))
  }
  annotate_factor(idx, levels)
}

# Annotator: wraps an integer index as an R factor with the given levels.
# Validates that 1 <= x <= length(levels).
annotate_factor.default <- function(x, levels, ...) {
  if (x < 1L || x > length(levels)) {
    stop(paste0("index ", x, " out of bounds for Factor[", paste(levels, collapse = ", "), "]"))
  }
  structure(x, class = "factor", levels = levels)
}

# base::nlevels is a plain function (length(levels(x))), not S3-generic, so
# it has no registered nlevels.factor method for generic_functions.R's
# UseMethod stub to dispatch to. .default replicates base's own definition.
nlevels.default <- function(x, ...) length(levels(x))

# --- State ---
# Real shared mutable state, backed by an R environment (reference type).
# `version` auto-increments on every set()/update(). The constructor
# captures class(value) as a per-instance runtime fingerprint; set/update
# validate new values against it (defense-in-depth; the TypR static type
# checker is the real guarantee against misuse in well-typed code).
#
# `state` is defined as `state.default` (not a plain function): the project
# build's generated R/generic_functions.R emits `state <- function(x, ...)
# UseMethod('state', x)` for every typed stdlib name, which would otherwise
# shadow a plain `state <- function(value) {...}` definition here. Dispatch
# on .default catches every input class, since the constructor accepts any T.
state.default <- function(value, ...) {
  e <- new.env(parent = emptyenv())
  e$value <- value
  e$version <- 0L
  e$.tag <- class(value)
  structure(e, class = "State")
}

get.State <- function(s, ...) s$value

set.State <- function(s, value, ...) {
  if (!identical(class(value), s$.tag)) {
    stop(paste0(
      "State type mismatch: expected ", paste(s$.tag, collapse = "/"),
      ", got ", paste(class(value), collapse = "/")
    ))
  }
  s$value <- value
  s$version <- s$version + 1L
  invisible(NULL)
}

update.State <- function(s, f, ...) {
  set.State(s, f(s$value))
}

version.State <- function(s, ...) s$version

map.State <- function(s, f, ...) state(f(s$value))

derive.State <- function(s, f, ...) state(f(s$value))

# --- Interop (Niveau 0) ---
# to_native: lowering récursif TypR→R plat à la frontière des packages R externes.
# from_native: lifting R natif→TypR piloté par un descripteur de type ("int","num","char","bool","Any").
# Ces fonctions sont la fondation du plan d'interop; elles ne modifient pas l'état TypR interne.

to_native <- function(x, ...) UseMethod("to_native")

to_native.default   <- function(x, ...) x
to_native.Integer   <- function(x, ...) as.integer(unclass(x))
to_native.Number    <- function(x, ...) as.numeric(unclass(x))
to_native.Character <- function(x, ...) as.character(unclass(x))
to_native.Boolean   <- function(x, ...) as.logical(unclass(x))

to_native.typed_vec <- function(x, ...) {
  if (length(x$data) == 0L) return(list())
  elems <- lapply(x$data, to_native)
  # Collapse to atomic vector when all elements share the same base storage type
  if (all(vapply(elems, is.atomic, logical(1))) &&
      length(unique(vapply(elems, typeof, character(1)))) == 1L) {
    do.call(c, elems)
  } else {
    elems
  }
}

# Handles records (class c("T","list")) and Tag variants not matched by a more specific method.
to_native.list <- function(x, ...) {
  result <- lapply(unclass(x), to_native)
  class(result) <- NULL
  result
}

to_native.Some  <- function(x, ...) to_native(x$body)
to_native.None  <- function(x, ...) NULL
to_native.State <- function(x, ...) to_native(get.State(x))

from_native <- function(x, ...) UseMethod("from_native")

# type_desc: "int" | "num" | "char" | "bool" | "Any" (default = identity)
from_native.default <- function(x, type_desc = "Any", ...) {
  if (is.null(x)) return(NULL)
  switch(type_desc,
    "int"  = Integer(as.integer(x)),
    "num"  = Number(as.numeric(x)),
    "char" = Character(as.character(x)),
    "bool" = Boolean(as.logical(x)),
    x
  )
}

# Typed lifting helpers: convert a plain R value to a specific TypR type.
# These are the idiomatic way to bring native R values into TypR-typed code.
from_int  <- function(x, ...) UseMethod("from_int")
from_num  <- function(x, ...) UseMethod("from_num")
from_char <- function(x, ...) UseMethod("from_char")
from_bool <- function(x, ...) UseMethod("from_bool")

from_int.default  <- function(x, ...) Integer(as.integer(x))
from_num.default  <- function(x, ...) Number(as.numeric(x))
from_char.default <- function(x, ...) Character(as.character(x))
from_bool.default <- function(x, ...) Boolean(as.logical(x))

# --- Interop (Niveau 2) ---
# from_nullable / to_nullable: bridge between R NULL and TypR Option<T>.
# Use from_nullable when an @extern function may return NULL to represent absence.
# Use to_nullable when passing an Option<T> to an external function expecting NULL.

from_nullable <- function(x, ...) UseMethod("from_nullable")
from_nullable.default <- function(x, ...) {
  if (is.null(x))
    structure(list("None"), class = c("None", "Option", "Tag", "list"))
  else
    structure(list("Some", body = x), class = c("Some", "Option", "Tag", "list"))
}

to_nullable <- function(x, ...) UseMethod("to_nullable")
to_nullable.Option <- function(x, ...) if (inherits(x, "Some")) x$body else NULL
to_nullable.default <- function(x, ...) x

# Foreign: identity function that serves as the runtime root for opaque Foreign<T> aliases.
# `type DataFrame <- Foreign<Any>` compiles to `DataFrame <- Foreign` in R, so calling
# `DataFrame(x)` returns `x` unchanged — native R objects (data.frame, R6, S4, etc.)
# pass through without boxing. to_native.default already handles Foreign values correctly.
Foreign <- function(x, ...) x
