# Localisation du binaire TypR.
#
# Trois sources, dans l'ordre :
#   1. le binaire embarqué dans le package (celui que la CI y place au moment
#      de construire le tarball de release — même version que le package) ;
#   2. l'option R `typr.path`, pour pointer un binaire de développement ;
#   3. le PATH du système.
#
# Les binaires ne sont plus commités dans le dépôt : une installation depuis
# les sources n'en contient donc pas, et retombe sur (2) ou (3).

get_executable <- function() {
  if (.Platform$OS.type == "windows") "typr.exe" else "typr"
}

#' Chemin du binaire TypR utilisé par le package
#' @export
typr_path <- function() {
  bundled <- system.file("bin", get_executable(), package = "typr.runner")
  if (nzchar(bundled) && file.exists(bundled)) {
    return(bundled)
  }

  configured <- getOption("typr.path", default = "")
  if (nzchar(configured) && file.exists(configured)) {
    return(configured)
  }

  on_path <- Sys.which("typr")
  if (nzchar(on_path)) {
    return(unname(on_path))
  }

  stop(
    "Binaire TypR introuvable.\n",
    "  - installez le package depuis une release GitHub (binaire inclus), ou\n",
    "  - installez typr (cargo install typr / Docker / release), ou\n",
    "  - pointez-le explicitement : options(typr.path = \"/chemin/vers/typr\")",
    call. = FALSE
  )
}

#' Version du binaire TypR utilisé
#' @export
typr_version <- function() {
  out <- system2(typr_path(), "--version", stdout = TRUE, stderr = TRUE)
  sub("^\\S+\\s+", "", out[1])
}

# Avertit si le binaire trouvé n'est pas de la même version que le package.
# Compilateur et runner sont versionnés ensemble : un écart signale une
# installation incohérente.
check_version_match <- function() {
  pkg <- as.character(utils::packageVersion("typr.runner"))
  bin <- tryCatch(typr_version(), error = function(e) NA_character_)
  if (!is.na(bin) && !identical(pkg, bin)) {
    warning(
      sprintf("typr.runner %s utilise un binaire typr %s — versions désalignées.", pkg, bin),
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Exécute une sous-commande typr, dans le bon répertoire de travail.
run_typr <- function(args, dir = NULL) {
  check_version_match()
  exe <- typr_path()

  if (!is.null(dir)) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE)
  }

  result <- system2(exe, args, stdout = TRUE, stderr = TRUE)
  cat(paste(result, collapse = "\n"), "\n")
  invisible(result)
}

# Répertoire du document actif dans RStudio, ou NULL.
active_document_dir <- function(require_saved = TRUE) {
  context <- rstudioapi::getActiveDocumentContext()
  if (is.null(context$path) || context$path == "") {
    if (require_saved) {
      rstudioapi::showDialog(
        title = "Error",
        message = "Select an opened and saved file"
      )
      return(NULL)
    }
    return(getwd())
  }
  dirname(context$path)
}

#' Create a TypR project
#' @param path Chemin du projet à créer
#' @export
new <- function(path) {
  run_typr(c("new", shQuote(path)))
}

#' Run the current TypR file
#' @export
run_file <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  if (is.null(context$path) || context$path == "") {
    rstudioapi::showDialog(title = "Error", message = "Select an opened and saved file")
    return(invisible(NULL))
  }
  run_typr(shQuote(basename(context$path)), dir = dirname(context$path))
}

#' Run the TypR project
#' @export
run <- function() {
  dir <- active_document_dir(require_saved = FALSE)
  if (is.null(dir)) return(invisible(NULL))
  run_typr("run", dir = dir)
}

#' Build the TypR project
#' @export
build <- function() {
  dir <- active_document_dir(require_saved = FALSE)
  if (is.null(dir)) return(invisible(NULL))
  run_typr("build", dir = dir)
}

#' Check the TypR project
#' @export
check <- function() {
  dir <- active_document_dir(require_saved = FALSE)
  if (is.null(dir)) return(invisible(NULL))
  run_typr("check", dir = dir)
}

#' Test the TypR project
#' @export
test <- function() {
  dir <- active_document_dir(require_saved = FALSE)
  if (is.null(dir)) return(invisible(NULL))
  run_typr("test", dir = dir)
}
