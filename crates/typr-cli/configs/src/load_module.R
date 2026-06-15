# TypR Project Loader — RFC-TR-030
#
# Loads transpiled R modules in topological @include order, without source(),
# with proper module encapsulation.  Each generated .R file is evaluated in its
# own isolated environment; TypR module environments (Math, Main, …) are
# collected and returned in a single flat `modules` environment.
#
# Usage:
#   source("load_module.R")
#   modules <- load_module(".")
#   modules$Main$main()
#
#   # Test mode — exposes @testable members (requires typr build --test)
#   modules <- load_module(".", test = TRUE)

load_module <- function(project_root = ".", test = FALSE) {
  generated_dir <- .typr_find_generated_dir(project_root)
  r_files       <- list.files(generated_dir, pattern = "\\.R$", full.names = FALSE)
  if (length(r_files) == 0L) stop("No .R files found in: ", generated_dir)

  includes   <- .typr_build_include_graph(r_files, generated_dir)
  load_order <- .typr_topo_sort(includes)

  modules <- new.env(parent = emptyenv())
  attr(modules, "__modules__")   <- character(0L)
  attr(modules, "__order__")     <- load_order
  attr(modules, "__test_mode__") <- test

  loaded <- list()

  for (file in load_order) {
    file_env <- new.env(parent = baseenv())

    # Inject bindings from already-loaded direct dependencies
    for (dep in includes[[file]]) {
      dep_env <- loaded[[dep]]
      if (!is.null(dep_env)) {
        for (nm in ls(dep_env, all.names = TRUE)) {
          assign(nm, get(nm, envir = dep_env, inherits = FALSE), envir = file_env)
        }
      }
    }

    sys.source(file.path(generated_dir, file), envir = file_env, chdir = FALSE)
    loaded[[file]] <- file_env

    # Collect TypR module environments (new.env() bindings in the loaded file)
    for (nm in ls(file_env)) {
      val <- get(nm, envir = file_env, inherits = FALSE)
      if (is.environment(val)) {
        assign(nm, val, envir = modules)
        attr(modules, "__modules__") <- unique(c(attr(modules, "__modules__"), nm))
      }
    }
  }

  lockEnvironment(modules)
  modules
}

# Resolve the generated R directory from typr.toml or fall back to R/
.typr_find_generated_dir <- function(project_root) {
  toml <- file.path(project_root, "typr.toml")
  if (file.exists(toml)) {
    lines <- readLines(toml, warn = FALSE)
    for (line in lines) {
      m <- regexpr('output_dir\\s*=\\s*"([^"]+)"', line, perl = TRUE)
      if (m > 0L) {
        cs <- attr(m, "capture.start")
        cl <- attr(m, "capture.length")
        return(file.path(project_root, substr(line, cs, cs + cl - 1L)))
      }
    }
  }
  file.path(project_root, "R")
}

# Build adjacency list from #' @include directives at the top of each .R file
.typr_build_include_graph <- function(r_files, generated_dir) {
  graph <- setNames(vector("list", length(r_files)), r_files)
  for (file in r_files) {
    deps  <- character(0L)
    lines <- readLines(file.path(generated_dir, file), warn = FALSE)
    for (line in lines) {
      if (!grepl("^#'", line)) break
      m <- regexpr("@include\\s+(\\S+\\.R)", line, perl = TRUE)
      if (m > 0L) {
        cs  <- attr(m, "capture.start")
        cl  <- attr(m, "capture.length")
        dep <- substr(line, cs, cs + cl - 1L)
        if (dep %in% r_files) deps <- c(deps, dep)
      }
    }
    graph[[file]] <- deps
  }
  graph
}

# Kahn's algorithm — returns files in load order (dependencies first)
.typr_topo_sort <- function(graph) {
  dependents <- setNames(vector("list", length(graph)), names(graph))
  for (nm in names(dependents)) dependents[[nm]] <- character(0L)
  in_deg <- setNames(integer(length(graph)), names(graph))
  for (file in names(graph)) {
    in_deg[[file]] <- length(graph[[file]])
    for (dep in graph[[file]]) {
      dependents[[dep]] <- c(dependents[[dep]], file)
    }
  }

  queue  <- names(in_deg)[in_deg == 0L]
  result <- character(0L)
  while (length(queue) > 0L) {
    node   <- queue[[1L]]
    queue  <- queue[-1L]
    result <- c(result, node)
    for (dep_of in dependents[[node]]) {
      in_deg[[dep_of]] <- in_deg[[dep_of]] - 1L
      if (in_deg[[dep_of]] == 0L) queue <- c(queue, dep_of)
    }
  }

  if (length(result) != length(graph)) stop("Cycle detected in @include dependencies")
  result
}
