#!/usr/bin/env Rscript
# Introspects installed R packages for the `.typr_cache/r_names.json` cache
# (see crates/typr-cli/src/r_name_cache.rs). For every exported name of every
# package named on the command line, reports whether that name is S3-generic,
# S4-generic, and whether a `<name>.default` method already exists — the three
# facts `typr build` needs to decide if shadowing the name with its own
# `UseMethod` stub would strand the original implementation.
#
# This is the runtime counterpart of tools/gen_r_name_db.R, which produces the
# committed base-R seed. The dispatch predicates below are deliberately kept
# identical to that script's.
#
# Usage:
#   Rscript introspect_pkg.R dplyr stringr
#
# Output is line-based TSV rather than JSON on purpose: jsonlite is a hard
# dependency of the seed generator, but this script runs on the *user's*
# machine during a build and must not require any package to be installed.
#
#   V<TAB>r_version
#   N<TAB>name<TAB>pkg<TAB>s3_generic<TAB>s4_generic<TAB>has_default
#   C<TAB>s4_class_name
#   E<TAB>pkg<TAB>reason          (package could not be loaded)

suppressMessages(library(methods))

args <- commandArgs(trailingOnly = TRUE)

emit <- function(...) cat(paste(c(...), collapse = "\t"), "\n", sep = "")

# A name is S3-generic if its body literally dispatches via UseMethod, or it is
# one of the internally-dispatched primitives listed in `.S3PrimitiveGenerics`
# (dispatch happens in C, so no visible UseMethod call, but a `name.<class>`
# method IS found). Kept in sync with tools/gen_r_name_db.R.
is_s3_generic <- function(name, val) {
  if (!is.function(val)) {
    return(FALSE)
  }
  if (exists(".S3PrimitiveGenerics") && name %in% .S3PrimitiveGenerics) {
    return(TRUE)
  }
  body_txt <- tryCatch(paste(deparse(body(val)), collapse = "\n"), error = function(e) "")
  grepl("UseMethod", body_txt, fixed = TRUE)
}

tsv_safe <- function(x) !grepl("[\t\n\r]", x)

emit("V", paste(R.version$major, R.version$minor, sep = "."))

for (pkg in args) {
  ok <- tryCatch(requireNamespace(pkg, quietly = TRUE), error = function(e) FALSE)
  if (!ok) {
    emit("E", pkg, "not installed or failed to load")
    next
  }

  ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
  if (is.null(ns)) {
    emit("E", pkg, "namespace unavailable")
    next
  }

  exports <- tryCatch(getNamespaceExports(ns), error = function(e) character(0))

  # S4 generics visible now that the namespace is loaded. `getGenerics()` is
  # global rather than per-package, so it is recomputed per package: loading a
  # package can register new S4 generics.
  s4_names <- tryCatch(
    {
      g <- getGenerics()
      unique(as.character(g@.Data))
    },
    error = function(e) character(0)
  )

  for (n in exports) {
    if (!tsv_safe(n)) next
    val <- tryCatch(get(n, envir = ns), error = function(e) NULL)
    if (is.null(val) || !is.function(val)) next
    # `<name>.default` may live in the package's own namespace or anywhere on
    # the search path (a base generic's default, say) — either one gives the
    # UseMethod stub something to dispatch to, so both count.
    has_default <-
      exists(paste0(n, ".default"), envir = ns, mode = "function", inherits = FALSE) ||
        exists(paste0(n, ".default"), mode = "function")
    emit(
      "N", n, pkg,
      if (is_s3_generic(n, val)) "1" else "0",
      if (n %in% s4_names) "1" else "0",
      if (has_default) "1" else "0"
    )
  }

  # NOTE: `getClasses()` reads its caller's environment when `where` is left to
  # default, which silently returns nothing when nested inside another call
  # (see tools/gen_r_name_db.R). Passing `where` explicitly avoids that trap.
  classes <- tryCatch(as.character(getClasses(where = ns)), error = function(e) character(0))
  for (cl in classes) {
    if (tsv_safe(cl)) emit("C", cl)
  }
}
