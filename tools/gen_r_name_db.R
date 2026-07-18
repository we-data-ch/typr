#!/usr/bin/env Rscript
# Regenerates crates/typr-cli/configs/src/r_name_db.json — the static oracle
# for the Phase C lint (soundness_transpilation.md § Phase C). This is the
# database the typr-cli build pipeline intersects, at compile time, against
# every top-level name it is about to emit (UseMethod stubs, record
# constructors) to catch the "collision with the rest of R's object systems"
# bug family (historical example: the `nlevels` bug, see CLAUDE.md).
#
# Run by hand, commit the result. Regenerate when bumping the R version this
# project targets (see the `r_version` field in the output).
#
# Usage:
#   Rscript tools/gen_r_name_db.R > crates/typr-cli/configs/src/r_name_db.json

suppressMessages({
  library(methods)
  library(stats)
  library(utils)
})

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to run this script (install.packages('jsonlite'))")
}

base_pkgs <- c("package:base", "package:stats", "package:utils", "package:methods")

# A name is treated as S3-generic if its body literally dispatches via
# UseMethod, or it is one of the internally-dispatched primitives R documents
# in `.S3PrimitiveGenerics` (these have no visible `UseMethod()` call in their
# body since the dispatch happens in C, but they are genuinely generic: a
# `name.<class>` method registered for them IS found).
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

has_default_method <- function(name) {
  exists(paste0(name, ".default"), mode = "function")
}

s4_generic_names <- tryCatch(
  {
    g <- getGenerics()
    unique(g@.Data)
  },
  error = function(e) character(0)
)

collect_pkg_names <- function(pkg) {
  ls(envir = as.environment(pkg), all.names = FALSE)
}

entries <- list()
seen <- character(0)

for (pkg in base_pkgs) {
  for (n in collect_pkg_names(pkg)) {
    if (n %in% seen) next
    seen <- c(seen, n)
    val <- tryCatch(get(n, envir = as.environment(pkg)), error = function(e) NULL)
    if (is.null(val)) next
    entries[[n]] <- list(
      pkg = sub("^package:", "", pkg),
      s3_generic = is_s3_generic(n, val),
      s4_generic = n %in% s4_generic_names,
      has_default = has_default_method(n)
    )
  }
}

# NOTE: `getClasses()` defaults `where = .externalCallerEnv()`, which reads
# the call stack of its *caller* — calling it nested inside another function
# call (`sort(...)`, or even `tryCatch(...)` itself) changes what it considers
# the external caller and silently returns zero classes. Call it bare, at
# top level, then post-process the result separately.
s4_classes_raw <- getClasses()
s4_classes <- sort(unique(as.character(s4_classes_raw)))

result <- list(
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  names = entries,
  s4_classes = s4_classes
)

cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null"))
cat("\n")
