#!/usr/bin/env Rscript
# Inspects formals for all R function names listed in functions_R.txt.
# For each name, extracts: package, S3/S4 status, presence of `...`, and
# parameter names. Cross-references with r_name_db.json for metadata.
#
# Output is JSON (requires jsonlite), printed to stdout.
#
# Usage:
#   Rscript tools/inspect_formals.R \
#     crates/typr-cli/configs/src/functions_R.txt \
#     crates/typr-cli/configs/src/r_name_db.json \
#     > crates/typr-cli/configs/src/r_formals_db.json

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript tools/inspect_formals.R <functions_R.txt> <r_name_db.json>")
}

functions_txt <- args[1]
r_name_db_path <- args[2]

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required (install.packages('jsonlite'))")
}

suppressMessages({
  library(methods)
  library(stats)
  library(utils)
})

# --- Read inputs ---

func_names <- readLines(functions_txt, warn = FALSE)
func_names <- func_names[nchar(func_names) > 0]

r_name_db <- jsonlite::fromJSON(r_name_db_path, simplifyVector = FALSE)

# --- Helpers ---

# Decode TypR name encoding: __ -> . (R's dot)
decode_name <- function(typr_name) {
  gsub("__", ".", typr_name, fixed = TRUE)
}

# Check if a name is S3-generic (copied from gen_r_name_db.R for consistency)
is_s3_generic <- function(name, val) {
  if (!is.function(val)) return(FALSE)
  if (exists(".S3PrimitiveGenerics") && name %in% .S3PrimitiveGenerics) return(TRUE)
  body_txt <- tryCatch(paste(deparse(body(val)), collapse = "\n"), error = function(e) "")
  grepl("UseMethod", body_txt, fixed = TRUE)
}

# Extract formals info for a function value.
# Primitives report `formals()` as NULL, but `args(val)` reveals their
# formal names; fall back to `formals(args(val))` in that case.
extract_formals <- function(val) {
  if (!is.function(val)) return(NULL)
  f <- tryCatch(formals(val), error = function(e) NULL)
  if (is.null(f)) {
    f <- tryCatch(formals(args(val)), error = function(e) NULL)
  }
  if (is.null(f)) return(NULL)

  param_names <- names(f)
  has_dots <- "..." %in% param_names

  # Filter out "..." from param list for clean output
  params <- param_names[param_names != "..."]

  list(
    has_dots = has_dots,
    n_params = length(params),
    params = as.list(params)
  )
}

# Determine which namespace(s) a name might live in
base_pkgs <- c("package:base", "package:stats", "package:utils", "package:methods")

find_in_namespaces <- function(r_name) {
  for (pkg in base_pkgs) {
    val <- tryCatch(get(r_name, envir = as.environment(pkg)), error = function(e) NULL)
    if (!is.null(val) && is.function(val)) {
      return(list(pkg = sub("^package:", "", pkg), val = val))
    }
  }
  # Also check the global environment (some generics land there)
  val <- tryCatch(get(r_name, envir = globalenv()), error = function(e) NULL)
  if (!is.null(val) && is.function(val)) {
    return(list(pkg = "(global)", val = val))
  }
  return(NULL)
}

# --- Main loop ---

entries <- list()
n_total <- length(func_names)
n_found <- 0L
n_not_found <- 0L
n_no_formals <- 0L

cat("Inspecting formals for", n_total, "names...\n", file = stderr())

for (i in seq_along(func_names)) {
  typr_name <- func_names[i]
  r_name <- decode_name(typr_name)

  # Cross-reference with r_name_db
  db_entry <- r_name_db$names[[r_name]]

  # Find in R namespaces
  found <- find_in_namespaces(r_name)

  if (is.null(found)) {
    n_not_found <- n_not_found + 1L
    entries[[typr_name]] <- list(
      r_name = r_name,
      pkg = if (!is.null(db_entry)) db_entry$pkg else NA_character_,
      s3_generic = if (!is.null(db_entry)) db_entry$s3_generic else FALSE,
      s4_generic = if (!is.null(db_entry)) db_entry$s4_generic else FALSE,
      has_default = if (!is.null(db_entry)) db_entry$has_default else FALSE,
      found_in_r = FALSE,
      has_dots = NA,
      n_params = NA_integer_,
      params = list()
    )
    next
  }

  n_found <- n_found + 1L
  val <- found$val
  pkg <- found$pkg

  # Override pkg from db_entry if available (more authoritative)
  if (!is.null(db_entry) && !is.na(db_entry$pkg)) {
    pkg <- db_entry$pkg
  }

  s3 <- is_s3_generic(r_name, val)
  s4 <- if (!is.null(db_entry)) db_entry$s4_generic else FALSE
  has_def <- if (!is.null(db_entry)) db_entry$has_default else FALSE

  formals_info <- extract_formals(val)

  if (is.null(formals_info)) {
    n_no_formals <- n_no_formals + 1L
    entries[[typr_name]] <- list(
      r_name = r_name,
      pkg = pkg,
      s3_generic = s3,
      s4_generic = s4,
      has_default = has_def,
      found_in_r = TRUE,
      has_dots = NA,
      n_params = NA_integer_,
      params = list()
    )
  } else {
    entries[[typr_name]] <- list(
      r_name = r_name,
      pkg = pkg,
      s3_generic = s3,
      s4_generic = s4,
      has_default = has_def,
      found_in_r = TRUE,
      has_dots = formals_info$has_dots,
      n_params = formals_info$n_params,
      params = formals_info$params
    )
  }
}

cat(sprintf("Done: %d found, %d not found in R, %d no formals\n",
            n_found, n_not_found, n_no_formals), file = stderr())

# --- Output ---

result <- list(
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  n_total = n_total,
  n_found = n_found,
  n_not_found = n_not_found,
  n_no_formals = n_no_formals,
  names = entries
)

cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null"))
cat("\n")
