#!/usr/bin/env Rscript
# Classifies R function names into tiers T1/T2/T3 based on heuristics.
# Reads r_formals_db.json (from inspect_formals.R) and applies rules.
#
# Tier definitions (from RFC-STDLIB-0001, section 6 phase 0):
#   T1 — probable pure & typable (no `...`, no S3 dispatch, no surprising
#        coercion). Feeds the compiler's .std_r_typed.bin in Phase 2.
#   T2 — typable but loose (`...`, variadic contraint, unions, S3 dispatch).
#        Doc/MCP only, never in the compiler.
#   T3 — non typable d'office (blacklist + do.call/with/eval/subsetting +
#        metaprogramming/environment/R-internals). UnknownFunction.
#   manual_review — heuristic is unsure; human review required.
#
# This is a FIRST PASS. The RFC explicitly requires relecture humaine after
# the heuristic run. The output JSON is the review workbench, not the law.
#
# Usage:
#   Rscript tools/classify_tiers.R <r_formals_db.json> [<stdlib .ty dir>] \
#     > crates/typr-cli/configs/src/r_tier_classification.json
#
# If the stdlib .ty directory is given, the classifier cross-checks its
# verdict against the signatures already typed & compiled in configs/std/*.ty —
# names already typed there are compiler-ground-truth, and a heuristic verdict
# that contradicts them is flagged as a disagreement.

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript tools/classify_tiers.R <r_formals_db.json> [<stdlib .ty dir>]")
}

formals_path <- args[1]
ty_dir <- if (length(args) >= 2) args[2] else NULL

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required (install.packages('jsonlite'))")
}

formals_db <- jsonlite::fromJSON(formals_path, simplifyVector = FALSE)

# --- Existing typed signatures (compiler ground truth from configs/std/*.ty) ---
# Parse every `@name: ...` declaration, decode `__` -> `.`, strip backticks.

typed_set <- character(0)
if (!is.null(ty_dir) && dir.exists(ty_dir)) {
  for (f in list.files(ty_dir, pattern = "\\.ty$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    for (ln in lines) {
      m <- regmatches(ln, regexpr("^@([^:]+):", ln))

      env <- sub("^@([^:]+):", "\\1", m)
      if (length(env) == 0 || nchar(env) == 0) next
      name <- sub("^`|`$", "", env)      # strip backticks around operators
      name <- gsub("__", ".", name, fixed = TRUE)   # __ -> . encoding
      typed_set <- c(typed_set, name)
    }
  }
  typed_set <- unique(typed_set)
}

# --- Blacklist (R-native names; source: crates/typr-core/src/utils/standard_library.rs) ---

BLACKLIST <- c(
  "test_that", "expect_true", "+", "*", "-", "/", "%%",
  "while", "repeat", "for", "if", "function",
  "||", "|", ">=", "<=", "<", ">", "==", "=", "^", "&&", "&",
  "next", "break", ".POSIXt", "source", "class", "union", "c", "library",
  "return", "list", "try", "integer", "character", "logical", "UseMethod",
  "length", "sapply", "inherits", "all", "lapply", "unlist", "array",
  "cat", "rep", "str", "oldClass", "stop", "invisible", "capture.output",
  "paste0", "unclass", "exists", "vector", "tags", "paste"
)

# --- T3 d'office — RFC section 6 phase 0 item 3 + obvious non-typeables ---

T3_RFC_EXTRAS <- c(
  "do.call", "with", "eval", "evalq", "local"
)

# Operators and special forms that cannot be typed as functions
T3_OPERATOR <- c(
  "%*%", "%/%", "%||%", "%in%", "%o%", "%x%", "%==%",
  "[", "[[", "$", "[<-", "[[<-", "$<-", "~", "(", "{", "@", "::", ":::"
)

# Metaprogramming / language-object constructors
T3_METAPROGRAMMING <- c(
  "substitute", "quote", "expression", "call", "alist", "formals", "body",
  "args", "deparse", "parse", "enquote", "bquote", "str2lang", "str2expression",
  "as.expression", "as.call", "as.name", "as.symbol"
)

# Environments / namespaces / search path
T3_ENVIRONMENT <- c(
  "environment", "new.env", "globalenv", "baseenv", "emptyenv", "parent.env",
  "as.environment", "asNamespace", "loadNamespace", "unloadNamespace",
  "attachNamespace", "requireNamespace", "getNamespace", "getNamespaceExports",
  "getNamespaceImports", "getNamespaceInfo", "getNamespaceName",
  "getNamespaceUsers", "getNamespaceVersion", "namespaceExport",
  "namespaceImportClasses", "namespaceImportFrom", "namespaceImportMethods",
  "packageEvent", "packageHasNamespace", "packageNotFoundError",
  "parseNamespaceFile", "importIntoEnv", "attach", "detach", "search",
  "searchpaths", "ls", "objects", "topenvironment", "topenv"
)

# R evaluation / dynamic dispatch internals
T3_R_INTERNALS <- c(
  "UseMethod", "NextMethod", "standardGeneric", "Recall",
  "setMethod", "setGeneric", "setClass",
  "get", "mget", "get0", "assign", "rm", "remove", "exists",
  "on.exit", "match.call", "match.arg", "nargs", "missing",
  "sys.call", "sys.calls", "sys.frame", "sys.frames", "sys.function",
  "sys.nframe", "sys.parent", "sys.parents", "sys.on.exit",
  "withVisible", "invisible", "force", "delayedAssign", "makeActiveBinding",
  "lockBinding", "unlockBinding", "bindingIsLocked", "bindingIsActive",
  "lockEnvironment", "environmentIsLocked",
  "trace", "untrace", "traceback", "debug", "undebug", "debugonce",
  "browser", "browserCondition", "browserSetDebug", "browserText",
  "debuggingState", "tracingState", "isdebugged", "retracemem", "untracemem",
  "tracemem", "callCC", "Recall", "Warning in", "dontCheck"
)

# Values that are not functions
T3_NON_FUNCTIONS <- c(
  "break", "next", "return", "if", "while", "for", "function",
  "TRUE", "FALSE", "NA", "NULL", "Inf", "NaN", "pi"
)

# --- RFC probable-T1 candidates (section 6 phase 0 item 2, verbatim) ---
# Listed by the RFC as probable T1 even though some carry `...` or S4 flags;
# each must clear the section-7 promotion gate during human review.

RFC_T1_CANDIDATES <- c(
  "abs", "sqrt", "log", "exp", "sum", "mean", "nrow", "ncol",
  "prod", "min", "max", "range", "cumsum", "cumprod", "length"
)

# --- Curated pure-T1 groups (checked before any dispatch/`...` rule) ---

T1_MATH <- c(
  # exponentials & logs
  "abs", "sqrt", "exp", "expm1", "log", "log1p", "log2", "log10", "logb",
  # rounding
  "ceiling", "floor", "trunc", "round", "signif", "zapsmall",
  # trigonometry
  "cos", "sin", "tan", "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
  # special functions
  "gamma", "lgamma", "digamma", "trigamma", "psigamma",
  "factorial", "lfactorial", "choose", "lchoose",
  # complex / misc
  "Re", "Im", "Mod", "Arg", "Conj", "xor",
  "bitwAnd", "bitwOr", "bitwXor", "bitwNot", "bitwShiftL", "bitwShiftR"
)

T1_TYPE_CHECKS <- c(
  "is.null", "is.numeric", "is.integer", "is.double", "is.character",
  "is.logical", "is.complex", "is.raw", "is.factor", "is.ordered",
  "is.data.frame", "is.array", "is.atomic", "is.recursive", "is.function",
  "is.primitive", "is.vector", "is.element", "is.finite", "is.infinite",
  "is.nan", "is.name", "is.symbol", "is.language", "is.environment",
  "is.object", "is.pairlist", "is.single", "is.unsorted", "is.qr", "is.table",
  "isTRUE", "isFALSE"
)

T1_STATS_REDUCTIONS <- c(
  "sum", "prod", "min", "max", "range", "mean",
  "cumsum", "cumprod", "cummax", "cummin",
  "pmax", "pmin", "rowSums", "colSums", "rowMeans", "colMeans",
  "var", "sd", "mad", "median", "quantile", "weighted.mean",
  "nrow", "ncol", "NROW", "NCOL", "dim", "lengths"
)

T1_CONVERSIONS <- c(
  "as.numeric", "as.integer", "as.double", "as.character", "as.logical",
  "as.complex", "as.raw", "as.factor", "as.data.frame", "as.vector",
  "as.array", "as.matrix", "as.list", "as.environment"
)

T1_STRINGS <- c(
  "nchar", "nzchar", "substr", "substring", "tolower", "toupper",
  "startsWith", "endsWith", "trimws", "chartr", "nlevels"
)

T1_VECTOR_BASIC <- c(
  "length", "which", "intersect", "setdiff", "setequal", "union",
  "rev", "sort", "order", "rank", "unique", "duplicated", "match",
  "table", "tabulate", "unlist", "as.vector", "rep.int", "seq_len",
  "seq_along", "seq.int", "colnames", "rownames", "names", "dimnames",
  "diag", "drop"
)

T1_ALL <- unique(c(T1_MATH, T1_TYPE_CHECKS, T1_STATS_REDUCTIONS,
                   T1_CONVERSIONS, T1_STRINGS, T1_VECTOR_BASIC))

# --- Other T2 candidates (I/O, side effects) ---

T2_SIDE_EFFECTS <- c(
  "readLines", "writeLines", "readRDS", "saveRDS", "url", "file", "pipe",
  "gzfile", "bzfile", "xzfile", "unz", "rawConnection", "textConnection",
  "socketConnection", "serverSocket", "socketAccept", "socketSelect",
  "socketTimeout", "sink", "showConnections", "closeAllConnections",
  "getAllConnections", "getConnection", "isOpen", "isIncomplete", "isSeekable",
  "pushBack", "pushBackLength", "clearPushBack", "stdin", "stdout", "stderr",
  "readBin", "writeBin", "readChar", "writeChar", "download.file",
  "read.dcf", "load", "save", "serialize", "unserialize", "dput", "dget",
  "dump", "write", "writeLines", "summary.socketAccept"
)

# --- Heuristic classification ---

classify_name <- function(typr_name, info) {
  r_name <- info$r_name

  # Rule 0: operator / special-name encoding. Names in functions_R.txt with
  # these characters are TypR operator forms, not typeable functions.
  if (grepl("^[^A-Za-z]", r_name)) {
    return(list(tier = "T3", reason = "T3_operator_or_special"))
  }

  # Rule 1: blacklist
  if (r_name %in% BLACKLIST) {
    return(list(tier = "T3", reason = "T3_blacklist"))
  }

  # Rule 2: RFC extras
  if (r_name %in% T3_RFC_EXTRAS) {
    return(list(tier = "T3", reason = "T3_rfc_extra"))
  }

  # Rule 3: operator forms (from T3_OPERATOR, dot or bracket names)
  if (r_name %in% T3_OPERATOR) {
    return(list(tier = "T3", reason = "T3_operator_form"))
  }

  # Rule 4: metaprogramming / language objects
  if (r_name %in% T3_METAPROGRAMMING) {
    return(list(tier = "T3", reason = "T3_metaprogramming"))
  }

  # Rule 5: environments / namespaces
  if (r_name %in% T3_ENVIRONMENT) {
    return(list(tier = "T3", reason = "T3_environment"))
  }

  # Rule 6: R internals
  if (r_name %in% T3_R_INTERNALS) {
    return(list(tier = "T3", reason = "T3_r_internals"))
  }

  # Rule 7: non-functions
  if (r_name %in% T3_NON_FUNCTIONS) {
    return(list(tier = "T3", reason = "T3_non_function"))
  }

  # Rule 8: not found in R
  if (!isTRUE(info$found_in_r)) {
    return(list(tier = "manual_review", reason = "unknown_not_in_r"))
  }

  # Rule 9: curated T1 groups — checked before dispatch / dots rules so
  # primitives registered as S4 generics (abs, sqrt, ...) stay T1.
  if (r_name %in% T1_ALL) {
    flags <- c()
    if (isTRUE(info$s3_generic)) flags <- c(flags, "s3")
    if (isTRUE(info$s4_generic)) flags <- c(flags, "s4")
    if (isTRUE(info$has_dots)) flags <- c(flags, "dots")
    if (length(flags) > 0) {
      return(list(tier = "T1", reason = paste0("T1_curated_", paste(flags, collapse = "+"))))
    }
    return(list(tier = "T1", reason = "T1_curated_clean"))
  }

  # Rule 10: RFC probable-T1 candidates not already covered
  if (r_name %in% RFC_T1_CANDIDATES) {
    return(list(tier = "T1", reason = "T1_rfc_candidate_review"))
  }

  # Rule 11: has dots → T2
  if (isTRUE(info$has_dots)) {
    return(list(tier = "T2", reason = "T2_has_dots"))
  }

  # Rule 12: S3/S4 generic → T2
  if (isTRUE(info$s3_generic) || isTRUE(info$s4_generic)) {
    return(list(tier = "T2", reason = if (isTRUE(info$s3_generic)) "T2_s3_dispatch" else "T2_s4_dispatch"))
  }

  # Rule 13: has_default -> T2 (a dispatch target exists)
  if (isTRUE(info$has_default)) {
    return(list(tier = "T2", reason = "T2_has_default"))
  }

  # Rule 14: I/O / side-effect functions → T2
  if (r_name %in% T2_SIDE_EFFECTS) {
    return(list(tier = "T2", reason = "T2_side_effects"))
  }

  # Rule 15: no formals recovered → manual review
  if (is.na(info$n_params) || is.null(info$n_params)) {
    return(list(tier = "manual_review", reason = "manual_review_no_formals"))
  }

  # Rule 16: clean, small, non-dispatch function in base/stats → probable T1
  if (info$n_params <= 5) {
    return(list(tier = "T1", reason = "T1_candidate"))
  }

  # Default: many params, would need a closer look
  return(list(tier = "manual_review", reason = "manual_review_many_params"))
}

# --- Classify all names ---

names_list <- formals_db$names

result <- list(
  r_version = formals_db$r_version,
  source = list(
    n_total = formals_db$n_total,
    n_found = formals_db$n_found,
    n_not_found = formals_db$n_not_found
  ),
  note = "First-pass heuristic. Human review required (RFC section 6, phase 0, item 2).",
  classifications = list(),
  summary = list()
)

tier_counts <- list(T1 = 0L, T2 = 0L, T3 = 0L, manual_review = 0L)
reason_counts <- list()

t1_subcats <- list()

# Consistency counters
consistency <- list(
  already_typed = 0L,
  agrees_t1 = 0L,
  disagrees = 0L,
  typed_but_blacklisted = 0L,
  disagreements = list()
)

for (typr_name in names(names_list)) {
  info <- names_list[[typr_name]]
  cls <- classify_name(typr_name, info)

  tier <- cls$tier
  reason <- cls$reason

  already_typed <- info$r_name %in% typed_set
  in_blacklist <- info$r_name %in% BLACKLIST

  # Consistency layer: a name in the R-function list AND already typed in a
  # compiled .ty signature is compiler ground truth. If the heuristic says
  # anything other than T1, that's a disagreement worth surfacing (Phase 2
  # must not lose an existing valid signature — RFC "zéro régression").
  if (already_typed) {
    consistency$already_typed <- consistency$already_typed + 1L
    if (tier == "T1") {
      consistency$agrees_t1 <- consistency$agrees_t1 + 1L
    } else {
      consistency$disagrees <- consistency$disagrees + 1L
      consistency$disagreements[[info$r_name]] <- list(
        heuristic_tier = tier,
        heuristic_reason = reason
      )
      tier <- "T1"
      reason <- paste0("T1_already_typed_disagree", "_", sub("^T1_", "", reason))
    }
    if (in_blacklist && tier == "T1") {
      consistency$typed_but_blacklisted <- consistency$typed_but_blacklisted + 1L
    }
  }

  tier_counts[[tier]] <- tier_counts[[tier]] + 1L
  if (is.null(reason_counts[[reason]])) reason_counts[[reason]] <- 0L
  reason_counts[[reason]] <- reason_counts[[reason]] + 1L

  if (tier == "T1" && grepl("^T1_", reason)) {
    subcat <- sub("^T1_", "", reason)
    t1_subcats[[subcat]] <- c(t1_subcats[[subcat]], typr_name)
  }

  result$classifications[[typr_name]] <- list(
    r_name = info$r_name,
    pkg = info$pkg,
    tier = tier,
    reason = reason,
    s3 = isTRUE(info$s3_generic),
    s4 = isTRUE(info$s4_generic),
    has_dots = isTRUE(info$has_dots),
    n_params = info$n_params,
    params = info$params,
    already_typed = already_typed
  )
}

result$summary <- list(
  T1 = tier_counts$T1,
  T2 = tier_counts$T2,
  T3 = tier_counts$T3,
  manual_review = tier_counts$manual_review,
  reasons = reason_counts,
  T1_subcategories = t1_subcats,
  consistency = consistency
)

# --- Output ---

cat("Classification summary (first pass):\n", file = stderr())
cat(sprintf("  T1 (probable typable):   %d\n", tier_counts$T1), file = stderr())
cat(sprintf("  T2 (probable doc-only):  %d\n", tier_counts$T2), file = stderr())
cat(sprintf("  T3 (non typable):        %d\n", tier_counts$T3), file = stderr())
cat(sprintf("  Manual review:           %d\n", tier_counts$manual_review), file = stderr())
if (length(typed_set) > 0) {
  cat("Consistency vs .ty (compiler ground truth):\n", file = stderr())
  cat(sprintf("  already typed:      %d\n", consistency$already_typed), file = stderr())
  cat(sprintf("  agrees with T1:     %d\n", consistency$agrees_t1), file = stderr())
  cat(sprintf("  disagreements:      %d\n", consistency$disagrees), file = stderr())
  cat(sprintf("  typed but blacklist: %d\n", consistency$typed_but_blacklisted), file = stderr())
}
cat("Reasons:\n", file = stderr())
for (r in names(sort(unlist(reason_counts), decreasing = TRUE))) {
  cat(sprintf("  %-28s %d\n", r, reason_counts[[r]]), file = stderr())
}

cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null"))
cat("\n")