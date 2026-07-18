#!/usr/bin/env Rscript
# Regenerates the tiny .rds fixtures used by the Phase D interop-matrix cases
# (soundness_transpilation.md § Phase D, interop_matrix.md). Each fixture is a
# real foreign R value (S3/S4/base-with-attributes) that a case's `main.ty`
# reads back via `@extern base::readRDS` — this is the "repro/ avec un vrai
# projet dont le main.ty reçoit la valeur étrangère" pattern from the plan
# doc, with the "fichier R compagnon" being this generator run once ahead of
# time instead of sourced at run (TypR's project loader sources every *.R
# file in R/ into its own isolated environment — see load_module.R — so a
# hand-written companion .R file placed there wouldn't be visible from
# main.ty's environment without extra plumbing; pre-baking + `readRDS` sides
# around that entirely and is exactly as real a foreign value).
#
# Run by hand, commit the result (same convention as tools/gen_r_name_db.R).
# Regenerate only if a case needs a different/additional foreign value.
#
# Usage:
#   Rscript tools/gen_interop_fixtures.R

suppressMessages(library(Matrix))

write_fixture <- function(value, dest) {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  saveRDS(value, dest)
  cat("wrote", dest, "\n")
}

# Row 1 (interop_matrix.md): S3 external — a real `lm` fit, the plan doc's
# own example ("objet lm, data.frame exotique").
lm_fixture <- lm(y ~ x, data = data.frame(x = 1:5, y = c(2, 4, 5, 4, 5)))

# Row 2: S4 — Matrix::Matrix, a real third-party S4 object (also the plan
# doc's own suggested example: "objet Matrix::Matrix").
matrix_fixture <- Matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Row 7: base vector with attributes — an ordered factor built directly with
# base::factor(), deliberately NOT going through TypR's own opaque
# `Factor<L>` stdlib type (factor.ty) so this exercises a genuinely foreign
# value, not a TypR-constructed one.
factor_fixture <- factor(c("lo", "hi", "med"), levels = c("lo", "med", "hi"), ordered = TRUE)

for (case in c(
  "0018-foreign-lm-let-annotation", "0019-foreign-lm-function-arg",
  "0020-foreign-lm-function-return"
)) {
  write_fixture(lm_fixture, file.path("cases", case, "repro", "fixtures", "lm.rds"))
}

for (case in c(
  "0022-foreign-s4-matrix-let-annotation", "0023-foreign-s4-matrix-function-arg",
  "0024-foreign-s4-matrix-function-return"
)) {
  write_fixture(matrix_fixture, file.path("cases", case, "repro", "fixtures", "matrix_s4.rds"))
}

for (case in c(
  "0026-foreign-factor-let-annotation", "0027-foreign-factor-function-arg",
  "0028-foreign-factor-function-return"
)) {
  write_fixture(factor_fixture, file.path("cases", case, "repro", "fixtures", "factor_ext.rds"))
}
