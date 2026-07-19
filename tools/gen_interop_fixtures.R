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
suppressMessages(library(R6))
suppressMessages(library(S7))

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

# Row 3: RC (setRefClass) — reference-semantics object, genuinely S4 under
# the hood (isS4() is TRUE), distinct from the Matrix::Matrix S4 row because
# its representation is an environment wrapped in an S4 shell rather than a
# slotted numeric structure.
rc_generator <- setRefClass("Counter",
  fields = list(value = "numeric"),
  methods = list(increment = function() {
    value <<- value + 1
  })
)
rc_fixture <- rc_generator$new(value = 5)

# Row 4: R6 — reference-semantics object, NOT S4 (isS4() is FALSE): a plain
# environment with class c("Counter6", "R6"), closer kin to the base-with-
# attributes row (factor) than to the S4 row, despite also being a
# reference/mutable object like RC.
r6_generator <- R6Class("Counter6",
  public = list(
    value = 5,
    increment = function() {
      self$value <- self$value + 1
      invisible(self)
    }
  )
)
r6_fixture <- r6_generator$new()

# Row 5: S7 (new_class) — R-core's new official OOP system, NOT S4
# (isS4() is FALSE) and NOT a reference type (typeof() is the new "object"
# type, but properties are set by value like a record, no mutation-through-
# alias semantics like RC/R6). Unlike RC/R6, print() works standalone on a
# bare readRDS()'d S7 fixture with no `library(S7)` call in the session --
# confirmed directly -- so this row uses print() as the oracle just like
# rows 1/2/7, no isS4()/inherits() workaround needed.
s7_generator <- new_class("Counter7", properties = list(value = class_numeric))
s7_fixture <- s7_generator(value = 5)

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

for (case in c(
  "0031-foreign-rc-function-arg", "0032-foreign-rc-function-return",
  "0033-foreign-rc-let-annotation"
)) {
  write_fixture(rc_fixture, file.path("cases", case, "repro", "fixtures", "rc.rds"))
}

for (case in c(
  "0035-foreign-r6-function-arg", "0036-foreign-r6-function-return",
  "0037-foreign-r6-let-annotation"
)) {
  write_fixture(r6_fixture, file.path("cases", case, "repro", "fixtures", "r6.rds"))
}

# Column i (module boundary), D.2: reuse the row 1/2/7 fixtures against a
# @pub fn declared inside a module instead of at top level.
write_fixture(lm_fixture, file.path("cases", "0039-foreign-s3-module-boundary", "repro", "fixtures", "lm.rds"))
write_fixture(matrix_fixture, file.path("cases", "0040-foreign-s4-module-boundary", "repro", "fixtures", "matrix_s4.rds"))
write_fixture(factor_fixture, file.path("cases", "0041-foreign-factor-module-boundary", "repro", "fixtures", "factor_ext.rds"))

# Columns d (record field) and g (pipe), D.4: reuse the row 1/2/7 fixtures.
write_fixture(lm_fixture, file.path("cases", "0044-foreign-s3-record-field", "repro", "fixtures", "lm.rds"))
write_fixture(lm_fixture, file.path("cases", "0045-foreign-s3-pipe", "repro", "fixtures", "lm.rds"))
write_fixture(matrix_fixture, file.path("cases", "0046-foreign-s4-record-field", "repro", "fixtures", "matrix_s4.rds"))
write_fixture(matrix_fixture, file.path("cases", "0047-foreign-s4-pipe", "repro", "fixtures", "matrix_s4.rds"))
write_fixture(factor_fixture, file.path("cases", "0048-foreign-factor-record-field", "repro", "fixtures", "factor_ext.rds"))
write_fixture(factor_fixture, file.path("cases", "0049-foreign-factor-pipe", "repro", "fixtures", "factor_ext.rds"))

# Column e (match subject), D.4 Phase 2: reuse the row 1/2/7 fixtures.
write_fixture(lm_fixture, file.path("cases", "0050-foreign-s3-match", "repro", "fixtures", "lm.rds"))
write_fixture(matrix_fixture, file.path("cases", "0051-foreign-s4-match", "repro", "fixtures", "matrix_s4.rds"))
write_fixture(factor_fixture, file.path("cases", "0052-foreign-factor-match", "repro", "fixtures", "factor_ext.rds"))

for (case in c(
  "0056-foreign-s7-function-arg", "0057-foreign-s7-function-return",
  "0058-foreign-s7-let-annotation"
)) {
  write_fixture(s7_fixture, file.path("cases", case, "repro", "fixtures", "s7.rds"))
}
