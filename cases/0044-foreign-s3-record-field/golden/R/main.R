#' @include std.R
#' @include generic_functions.R
#' @include types.R

# Row 1 (S3 external, `lm`) x column d (record field) of interop_matrix.md.
LmModel <- Foreign

Wrapper <- function(label, model, .spread = NULL) {
  explicit <- list()
  if (!missing(label)) explicit[["label"]] <- label
  if (!missing(model)) explicit[["model"]] <- model
  x <- typr_spread_record(explicit, .spread)
  as.Wrapper(x)
}
as.Wrapper <- function(x) {
  if (!inherits(x, "Wrapper")) class(x) <- c("Wrapper", "list")
  x <- validate_Wrapper(x)
  x <- validate(x)
  x
}
validate_Wrapper <- function(x) {
  required_fields <- c("label", "model")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Wrapper: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["label"]], "character")) stop("Validation failed for type Wrapper: field 'label' must be of class character")
  x
}
`m` <- base::readRDS(to_native("fixtures/lm.rds" |> as.Character()))

`w` <- Wrapper(model = m, label = "test" |> as.Character())

print(w[['model']])