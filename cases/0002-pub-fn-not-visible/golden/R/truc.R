#' @include std.R
#' @include generic_functions.R
#' @include types.R
do <- function(x, ...) UseMethod("do")
truc <- new.env(parent = emptyenv())
local({
Object <- function(a, b, .spread = NULL) {
  explicit <- list()
  if (!missing(a)) explicit[["a"]] <- a
  if (!missing(b)) explicit[["b"]] <- b
  x <- typr_spread_record(explicit, .spread)
  as.Object(x)
}
as.Object <- function(x) {
  if (!inherits(x, "Object")) class(x) <- c("Object", "list")
  x <- validate_Object(x)
  x <- validate(x)
  x
}
validate_Object <- function(x) {
  required_fields <- c("a", "b")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Object: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["a"]], "integer")) stop("Validation failed for type Object: field 'a' must be of class integer")
  if (!inherits(x[["b"]], "logical")) stop("Validation failed for type Object: field 'b' must be of class logical")
  x
}
# @case : public function defined
#' @method do Object
`do.Object` <- (function(self) {
self$b
} |> as.Boolean()) |> as.Generic()

truc$Object <- Object
truc$do.Object <- do.Object
registerS3method("do", "Object", do.Object)
})
truc$do <- do
do.Object <- truc$do.Object