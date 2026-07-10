#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include animation.R
get_object_by_id <- function(x, ...) UseMethod("get_object_by_id")
object <- new.env(parent = emptyenv())
local({
Animation <- animation$Animation

Id <- function(id, .spread = NULL) {
  explicit <- list()
  if (!missing(id)) explicit[["id"]] <- id
  x <- typr_spread_record(explicit, .spread)
  as.Id(x)
}
as.Id <- function(x) {
  if (!inherits(x, "Id")) class(x) <- c("Id", "list")
  x <- validate_Id(x)
  x <- validate(x)
  x
}
validate_Id <- function(x) {
  required_fields <- c("id")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Id: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["id"]], "character")) stop("Validation failed for type Id: field 'id' must be of class character")
  x
}
Object <- function(id, .spread = NULL) {
  explicit <- list()
  if (!missing(id)) explicit[["id"]] <- id
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
  required_fields <- c("id")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Object: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["id"]], "character")) stop("Validation failed for type Object: field 'id' must be of class character")
  x
}
#' @method get_object_by_id Array1
`get_object_by_id.Array1` <- (function(objects) {
NA
} |> as.Object()) |> as.Generic()

object$Id <- Id
object$Object <- Object
object$get_object_by_id.Array1 <- get_object_by_id.Array1
registerS3method("get_object_by_id", "Array1", get_object_by_id.Array1)
})
object$get_object_by_id <- get_object_by_id
get_object_by_id.Array1 <- object$get_object_by_id.Array1