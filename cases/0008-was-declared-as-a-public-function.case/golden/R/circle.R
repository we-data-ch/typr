#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include color.R
new_circle <- function(x, ...) UseMethod("new_circle")
circle <- new.env(parent = emptyenv())
local({
Color <- color$Color
Circle <- function(fill, id, position, radius, stroke, .spread = NULL) {
  explicit <- list()
  if (!missing(fill)) explicit[["fill"]] <- fill
  if (!missing(id)) explicit[["id"]] <- id
  if (!missing(position)) explicit[["position"]] <- position
  if (!missing(radius)) explicit[["radius"]] <- radius
  if (!missing(stroke)) explicit[["stroke"]] <- stroke
  x <- typr_spread_record(explicit, .spread)
  as.Circle(x)
}
as.Circle <- function(x) {
  if (!inherits(x, "Circle")) class(x) <- c("Circle", "Position", "Object", "list")
  x <- validate_Circle(x)
  x <- validate(x)
  x
}
validate_Circle <- function(x) {
  required_fields <- c("fill", "id", "position", "radius", "stroke")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Circle: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["fill"]], "character")) stop("Validation failed for type Circle: field 'fill' must be of class character")
  if (!inherits(x[["id"]], "character")) stop("Validation failed for type Circle: field 'id' must be of class character")
  if (!inherits(x[["radius"]], "integer")) stop("Validation failed for type Circle: field 'radius' must be of class integer")
  if (!inherits(x[["stroke"]], "character")) stop("Validation failed for type Circle: field 'stroke' must be of class character")
  x
}
#' @method new_circle character
`new_circle.character` <- (function(id, r) {
Circle(id = id, position = c(0L |> as.Integer(), 0L |> as.Integer()), radius = r, stroke = "#000000" |> as.Character(), fill = "#ffffff" |> as.Character())
}) |> as.Generic()


circle$Circle <- Circle
circle$new_circle.character <- new_circle.character
registerS3method("new_circle", "character", new_circle.character)
})
circle$new_circle <- new_circle
new_circle.character <- circle$new_circle.character