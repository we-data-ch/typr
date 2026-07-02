#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include color.R
#' @include object.R
add <- function(x, ...) UseMethod("add")
scene <- new.env(parent = emptyenv())
local({
Object <- object$Object
Color <- color$Color
Scene <- function(background, fps, height, objects, width, .spread = NULL) {
  explicit <- list()
  if (!missing(background)) explicit[["background"]] <- background
  if (!missing(fps)) explicit[["fps"]] <- fps
  if (!missing(height)) explicit[["height"]] <- height
  if (!missing(objects)) explicit[["objects"]] <- objects
  if (!missing(width)) explicit[["width"]] <- width
  x <- typr_spread_record(explicit, .spread)
  as.Scene(x)
}
as.Scene <- function(x) {
  if (!inherits(x, "Scene")) class(x) <- c("Scene", "list")
  x <- validate_Scene(x)
  x <- validate(x)
  x
}
validate_Scene <- function(x) {
  required_fields <- c("background", "fps", "height", "objects", "width")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Scene: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["background"]], "character")) stop("Validation failed for type Scene: field 'background' must be of class character")
  if (!inherits(x[["fps"]], "integer")) stop("Validation failed for type Scene: field 'fps' must be of class integer")
  if (!inherits(x[["height"]], "integer")) stop("Validation failed for type Scene: field 'height' must be of class integer")
  if (!inherits(x[["width"]], "integer")) stop("Validation failed for type Scene: field 'width' must be of class integer")
  x
}
#' @method new_scene
`new_scene` <- (function() {
Scene(width = 1920L |> as.Integer(), height = 1080L |> as.Integer(), background = "#000000" |> as.Character(), fps = 60L |> as.Integer(), objects = typed_vec(dim = c(0)) |> as.Array1())
}) |> as.Generic()

#' @method add Scene
`add.Scene` <- (function(self, obj) {
Scene(objects = extend(self$objects, obj), .spread = self)
}) |> as.Generic()

scene$Scene <- Scene
scene$new_scene <- new_scene
scene$add.Scene <- add.Scene
registerS3method("add", "Scene", add.Scene)
})
scene$add <- add
add.Scene <- scene$add.Scene