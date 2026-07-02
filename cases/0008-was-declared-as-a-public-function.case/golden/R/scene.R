#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include color.R
#' @include object.R
#' @include animation.R
#' @include storyboard.R
add <- function(x, ...) UseMethod("add")
set_objects <- function(x, ...) UseMethod("set_objects")
render <- function(x, ...) UseMethod("render")
scene <- new.env(parent = emptyenv())
local({
#' @importFrom jsonlite toJSON

Object <- object$Object
Color <- color$Color
Animation <- animation$Animation
set_time <- animation$set_time
new_storyboard <- storyboard$new_storyboard
snapshots <- storyboard$snapshots
Scene <- function(animations, background, fps, height, objects, time, width, .spread = NULL) {
  explicit <- list()
  if (!missing(animations)) explicit[["animations"]] <- animations
  if (!missing(background)) explicit[["background"]] <- background
  if (!missing(fps)) explicit[["fps"]] <- fps
  if (!missing(height)) explicit[["height"]] <- height
  if (!missing(objects)) explicit[["objects"]] <- objects
  if (!missing(time)) explicit[["time"]] <- time
  if (!missing(width)) explicit[["width"]] <- width
  x <- typr_spread_record(explicit, .spread)
  as.Scene(x)
}
as.Scene <- function(x) {
  if (!inherits(x, "Scene")) class(x) <- c("Scene", "Snapshot", "list")
  x <- validate_Scene(x)
  x <- validate(x)
  x
}
validate_Scene <- function(x) {
  required_fields <- c("animations", "background", "fps", "height", "objects", "time", "width")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Scene: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["background"]], "character")) stop("Validation failed for type Scene: field 'background' must be of class character")
  if (!inherits(x[["fps"]], "integer")) stop("Validation failed for type Scene: field 'fps' must be of class integer")
  if (!inherits(x[["height"]], "integer")) stop("Validation failed for type Scene: field 'height' must be of class integer")
  if (!inherits(x[["time"]], "integer")) stop("Validation failed for type Scene: field 'time' must be of class integer")
  if (!inherits(x[["width"]], "integer")) stop("Validation failed for type Scene: field 'width' must be of class integer")
  x
}
#' @method new_scene
`new_scene` <- (function() {
Scene(width = 1920L |> as.Integer(), height = 1080L |> as.Integer(), background = "#000000" |> as.Character(), fps = 60L |> as.Integer(), time = 0L |> as.Integer(), objects = typed_vec(dim = c(0)) |> as.Array5(), animations = typed_vec(dim = c(0)) |> as.Array6())
}) |> as.Generic()

#' @method add Scene
`add.Scene` <- (function(self, ani) {
`a` <- set_time(ani, self$time)

Scene(animations = {
extend(self$animations, ani)
}, time = {
a$time
}, .spread = self)
}) |> as.Generic()

#' @method set_objects Scene
`set_objects.Scene` <- (function(self, objects) {
Scene(objects = objects, .spread = self)
}) |> as.Generic()

#' @method render Scene
`render.Scene` <- (function(self) {
#@case: the R code says new_storyboard can't find implementation for self$objects
snapshots(new_storyboard(self$objects), self$animations)
}) |> as.Generic()

scene$Scene <- Scene
scene$new_scene <- new_scene
scene$add.Scene <- add.Scene
registerS3method("add", "Scene", add.Scene)
scene$set_objects.Scene <- set_objects.Scene
registerS3method("set_objects", "Scene", set_objects.Scene)
scene$render.Scene <- render.Scene
registerS3method("render", "Scene", render.Scene)
})
scene$add <- add
add.Scene <- scene$add.Scene
scene$set_objects <- set_objects
set_objects.Scene <- scene$set_objects.Scene
scene$render <- render
render.Scene <- scene$render.Scene