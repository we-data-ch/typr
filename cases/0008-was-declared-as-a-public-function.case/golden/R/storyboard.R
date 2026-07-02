#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include animation.R
#' @include object.R
new_storyboard <- function(x, ...) UseMethod("new_storyboard")
snapshots <- function(x, ...) UseMethod("snapshots")
storyboard <- new.env(parent = emptyenv())
local({
Animation <- animation$Animation
Object <- object$Object
animate <- object$animate
Snapshot <- function(objects, time, .spread = NULL) {
  explicit <- list()
  if (!missing(objects)) explicit[["objects"]] <- objects
  if (!missing(time)) explicit[["time"]] <- time
  x <- typr_spread_record(explicit, .spread)
  as.Snapshot(x)
}
as.Snapshot <- function(x) {
  if (!inherits(x, "Snapshot")) class(x) <- c("Snapshot", "list")
  x <- validate_Snapshot(x)
  x <- validate(x)
  x
}
validate_Snapshot <- function(x) {
  required_fields <- c("objects", "time")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Snapshot: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  if (!inherits(x[["time"]], "integer")) stop("Validation failed for type Snapshot: field 'time' must be of class integer")
  x
}
StoryBoard <- function(objects, snapshots, .spread = NULL) {
  explicit <- list()
  if (!missing(objects)) explicit[["objects"]] <- objects
  if (!missing(snapshots)) explicit[["snapshots"]] <- snapshots
  x <- typr_spread_record(explicit, .spread)
  as.StoryBoard(x)
}
as.StoryBoard <- function(x) {
  if (!inherits(x, "StoryBoard")) class(x) <- c("StoryBoard", "list")
  x <- validate_StoryBoard(x)
  x <- validate(x)
  x
}
validate_StoryBoard <- function(x) {
  required_fields <- c("objects", "snapshots")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type StoryBoard: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  x
}
#' @method new_storyboard Array1
`new_storyboard.Array1` <- (function(objects) {
StoryBoard(objects = objects, snapshots = typed_vec(dim = c(0)) |> as.Array2())
}) |> as.Generic()

#' @method snapshot StoryBoard
`snapshot.StoryBoard` <- (function(self, animation) {
`objs` <- map(self$objects, function(x) { animate(x, animation) })

`snap` <- Snapshot(time = animation$time, objects = objs)

StoryBoard(objects = objs, snapshots = extend(self$snapshots, snap))
}) |> as.Generic()

#' @method snapshots StoryBoard
`snapshots.StoryBoard` <- (function(self, actions) {
fold(actions, self, function(acc, x) { snapshot(acc, acc) })
}) |> as.Generic()

storyboard$Snapshot <- Snapshot
storyboard$StoryBoard <- StoryBoard
storyboard$new_storyboard.Array1 <- new_storyboard.Array1
registerS3method("new_storyboard", "Array1", new_storyboard.Array1)
snapshot <- function(x, ...) UseMethod("snapshot")
registerS3method("snapshot", "StoryBoard", snapshot.StoryBoard)
storyboard$snapshots.StoryBoard <- snapshots.StoryBoard
registerS3method("snapshots", "StoryBoard", snapshots.StoryBoard)
})
storyboard$new_storyboard <- new_storyboard
new_storyboard.Array1 <- storyboard$new_storyboard.Array1
storyboard$snapshots <- snapshots
snapshots.StoryBoard <- storyboard$snapshots.StoryBoard