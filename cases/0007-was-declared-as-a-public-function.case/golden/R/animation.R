#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include position.R
animate <- function(x, ...) UseMethod("animate")
animate_move <- function(x, ...) UseMethod("animate_move")
animation <- new.env(parent = emptyenv())
local({
Position <- position$Position
move <- position$move

Animator <- function(animations, .spread = NULL) {
  explicit <- list()
  if (!missing(animations)) explicit[["animations"]] <- animations
  x <- typr_spread_record(explicit, .spread)
  as.Animator(x)
}
as.Animator <- function(x) {
  if (!inherits(x, "Animator")) class(x) <- c("Animator", "list")
  x <- validate_Animator(x)
  x <- validate(x)
  x
}
validate_Animator <- function(x) {
  required_fields <- c("animations")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Animator: missing fields: ", paste(missing_fields, collapse = ", ")))
  }
  x
}
#' @method animate default
`animate.default` <- (function(target) {
`animations` <- list(animations = typed_vec(dim = c(0)) |> as.Generic()) |> as.Record3()

validate_Animator(spread(target, animations))
}) |> as.Generic()

#@case: The function animate_move was not found when generated see ../R/R_error.md
#' @method animate_move Record4
`animate_move.Record4` <- (function(a, direction) {
validate_Animator(move(a, direction))
}) |> as.Generic()

animation$animate.default <- animate.default
registerS3method("animate", "default", animate.default)
animation$animate_move.Record4 <- animate_move.Record4
registerS3method("animate_move", "Record4", animate_move.Record4)
})
animation$animate <- animate
animate.default <- animation$animate.default
animation$animate_move <- animate_move
animate_move.Record4 <- animation$animate_move.Record4