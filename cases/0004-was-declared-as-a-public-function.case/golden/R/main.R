#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include circle.R
#' @include position.R
#' @include scene.R




new_circle <- circle$new_circle
Circle <- circle$Circle
Position <- position$Position
move <- position$move
right <- position$right
Scene <- scene$Scene
new_scene <- scene$new_scene
`c1` <- new_circle(2L |> as.Integer())

print(c1)