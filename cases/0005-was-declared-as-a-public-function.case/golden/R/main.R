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
#@case: add is imported here
add <- scene$add
`c1` <- new_circle(2L |> as.Integer())

`sc` <- new_scene()

#@case: it is said that add is not defined in this scope
print(add(sc, c1))