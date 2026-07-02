#' @include std.R
#' @include generic_functions.R
#' @include types.R
#' @include circle.R
#' @include position.R
#' @include scene.R
#' @include animation.R
#' @include storyboard.R






new_circle <- circle$new_circle
Circle <- circle$Circle
Position <- position$Position
move <- position$move
right <- position$right
Scene <- scene$Scene
new_scene <- scene$new_scene
add <- scene$add
render <- scene$render
StoryBoard <- storyboard$StoryBoard
`c1` <- new_circle("c1" |> as.Character(), 4L |> as.Integer())

`scene` <- new_scene()

#@case: move is not working because Circle is not recognized as a subtype of Position, why?
render(add(scene, move(c1, c(10L |> as.Integer(), 10L |> as.Integer()), 1000L |> as.Integer())))