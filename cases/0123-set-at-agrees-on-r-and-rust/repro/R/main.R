#' @include std.R
#' @include generic_functions.R
#' @include types.R

print(
  c(join(
    c("a" |> as.Character(), "b" |> as.Character(), "c" |> as.Character()) |>
      identity(),
    "," |> as.Character()
  )) |>
    identity()
)
print(
  c(join(c("solo" |> as.Character()) |> identity(), "-" |> as.Character())) |>
    identity()
)
print(
  c(contains("ell" |> as.Character(), "hello" |> as.Character())) |> identity()
)
print(
  c(contains("xyz" |> as.Character(), "hello" |> as.Character())) |> identity()
)
print(
  c(contains("" |> as.Character(), "hello" |> as.Character())) |> identity()
)
