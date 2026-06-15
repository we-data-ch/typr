{
test_that("radius" |> as.Character(), {
`c1` <- new_circle(4L |> as.Integer())

expect_equal(c1, 4L |> as.Integer())
})
}