`h(n: int, x: [num] & length(>= 1))` called as `h(2, read_x())` with `read_x(): [num]` emits
`h(2L |> as.Integer(), read_x())`: the `length(>= 1)` obligation is dropped. Passing a literal or
a refined argument elsewhere should not cancel the check on the other. Expected a
`typr_refine_length_range(...)` wrapper around `read_x()`.

Related symptom seen while probing: a parasitic `vapply` lifting is generated for `x: [num]`.
