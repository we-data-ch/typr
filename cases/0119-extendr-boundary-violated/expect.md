`[rust] platform = "extendr"` makes `typr build` check the R boundary before generating anything.
`area` takes a `Shape`, a union: only scalars, vectors and records cross into R, so the build stops
with T054 on `area`. `double` takes an `int` and is not reported
(rfcs/0000-compilation-targets.md §6).
