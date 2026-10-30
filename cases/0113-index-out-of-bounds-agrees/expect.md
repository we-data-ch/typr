`v[7]` on a three-element vector stops the program on `r` (`subscript out of bounds`, base R's
`[[`) and on `rust` (`typr_rt::Vector::at`), so `int` never holds `NA` (rfcs/0000-compilation-targets.md
§2). The statement after it must not run. The error text itself is on stderr and is not compared
(R localises it); the differential check compares stdout, which is `[1] 2` on both.
