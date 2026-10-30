Differential case (rfcs/0000-compilation-targets.md §8): `typr run --target r` and `--target rust`
must print the same thing. Covers `typr_rt::Vector` (Step 4): element-wise operators, R's
recycling rule (`v + [1, 2]` warns on stderr, which is not compared), common decimals for `num`
vectors, left-aligned `char` vectors, line wrapping with `[26]` labels at 80 columns, 1-based
indexing and slicing, `for`.
Only vectors and results of base functions are printed: a boxed scalar prints differently on `r`
(case 0112).
