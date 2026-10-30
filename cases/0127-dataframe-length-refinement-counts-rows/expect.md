`typr_refine_length` / `typr_refine_length_range` measured `length(x)`, which for a data.frame is the
number of columns. A refinement `df[#N]{...} & length(> 0)` therefore passed an empty data frame
(1 column, 0 rows). The prelude helpers now measure `nrow()` for a data frame
(`typr_refine_size`).

The oracle checks the obligation is still emitted and that the prelude measures rows. The R side was
also checked by hand: `typr_refine_length_range(data.frame(a = numeric(0)), 0, TRUE, Inf, FALSE, "t")`
now stops with "length 0 is out of bounds (0, Inf)".

Also fixed here: a user function with a data-frame parameter was emitted as `f.ArrayN`, which a plain
data.frame never has as a class, so any call failed with "no applicable method". The S3 suffix of a
data-frame parameter (refined or not) is now `data.frame` (`VarType::get_class`/`get_class_unquoted`).
Known limit: two overloads of one function whose parameters are data frames of different schemas
share the `f.data.frame` method and collide.
