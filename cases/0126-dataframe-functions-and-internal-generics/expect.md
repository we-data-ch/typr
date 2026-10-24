`dataframe.ty` types `nrow`, `ncol`, `dim`, `names`, `colnames`, `rownames`, `colSums`… for a
`data_frame(...)` argument (`df[#N]{}` accepts any schema). Every overloaded stdlib name gets a
`name <- function(x, ...) UseMethod('name', x)` stub, which strands R primitives that dispatch
internally: `names(<data.frame>)` failed with "no applicable method" because no `names.default`
exists. A `names.default` forwarding to `base::names` is no fix (infinite recursion: the primitive's
own dispatch finds it), so `r_name_lint.rs` restores the primitive (`names <- base::names`).
