Differential case (rfcs/0000-compilation-targets.md §8, Step 5): monomorphised generic functions
(one instance per call-site type), a generic record (`Box<T>`), higher-order generics and closures
capturing their environment. Scalars are wrapped in vectors: a boxed scalar prints differently on
`r` (case 0112).
Left out because the `r` target itself fails on them (both work on `rust`, see
`tests/rust_target.rs`): a generic function over a bare vector (`first(xs: [#N, T])`: `UseMethod`
finds no method for a plain `integer`), and a generic tag union (`let a: Opt<int> <- .Some(4)`:
`as.Opt` is never defined).
