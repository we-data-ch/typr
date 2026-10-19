Where the checker cannot prove a refinement (`let a: [3, int] <- v`, an argument, a returned value) it
records an obligation (`refinement_check.rs`); `r` wraps the expression in `typr_refine_length` /
`typr_refine_value`, and `rust` in `typr_rt::refine_length` / `refine_value` (rfcs/0000-compilation-targets.md,
Step 4 follow-up). `mk(short)` returns a vector of length 2 where 3 is required: the program stops there on both targets
(`Type refinement violation at main.ty:N: expected length 3, got 2`, on stderr, not compared), so the
last `print(a)` must not run. Stdout is `[1] 1 2 3`, `[1] 5 6`, `[1] 1 2 3`.
