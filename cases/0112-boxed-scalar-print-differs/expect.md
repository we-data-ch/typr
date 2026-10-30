`print.Integer`/`.Character`/`.Boolean`/`.Number` in `std.R` use `cat(unclass(x), fill = TRUE)`, so a
value that still carries the TypR box prints bare, while the same value out of a base function
(`length(v)`, `v[2]`) prints `[1] 3`. The result depends on whether the value happens to be boxed.
Which format is the portable one is an open question (RFC, Step 4 notes). Until it is decided, the
divergence is recorded here; the case fails the day the two agree.
