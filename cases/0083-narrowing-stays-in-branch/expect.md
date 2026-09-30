# 0083 — narrowing is scoped to the branch it proves

The else-branch of `length(v) == 2`, the code after the `if`, and the then-branch of a `||` are not
narrowed: each `dist(v)` keeps its runtime length check.
