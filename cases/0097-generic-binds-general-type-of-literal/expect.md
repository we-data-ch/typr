# 0097 — a generic binds the general type of a literal

`f: (a: T, b: T) -> T` called as `f(3, 5)` was rejected because `T` was bound
to the literal types `3` then `5`, which differ. A generic now stands for the
general type: `7` becomes `int`, `"hello"` `char`, `true` `bool`, `1.5` `num`.
Mixed calls (`f(3, "a")`) are still rejected.
