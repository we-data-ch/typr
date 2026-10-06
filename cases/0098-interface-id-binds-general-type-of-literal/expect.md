# 0098 — a repeated bare interface binds the general type of a literal

`f: (a: Simple, b: Simple) -> Simple` called as `f(3, 5)` was rejected with
"'Simple' is bound to '3' and then to '5'": the shared id was bound to the
literal types. It now binds the general type (`int`), as a free generic does.
