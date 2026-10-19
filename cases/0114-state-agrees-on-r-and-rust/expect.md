`State<T>` is a shared mutable cell: `t <- s` copies the handle, not the value, so `set(t, 10)`
is visible through `s`. `version` counts the writes.
