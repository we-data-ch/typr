`fn(a: T, b: U): T { b }` used to type-check: a generic written in a parameter type was
a flexible variable, so `U` unified with `T`. Both generics are now treated as empty-interface
rigid variables (one per name) inside the body, nested ones included (`[#N, T]`, `list { x: T }`).

Expected: both functions of the repro are rejected, with a message naming `T`/`U`
(never `__RIGID_n`). Valid signatures (`fn(a: T, b: U): T { a }`, `fn(a: [#N, T]): [#N, T] { a }`)
must keep compiling; they are covered by unit tests in `type_checking/function.rs`.

Where: `type_checking/function.rs` (`generic_rigids`, `substitute_generics`) and
`components/type/mod.rs` (`is_subtype_raw`: structural comparison of composites holding rigids).
