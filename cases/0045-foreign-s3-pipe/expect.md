## Should happen

`m |> print()` (pipe syntax sugar for `print(m)`) must not touch `m` at
all — it should show the usual `Call:`/`Coefficients:` `lm` output.

## Localisation (#@case)

Part of Phase D.4 of `soundness_plan.md`. The pipe operator desugars to a
plain function call (`Op::Pipe` handling in
`processes/type_checking/dot_pipe_access.rs` / the transpiler's matching
arm) before any type-directed casting logic runs, so there was no reason
to expect a bug here distinct from a plain function call (already covered
by `0019-foreign-lm-function-arg`) — confirmed directly rather than assumed.
