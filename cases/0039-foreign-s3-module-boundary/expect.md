## Should happen

`describe(m)` must dispatch correctly on `m`'s real `lm` class **even
though `describe` is declared as a `@pub fn` inside `module Reporter { ...
}` rather than at the top level**. `print(x)` inside `describe` should show
the usual `Call:`/`Coefficients:` `lm` output.

## Localisation (#@case)

Part of Phase D.2 of `soundness_plan.md`: a structural gap found while
auditing the module post-processing block in
`crates/typr-core/src/processes/transpiling/mod.rs` — a `Foreign<T>`-family
(or pure-interface) dispatch parameter makes the `Lang::Let` transpile arm
emit a `raw_name.default <- typed_name` fallback binding (the mechanism bug
#4 of the interop matrix's four original fixes relies on), but that binding
only ever existed *inside* the module's `local({...})` block. The
post-processing loop (~line 2165-2224 as of the plan doc) re-exported
`typed_name` (e.g. `describe.Foreign0`) to both the module env (`M$...`)
and top level, but never did the same for `raw_name.default`
(`describe.default`) — so a real foreign value's runtime class (`"lm"`)
never matches `describe.Foreign0`'s suffix, `describe.default` is the only
method that *could* catch it, and that method was invisible outside the
module. Confirmed directly: reverting the fix and rebuilding
(`--no-incremental`) reproduces exactly the predicted failure —
`Erreur dans UseMethod("describe") : pas de méthode pour 'describe'
applicable pour un objet de classe "lm"`.

Fixed by adding the same export + re-expose treatment for
`raw_name.default` as already existed for `typed_name`, gated on the exact
same condition (`is_foreign_dispatch || (pure interface facet)`) already
used by the `Lang::Let` arm that originally emits the `.default` binding.
