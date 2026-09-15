# external-type-definition-low-tier-degrades-to-any

Source: `typR/registry.md` §5.4 (D2 — "an unreliable definition widens to `Any`, it never fails
a build") and §13 J2, last open checklist item ("`cases/`: définition absente / tier bas /
version trop ancienne → jamais d'erreur dure").

## Ce qui devrait se passer

`gizmo` is locked in `typr.lock` at tier `T3` and cached under
`cache/typr/types/gizmo/<digest>/` (this case bundles that cache directory itself, mirrored at
`$XDG_CACHE_HOME/typr/types/…`, since the real cache lives outside any project directory —
`cases.rs::build_sandbox` now copies a case's `cache/` folder into the sandbox and points
`$XDG_CACHE_HOME` at it before invoking `typr`). Its one declared symbol,
`@spin_gizmo: (int) -> int;`, is a plain declaration with no real R binding behind it.

The project has no `typr.toml`, so `standard_library::load_project_type_definitions` applies the
default `trust = "T2"`. `T3 < T2`, so `names_below_trust` must widen `spin_gizmo` to
`VarType::degrade_to_any` — the `(Any, UnknownFunction)` pair, callable with any argument type,
arity-checked against the declared parameter count.

`main.ty` calls `spin_gizmo("not-an-int")` — the wrong argument *type* for the declared
`(int) -> int` signature, but the right *arity* (one argument). This is a 3-way discriminator:

- degradation broken and the definition never loads at all → `spin_gizmo` is an unknown symbol →
  hard type error ("unknown function"/"Type errors found");
- degradation broken but the definition loads with its literal declared signature → the `string`
  argument doesn't match `int` → hard type error;
- degradation working as designed → arity matches, argument type isn't checked → `typr check`
  succeeds.

## Vérification

Implemented already (`typR/registry.md` §13 J2, "seuil `trust` + règle de dégradation" and
"brancher `typr.lock` sur `check`/`build`/`run`"): `crates/typr-cli/src/standard_library.rs`
(`load_external_ty_definitions`, `names_below_trust`, `load_project_type_definitions`) and
`crates/typr-cli/src/type_registry.rs` (`resolve_locked_definitions`). This case is the
end-to-end regression net through the real CLI that those checklist items were missing —
previously only exercised by `type_registry.rs`'s `#[cfg(test)]` module, not by `typr case run`.

## Statut

Kept as a regression net: any future change that stops loading a locked, cached, low-tier
definition — or that starts enforcing its declared types instead of degrading them — breaks
this case.
