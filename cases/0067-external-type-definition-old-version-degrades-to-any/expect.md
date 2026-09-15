# external-type-definition-old-version-degrades-to-any

Source: `typR/registry.md` §7.2 ("Compatibilité de versions : borne minimale, pas plage
fermée") and §13 J2, last open checklist item.

## Ce qui devrait se passer

`sprocket` is locked at tier `T1` — normally trusted outright, regardless of the project's
`trust` threshold — but `typr.lock` records `r_version_seen = "1.0.0"`, below the manifest's
`since = "2.0.0"` floor. Per §7.2, a version below `since` must **warn and degrade to `Any`**,
never refuse the build, and the degradation applies to *every* declared name in the definition,
independent of that name's own tier (`standard_library::degrade_if_version_out_of_range` widens
`all_declared_names`, not just the ones under `trust`).

`main.ty` calls `turn_sprocket("not-an-int")` — right arity, wrong argument type for the
declared `(int) -> int` — the same 3-way discriminator as case
`0066-external-type-definition-low-tier-degrades-to-any`: unknown symbol or an enforced strict
signature would both raise a hard type error; a correctly degraded `Any` function does not.

## Vérification

Implemented already: `crates/typr-cli/src/standard_library.rs`
(`degrade_if_version_out_of_range`, `version_less_than`) and
`crates/typr-cli/src/type_registry.rs` (`resolve_locked_definitions`,
`observed_r_package_version`). Previously only exercised by unit tests in those two files'
`#[cfg(test)]` modules (`load_project_type_definitions_degrades_when_locked_version_is_below_since`)
— this case is the same scenario replayed through the real `typr` binary via `typr case run`,
using a case-bundled `cache/` directory (`cases.rs::build_sandbox`, `$XDG_CACHE_HOME`) since the
real cache lives outside any project directory (§7.4).

## Statut

Kept as a regression net for the version-floor half of D2/§7.2, alongside case
`0066-external-type-definition-low-tier-degrades-to-any` for the tier half.
