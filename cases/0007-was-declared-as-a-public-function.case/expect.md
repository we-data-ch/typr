# : Was declared as a public function

## Ce qui devrait se passer

`typr build` doit générer un `R/animation.R` chargeable par `devtools::load_all()` et
`animate(c1)` (où `c1` est un `Circle`) doit réellement dispatcher vers l'implémentation
`%T`-typée `animate`, puisque `Circle <: %T` au sens du système de types.

## Anomalies trouvées (→ règles `expect.toml`)

1. **`registerS3method` enregistrait la classe littérale `"%T"`** (au lieu d'une classe
   réellement portée par un objet R) : `VarType::get_class_unquoted` retombait sur
   `t.pretty()` pour un type sans alias correspondant, au lieu du fallback `"Generic"`
   utilisé par sa cousine `VarType::get_class`. Aucune valeur R ne porte jamais la classe
   `"%T"`, donc `animate(c1)` levait `no applicable method`.

2. **Le nom de méthode exporté ne correspondait pas au nom réellement défini** :
   `animate_move` était défini dans le corps du module sous `animate_move.Record4`
   (calculé avec `inner_cont`, le contexte ré-typé du corps du module) mais
   réexporté/`registerS3method`-é sous `animate_move.Record1` (calculé avec `cont`,
   le contexte du module *avant* re-typage). Les deux contextes pouvaient nommer le
   même type structurel anonyme (`Animator & Position`) différemment, donnant un nom
   qui n'existait jamais à l'exécution → `objet 'animate_move.Record1' introuvable`.

3. **Même avec (1) et (2) corrigés, le dispatch échouait encore** : aucun `Circle`
   (ni aucun autre record TypR) ne porte jamais la classe littérale `"Generic"` dans sa
   chaîne de classes R (seuls les scalaires `Integer`/`Number`/`Character`/`Boolean`
   l'ont). Le seul suffixe que `UseMethod` retrouve *toujours*, quelle que soit la
   chaîne de classes de l'objet, est `.default`. Le fallback de `get_class`/
   `get_class_unquoted` a donc été changé de `"Generic"` vers `"default"`, alignant
   le suffixe de méthode (`animate.default`) et la classe `registerS3method` (`"default"`)
   sur la seule convention qui dispatche réellement (déjà établie pour `state.default`,
   `factor.default`, `max.default`, etc. — voir CLAUDE.md).

## Localisation (#@case)

- `TypR/circle.ty:10` — : Was declared as a public function
- `TypR/main.ty:5` — : `move` was imported in the scope
- `TypR/main.ty:8` — : `Circle` is a subtype of `Position`
- `TypR/main.ty:10` — : `move` should works on type `Circle`
- `TypR/position.ty:1` — : `Position` type publicly defined here
- `TypR/position.ty:7` — : `move` function publicly defined here

## Correctif (code)

- `crates/typr-core/src/components/context/vartype.rs`: `get_class`/`get_class_unquoted`
  fallback `"Generic"`/`t.pretty()` → `"default"`.
- `crates/typr-core/src/processes/transpiling/mod.rs` (`Lang::Module` export loop):
  `display_type(cont)`/`get_class_unquoted(&var_type)` → `display_type(&inner_cont)`/
  `inner_cont.get_class_unquoted(&var_type)`, matching the context already used to
  render `body_content`.
- `crates/typr-core/src/processes/transpiling/mod.rs` (`Lang::Let`): the `Type::Any(_) |
  Type::Generic(_, _) => ".default"` branch now only special-cases `Type::Any` (whose
  `new_name` is deliberately left unsuffixed by `display_type`); `Type::Generic`/
  `Type::KindedGen` already get `.default` baked into `new_name` via the fixed
  `get_class` fallback, so they fall through the catch-all arm instead of being
  double-suffixed.

## Faux "REGRESS" trouvé et corrigé le 2026-07-18

Ce cas ressortait `REGRESS` (rules fail + golden diff sur `R/animation.R`) sur `main`, avant
même la session Phase D. Investigation : le fix ci-dessus tient toujours — `registerS3method`
utilise bien `"default"`/`animate.default` partout, aucune classe `"%T"` n'apparaît, et les deux
sites (définition + export) utilisent bien le même contexte. Ce qui a dérivé, c'est purement le
compteur interne `RecordN` : le type structurel anonyme `Animator & Position` s'appelait
`Record4` au moment du gel du golden, `Record5` aujourd'hui — un type anonyme de plus a été
enregistré ailleurs dans le programme entre-temps (dérive attendue et documentée, cf.
`CLAUDE.md` § alias hoisting/RecordN). `typr run` sur ce repro exact réussit (exit 0, dispatch
correct) à la fois avec `Record4` (golden original, vérifié en remontant l'historique git) et
`Record5` (aujourd'hui) — donc aucun bug de comportement, seulement un identifiant interne qui a
changé de nom. `expect.toml` pinnait ce nombre en dur (`must_contain =
"animate_move.Record4"`) ; upgradé vers un oracle `r-run` qui teste l'invariant réel (le nom
exporté correspond au nom défini, le dispatch aboutit) sans dépendre du numéro — voir le
commentaire en tête d'`expect.toml`.
