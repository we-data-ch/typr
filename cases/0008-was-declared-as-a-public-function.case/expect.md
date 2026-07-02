# 0008 — `move` sur `Circle` + dispatch `new_storyboard` sur vecteur vide casté

## Ce qui devrait se passer

`typr build` doit produire un package R qui se charge et s'exécute :

1. `c1: Circle` est un sous-type structurel de `Position` (il a `id: char`
   et `position: Vec[2, int]` plus des champs en plus) — `c1.move(...)`
   doit donc dispatcher sur `move.Position` : la chaîne de classes émise
   par `as.Circle` doit contenir `"Position"`.
2. `[] as! [Object]` / `[] as! [Animation]` dans le corps de `new_scene`
   doivent être annotés avec un cast `as.ArrayN()` réel (enregistré dans
   `types.R`, chaîne de classes incluant le type du paramètre
   `objects: [Object]` de `new_storyboard`), pas le fallback
   `as.Generic()` — sinon `new_storyboard(self$objects)` échoue à
   l'exécution avec « pas de méthode applicable ».
3. `snapshot`, fonction **privée** du module storyboard émise comme
   méthode S3 `snapshot.StoryBoard`, doit avoir un stub générique
   `snapshot <- function(x, ...) UseMethod("snapshot")` visible depuis les
   closures du module (dans le `local({...})`).

## Anomalies (→ règles expect.toml)

- `objects = typed_vec(dim = c(0)) |> as.Generic()` dans `R/scene.R` (au
  lieu de `|> as.ArrayN()`) → échec runtime du dispatch `new_storyboard`.
- Pas de stub générique pour la fonction privée `snapshot` → « impossible
  de trouver la fonction "snapshot" » à l'exécution.
- (résolu en amont) chaîne de classes de `Circle` sans `Position`.

## Causes racines (corrigées)

Toutes de la même famille « context drop » ([[bug-lines-single-statement-context-drop]],
constructor-call context drop) : les alias structurels auto-générés
(`ArrayN`, …) enregistrés à la volée pendant le typage étaient perdus
avant la transpilation :

1. `type_checking/function.rs` — `function()` retournait `context.clone()`,
   jetant le contexte du corps (fixé : `hoist_aliases`).
2. `function_application.rs` — `get_expanded_parameters_with_their_types`
   typait les arguments en parallèle et jetait leurs contextes (fixé :
   typage séquentiel + hoist).
3. `type_checking/mod.rs`, arm `Lang::Module` — seul `merge_record_aliases`
   survivait à la frontière de module ; les alias `ArrayN` et le compteur
   étaient perdus (fixé : `hoist_aliases` + injection dans le graphe de
   sous-types via `Context::push_types`/`hoist_aliases`).
4. `context/mod.rs` — `get_matching_alias_signature` ne résolvait pas les
   alias record module-privés (`Object`) apparaissant dans des types
   hoistés → réduction vers `Any` → sous-typage `[0, Object] <: [#N, Object]`
   faux → classe du supertype absente de la chaîne (fixé : fallback sur
   `record_aliases`, le registre whole-program déjà maintenu).
5. `transpiling/mod.rs` — les fonctions typées **privées** d'un module
   n'émettaient ni stub générique `UseMethod` ni `registerS3method`
   (fixé : émis dans le `local({...})`, sans export).

## Hors périmètre

- `storyboard.ty` contient un vrai bug utilisateur : `\(acc, x)
  acc.snapshot(acc)` passe `acc` (StoryBoard) comme paramètre `animation`
  → échec de `validate_Snapshot` à l'exécution (champ `time` manquant).
  Avec `acc.snapshot(x)`, le package se charge de bout en bout. Que le
  type-checker ne signale pas cet argument mal typé dans une lambda
  spécialisée est une lacune de diagnostic distincte (spécialisation de
  lambdas), pas l'objet de ce cas.

## Localisation (#@case)

- `TypR/main.ty:22` — `move` doit marcher sur `Circle` (sous-type de `Position`)
- `TypR/scene.ty:35` — le constructeur invoquait `|> as.Generic()` au lieu de `|> as.ArrayX()`
- `TypR/scene.ty:62` — `new_storyboard` ne trouvait pas d'implémentation pour `self$objects`
- `TypR/position.ty:6` — type `Position` défini publiquement ici
- `TypR/position.ty:12` — fonction `move` définie publiquement ici
