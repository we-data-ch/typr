# Chained UFCS call on a record alias

## Ce qui devrait se passer

`sc.add(c1).add(c1).print()` doit type-checker et se transpiler sans erreur :
chaque `.add(...)` doit rester applicable sur le résultat du précédent, puisque
`add: (self: Scene, obj: Object) -> Scene` retourne toujours un `Scene`.

## Anomalies trouvées

1. **Bug du repro (cause directe de l'erreur observée)** : `main.ty` appelait
   `sc.add(c1).add(c2)` alors que `c2` n'est jamais défini dans ce fichier — une
   faute de frappe (probablement `c1` voulu). Une variable indéfinie est
   actuellement typée silencieusement `Any` par
   `Context::get_type_from_existing_variable` (aucun `TypeError` dédié
   n'existe pour ce cas), donc l'erreur remonte plus loin sous une forme
   confuse : `Function add<list{...record Scene expansé...}> not defined in
   this scope` au lieu de pointer sur `c2`. Corrigé ici en remplaçant `c2` par
   `c1` dans le repro. Le gap plus large (variable indéfinie sans diagnostic
   dédié, à l'origine de ce message trompeur) est noté mais volontairement
   laissé de côté : il touche `Context::get_true_variable` /
   `get_type_from_existing_variable`, partagé avec la logique LSP qui filtre
   déjà des faux positifs sur les imports cross-fichiers (voir
   `project_lsp_diagnostics` en mémoire) — un changement plus large et risqué,
   hors scope de ce cas.

2. **Bug réel trouvé en creusant (corrigé dans typr-core)** :
   `UnificationMap::apply_unification_type` (crates/typr-core/src/processes/type_checking/unification_map.rs)
   réduisait *systématiquement* le type de retour d'un appel de fonction dès
   qu'aucune substitution générique ne s'appliquait — ce qui est le cas pour
   *toute* fonction non générique, pas seulement les alias primitifs comme
   `type Int <- int`. Pour un alias record (`type Scene <- list {...}`), ça
   expanse le retour vers le record structurel complet et fait perdre le nom
   `Scene`. Dans ce repro précis, la résolution de surcharge reste structurelle
   (elle réduit aussi le côté candidat avant de comparer), donc l'alias perdu
   ne cassait pas *ce* cas — mais cassait le type affiché/inferé dans d'autres
   contextes (ex: `typr debug --types` affichait `list{width: int, height:
   int}` au lieu de `Scene` après un appel chaîné). Fixé en ne réduisant
   l'alias que s'il ne reproduit pas une structure porteuse d'identité de
   dispatch (`Record`/`Tag`/`Operator`) ; testé par
   `test_chained_call_preserves_record_alias_return_type` dans
   `function_application.rs`.

## Localisation

- `TypR/main.ty:19` — `sc.add(c1).add(c1).print()` (corrigé : était `c2`)
- `TypR/scene.ty:28` — `add: (self: Scene, obj: Object) -> Scene`
- `crates/typr-core/src/processes/type_checking/unification_map.rs:135` —
  `apply_unification_type`, le fallback de réduction inconditionnelle corrigé
