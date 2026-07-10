# array-covariance-record-subtype

## Ce qui DEVRAIT se passer

Dans le projet `framr`, `Object <- Id & Animable` (un alias `Record & Interface`) et `Circle`
satisfait structurellement `Object` : `Circle` a le champ `id: char` requis par `Id`, et une
fonction libre `animate(self: Circle, animation: Animation): Circle` qui satisfait la méthode
`animate` de l'interface `Animable`. Un appel scalaire `f(c: Circle)` où `f` attend `Object`
type-check déjà correctement (vérifié directement dans `crates/typr-core` avec un cas réduit :
`f <- fn(o: Object): Object { o }; f(c)` passe).

L'utilisateur attend la même chose au niveau tableau : `new_scene` attend `objects: [Object]`,
et lui passe `[c1]` où `c1: Circle`. Comme `Circle <: Object`, il s'attend à ce que
`[Circle] <: [Object]` (les tableaux sont covariants), donc que l'appel type-check.

`Type::is_subtype_raw` (`components/type/mod.rs`) supporte déjà ceci correctement de façon
générale : un test direct confirme que
`Vec(_, _, Circle_alias).is_subtype(&Vec(_, _, Object_alias), &ctx)` retourne `true` — la
covariance structurelle des tableaux ET la satisfaction d'interface fonctionnent très bien au
niveau du type-system générique.

## Anomalies

Le bug n'est PAS dans `is_subtype_raw` mais dans la résolution de surcharge de
`function_application.rs` : `FILTERING 2.5` (`filter_interface_match`) est le mécanisme qui
gère la satisfaction d'interface structurelle pour choisir une signature candidate — mais son
filtre de garde appelle `facets::interface_facet(ctx, &reduced_param)` sur le premier paramètre
réduit **tel quel**. Pour un paramètre scalaire `o: Object`, `reduced_param` se réduit en
`Intersection(Id, Animable)` et `interface_facet` sait en extraire la facette `Interface`
(`Animable`) → le candidat est retenu, et le test `is_subtype_raw` (qui, lui, fonctionne)
confirme le match.

Pour un paramètre tableau `objects: [Object]`, `reduced_param` se réduit en
`Vec(_, N, Intersection(Id, Animable))` — un `Type::Vec` **enveloppant** l'intersection, pas
l'intersection elle-même. `facets::interface_facet` (`processes/type_checking/facets.rs`) ne
matche que `Type::Interface`, `Type::Operator(Intersection, ...)` et `Type::Generic` au niveau
racine ; il n'y a aucun bras pour `Type::Vec`. Résultat : `interface_facet(ctx, [Object]
reduced)` retourne `None`, le candidat `new_scene`/`g` est exclu de `filter_interface_match`
*avant même* que le (correct) test `is_subtype_raw` soit invoqué sur le tableau entier.

Le chemin `FILTERING 1` (`filter_by_first_param` / `match_types_to_generic`) échoue aussi sur ce
cas, mais pour une raison indépendante et non ciblée par ce cas (perte d'identité d'alias par
`reduce_type` avant l'appel à `to_interface`, et usage de `Context::empty()` dans le fallback de
`get_gen_type`) — FILTERING 1 est censé échouer ici de toute façon (il ne gère pas ce genre de
covariance), donc pas de fix requis à cet endroit pour ce cas précis.

Erreur observée (voir `observed.txt`) : `Function new_scene<[1, list{...}]> not defined in this
scope.` — le message générique de "aucune signature ne correspond", alors qu'une signature
existe et devrait matcher.

## Trois bugs typr-core, plus deux pièges côté projet utilisateur

En creusant le repro réel (multi-module), le simple fix de `filter_interface_match` ci-dessus ne
suffisait pas : trois bugs distincts s'empilaient, plus deux pièges dans le projet `framr`
lui-même qui ne sont PAS des bugs typr-core.

**Bugs typr-core corrigés (les 4 changements de cette session) :**

1. `facets::interface_facet` ne regarde jamais à travers un `Type::Vec` → `has_interface_facet`
   ajouté dans `function_application.rs`, unwrap un niveau de tableau avant de déléguer à
   `interface_facet`. C'est le bug "array covariance" tel que rapporté par l'utilisateur.
2. `Context::get_functions_from_type` ne matchait que par égalité stricte `var.get_type() ==
   typ` — cassé dès qu'un type perd son identité d'alias (élément de littéral tableau réduit par
   `typing_container`, OU fonction ré-exportée à travers une frontière de module, où
   `Lang::Module`'s `pub_arg_types` reconstruit un `Var` frais sans le related_type d'origine).
   Fallback ajouté : comparer les formes réduites, puis (second fallback) comparer directement
   le premier paramètre déclaré de la fonction (`typ2.get_first_parameter()`), qui ne peut
   jamais se perdre.
3. `Type::to_interface` substituait `Self` en cherchant `typ` (le type de la requête) tel quel
   dans la signature de la méthode trouvée — échoue silencieusement si `typ` a été réduit
   entre-temps (le cas n°2 ci-dessus). Fix : dériver le type à substituer du premier paramètre
   *déclaré par la fonction elle-même* (`typ2.get_first_parameter()`), qui est fiable quelle que
   soit la façon dont la fonction a été retrouvée.
4. Asymétrie de réduction : au moment de l'export d'un module (`Lang::Module`'s `pub_arg_types`),
   une fonction exportée est réduite dans son ensemble (`Type::Function`'s `reduce_type_helper`
   récurse dans les paramètres) — mais une `interface` exportée ne l'est pas (`reduce_type_helper`
   n'a pas de bras pour `Type::Interface`, donc ses méthodes ne sont jamais recursées). Résultat :
   la méthode retrouvée sur `Circle` a son 2e paramètre (`Animation`) réduit en `Record`, alors
   que la méthode déclarée par `Animable` le garde en `Alias("Animation", ...)` — l'égalité stricte
   `args1 == args2` de `is_subtype_raw`'s bras `Interface` échoue à tort. Fix :
   `interface_methods_satisfy` compare méthode par méthode en réduisant les deux côtés au moment
   de la comparaison, ce qui absorbe l'asymétrie peu importe où/quand chaque côté a été réduit.

**Deux pièges dans le projet `framr` (pas des bugs typr-core, mais à corriger côté projet) :**

- `TypR/circle.ty` référence `Animation` dans la signature d'`animate` sans jamais l'importer
  (`mod animation; use animation::Animation;` manquants) — un alias non résolu retombe
  silencieusement sur `Any` plutôt que de lever une erreur de compilation. Tant que ce n'est pas
  corrigé, `animate`'s vraie signature est `(Circle, Any) -> Circle`, qui ne matche PAS
  structurellement `Animable`'s `(Self, Animation) -> Self` — même avec les 4 fixes ci-dessus.
- La satisfaction d'interface structurelle de TypR n'examine que les fonctions **visibles dans le
  contexte local du fichier appelant** (celles amenées par `use`), pas le programme entier. Même
  `Circle`/`Object`/`Animation` correctement importés, `new_scene([c1])` dans `main.ty` échoue tant
  que `main.ty` n'a pas aussi `use circle::animate;` — alors que `main.ty` n'appelle jamais
  `animate` directement. C'est un comportement existant, potentiellement surprenant, pas quelque
  chose que cette session a changé ou corrigé ; le `repro/` de ce cas inclut cet import pour
  refléter ce qu'il faut réellement écrire aujourd'hui.
