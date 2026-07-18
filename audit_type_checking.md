# Audit du type-checking — edge cases non couverts & plan de mitigation

*Date : 2026-07-17. Périmètre : `crates/typr-core/src/processes/type_checking/` (eval + typing + fichiers extraits). Basé sur lecture du code, pas encore sur repro systématique — chaque item doit être confirmé par un test avant correction.*

Chaque item a un identifiant (`S1`, `M2`, …) pour référencement dans les commits/cases.

---

## 1. Failles systémiques : dégradation silencieuse vers `Any`

Le pattern dominant du checker en cas d'imprévu est « retourner `Any` (ou `UnknownFunction`) sans erreur ». C'est la source n°1 d'edge cases futurs : chaque nouvelle construction qui tombe dans un de ces trous compile sans diagnostic et explose au runtime R.

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| S1 | ~~`mod.rs:1634-1660` (`Lang::Variable`)~~ | ✅ **FIXÉ (2026-07-17)** : après le check existant « membre de module non importé » (`VariableNotImported`), le même `context.get_type_from_variable(&old_var).is_err()` déclenche maintenant `TypeError::UndefinedVariable` si le nom n'est résolu nulle part — y compris les placeholders R/JS non typés (`functions_R.txt`), qui continuent de résoudre normalement puisqu'ils sont déjà présents dans `variables` (`get_type_from_variable` réussit pour eux). Vérifié : suite complète (`cargo test --workspace`, 534+31+56 tests), et un vrai projet (`typr new`+`build`+`run`) exerçant `print`/`c`/`sum`/`map`/record/`module`+`use` compile et s'exécute sans régression ; une variable réellement indéfinie produit désormais `Undefined variable 'name'` avec position source. Tests : `edge_s1_undefined_variable_is_an_error`, `edge_s1_untyped_r_builtin_still_resolves` (mod.rs). | ~~**Haute**~~ |
| S2 | ~~`mod.rs:2135`~~ | ✅ **FIXÉ (2026-07-17)** : catch-all `_ => any_type()` de `typing()` remplacé par un match exhaustif sur les 62 variantes de `Lang`. A révélé deux vrais bugs cachés derrière ce catch-all, corrigés dans la foulée : **`Lang::NA`** ne typait jamais en `Type::NA` (toujours `Any`) ; **`Lang::WhileLoop`** n'avait *aucun* arm — condition et corps d'un `while` n'étaient jamais type-checkés du tout. Les 4 variantes restantes (`VecFunctionApp`, `Exp`, `TypePattern`, `Union`) sont des artefacts de réécriture post-typing ou du code mort ; elles ont un arm explicite documenté (comportement `Any` inchangé) au lieu du wildcard, pour que toute future variante `Lang` force une erreur de compilation ici. Tests rouges → verts : `edge_na_literal_types_as_na_not_any`, `edge_while_loop_condition_must_be_boolean`, `edge_while_loop_body_type_error_detected`. | ~~**Haute**~~ |
| S3 | ~~`mod.rs:666-672`~~ | ✅ **FIXÉ (2026-07-17)** : catch-all `_ => unknown_function_type()` de `eval()` remplacé par un match exhaustif listant explicitement les 53 variantes restantes (toutes délèguent au même comportement — `eval()` n'est provablement appelé qu'avec les 9 variantes déclaration/statement déjà gérées). Pas de bug caché trouvé ici (aucun appelant externe), mais même garde-fou structurel installé. | ~~Moyenne~~ |
| S4 | `type_comparison.rs:87-89` | Alias non résolu → `Any` dans `reduce_type`. Mitigé par `collect_undefined_aliases` sur les chemins `let`/`fn`/`type` seulement ; tout autre chemin (scrutiné de `match`, type de cast, champ profond, générique d'alias…) passe encore sans erreur. | Moyenne |
| S5 | `mod.rs:1803-1807` (`Lang::Lambda`) | Les erreurs du typage initial du corps d'une lambda sont **jetées** (délibérément, pour la re-spécialisation). Une lambda jamais appliquée n'est donc jamais vérifiée : `let f <- \(x) inexistant(x);` sans appel = zéro diagnostic. | Moyenne |
| S6 | `mod.rs:559-562` (`Lang::Module`) | Un membre `@pub` dont le typage a échoué est exporté comme `empty_type()` via `unwrap_or_else` — le module se construit, l'importeur voit un type incohérent en aval. | Moyenne |
| S7 | `mod.rs:248-256` (`Lang::Alias`) | Identifiant d'alias non convertible en `Var` → retour silencieux `unknown_function_type()`, la déclaration est avalée. | Basse |

**Chiffre :** `mod.rs` seul contient 17 usages de `any_type()`, dont plusieurs sans erreur associée.

---

## 2. `match` : le plus gros trou fonctionnel (`match_expression.rs`, 135 lignes, 0 test inline)

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| M1 | ~~`match_expression()`~~ | ✅ **FIXÉ (2026-07-17)** : exhaustivité vérifiée via `collect_tag_variants` (marche le même arbre `Operator(Union, ...)` que M2/M5) ; `TypeError::NonExhaustiveMatch` si des variantes manquent et qu'aucune branche catch-all (`_ =>` ou `nom =>`) n'est présente. **Gardé volontairement inactif** quand le scrutiné est un `Type::Tag` isolé (pas un vrai union à 2+ variantes) : un `let x <- .Circle(3);` non annoté infère ce type étroit (une seule variante concrète), pas l'alias déclaré au complet — vérifier l'exhaustivité dessus aurait donné des faux positifs constants sur l'usage TypR ordinaire. Vérifié en pipeline réel (`typr build`) : un match à 3 variantes n'en couvrant que 2 produit `This match doesn't cover every variant` avec la position exacte. | ~~**Haute**~~ |
| M2 | ~~`build_match_branch_context:25-35`~~ | ✅ **FIXÉ (2026-07-17)** : le scrutiné passe maintenant par `reduce_type` avant dispatch, et le binding d'un pattern tag marche l'arbre `Operator(Union, ...)` via `find_tag_variant` au lieu de dépendre d'un match exact `Type::Tag` ou du cas `"Option"` codé en dur (supprimé) — marche uniformément pour tout union utilisateur ET pour `Option<T>` (même mécanisme de réduction générique via `reduce_alias`). | ~~**Haute**~~ |
| M3 | ~~`build_match_branch_context:104` (`_ =>`)~~ | ✅ **FIXÉ (2026-07-17)** : les formes de pattern non gérées (élément non-`Variable` dans un pattern tag/tuple/record) poussent maintenant `TypeError::UnsupportedPattern` au lieu de dropper silencieusement le binding. Note : `.Some(.Some(x))` s'avère ne **pas parser du tout** aujourd'hui (le parser de pattern tag n'accepte qu'un identifiant simple entre parenthèses) — ce cas précis n'atteint donc jamais le type-checker ; le filet ajouté couvre les formes qui, elles, parsent bien (ex. un élément non-variable dans un pattern tuple/record). | ~~**Haute**~~ |
| M4 | ~~`match_expression()`~~ | ✅ **FIXÉ (2026-07-17)** : `TypeError::PatternTypeMismatch` quand un pattern tag nomme une variante absente du vrai union du scrutiné, ou quand un pattern tuple/record est utilisé contre un scrutiné qui ne l'est pas — **seulement** quand le scrutiné est un vrai `Operator(Union, ...)` à 2+ variantes (même garde-fou que M1, même raison : un `Type::Tag` isolé ne permet pas de distinguer "vraiment inconnu" de "pas la variante concrète actuelle mais un membre valide de l'alias"). | ~~Moyenne~~ |
| M5 | ~~`build_match_branch_context:80-83`~~ | ✅ **FIXÉ (2026-07-17)** : couvert par le même `reduce_type` en amont que M2 — un scrutiné `Type::Alias("Point")` se réduit en `Type::Record` avant extraction des champs. | ~~Moyenne~~ |
| M6 | — | Pas de détection de branches dupliquées / inaccessibles. Toujours ouvert (hors scope Phase 2). | Basse |

---

## 3. Opérateurs & flux de contrôle (`typing()` inline)

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| O1 | ~~`mod.rs:1183`~~ | ✅ **FIXÉ (2026-07-18)** : les 5 comparaisons (`==`, `<=`, `>=`, `>`, `<`) réduisent maintenant les deux opérandes (`reduce_type`) avant de les comparer, et acceptent en plus explicitement un mélange `int`/`num`, ou l'un sous-type de l'autre (`is_subtype` bidirectionnel) — pas seulement l'égalité stricte réduite. Couvre l'alias vers son type sous-jacent (`type Meters <- int; let m: Meters <- 5; m == 5`) et `1 < 1.5`. | ~~**Haute**~~ |
| O2 | ~~`mod.rs:1137`, `mod.rs:1729`~~ | ✅ **FIXÉ (2026-07-18)** : `&&`/`||`, `!` (`Lang::Not`) et la condition du `if` appellent maintenant `reduce_type` avant `is_boolean()` — un alias vers `bool` (`type Flag <- bool`) passe. Bonus (hors périmètre O2 initial, même bug) : la condition du `while` avait le même défaut, corrigée au même endroit. | ~~Moyenne~~ |
| C1 | ~~`mod.rs:1766`~~ | ✅ **FIXÉ (2026-07-18)** : nouveau champ `Context::expected_return_type` (parallèle à `self_type`), posé par `function()` sur le sub-contexte avant de typer le corps ; l'arm `Lang::Return` du `typing()` compare maintenant la valeur retournée à ce type via `function::is_compatible_return_type` (fonction partagée, extraite de la logique déjà utilisée pour l'expression finale — mêmes trois clauses : opacité, `reduce_and_subtype`, générique rigide) et pousse `TypeError::UnmatchingReturnType` sur incompatibilité. `fn(x: int): int { if (x > 0) { return "a"; } else { 1 } }` est maintenant rejeté ; le cas compatible (`return 5;`) ne l'est pas (pas de faux positif). | ~~**Haute**~~ |
| C2 | ~~`mod.rs:1111-1112`~~ | ✅ **FIXÉ (2026-07-18)** : nouveau flag `Context.config.in_loop` (même patron que `in_module_body`), posé à `true` sur le sous-contexte utilisé pour typer le corps de `Lang::Loop`/`WhileLoop`/`ForLoop` ; les arms `Lang::Break`/`Lang::Next` vérifient `context.is_in_loop()` et poussent la nouvelle erreur `TypeError::LoopControlOutsideLoop` sinon. `break;`/`next;` au top niveau sont désormais rejetés ; à l'intérieur d'une boucle (`while`/`for`), toujours acceptés. | ~~Basse~~ |
| C3 | ~~`mod.rs:1243-1254`~~ | ✅ **Pas un bug vivant (vérifié 2026-07-18)** : la mémoire `bug_lines_single_statement_context_drop` décrivait le fast-path `exprs.len() == 1` comme reconstruisant le `TypeContext` depuis le contexte *pré-statement* — mais le code actuel utilise déjà `tc.context` (le contexte *retourné* par le typage du statement, avec ses bindings) pour la branche `_out`, pas le contexte d'entrée. Testé directement via `typing_with_errors`/`parse_from_string` (chemin réel, `Lang::Lines` à 1 élément) et via `FluentParser` (le scénario exact que citait la mémoire) : dans les deux cas `x` de `let x <- 5;` seul en top-level est bien présent dans le contexte final. Mémoire et audit obsolètes sur ce point — corrigés ici. Test de régression : `edge_c3_lines_single_statement_retains_binding` (`mod.rs`). | ~~Moyenne~~ |
| C4 | ~~`mod.rs:1310-1314`~~ | ✅ **FIXÉ (2026-07-18)** : le résultat d'un `if`/`else` collapse maintenant quand les deux branches réduites (`reduce_type`) sont égales ou que l'une est sous-type de l'autre (bidirectionnel, même patron que O1) — plus seulement sur égalité stricte des types non réduits. `if (c) { p } else { q }` avec `p: Point` et `q: list{x:int,y:int}` structurellement identiques donne maintenant un seul type au lieu de `Point \| Record`. Test de régression : `edge_c4_if_else_collapses_structurally_identical_branches` (`mod.rs`). | ~~Basse~~ |

---

## 4. Indexation, vecteurs, data frames

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| I1 | ~~`mod.rs:1626-1649`~~ | ✅ **Déjà résolu (vérifié 2026-07-18, fix antérieur à cet audit — commit `4bce238` « fixed vector negative indexation "a[-1]" »)** : ce n'est pas un bug ouvert, l'audit était simplement obsolète sur ce point. TypR a tranché pour sa **propre** sémantique (« depuis la fin », pas l'exclusion R) et l'a codée des deux côtés en cohérence : le type-checker type `v[-1]` en le type élément, et `transpiling/mod.rs` émet un codegen R dédié (`v[[length(v)]]`, `v[[length(v) - nL]]`, …) au lieu de laisser passer `v[-1]` tel quel — donc pas de divergence type/runtime. Vérifié en pipeline réel (`typr debug --r`) : `v[-1]` sur `[int]` type en `int` et transpile en `v[[length(v)]]`. | ~~**Haute**~~ |
| I2 | ~~`mod.rs:1651-1681`~~ | ✅ **FIXÉ (2026-07-18) — plus grave que décrit** : ce n'était pas juste « pas de check », c'était un **panic Rust** (`not implemented for language Variable`) : `Lang::len()` — appelé sur chaque membre d'index via `x.len()` pour bâtir la sonde de taille statique — ne gère que `Integer`/`Array`/`Vector` et panique sur tout le reste, donc tout index dynamique non littéral (`v[i]`, `v[f(x)]`) plantait le compilateur entier. Fix : un membre d'index qui n'est pas une de ces 3 formes littérales est maintenant type-checké et doit réduire vers `Type::Integer` (sinon `TypeError::WrongIndexing`) ; la sonde de taille dégrade vers `Tint::Unknown` pour cette dimension (pas de borne statique possible sur une valeur inconnue à la compilation), au lieu de paniquer. Vérifié par 2 tests (`edge_i2_dynamic_index_does_not_panic_and_is_checked`, `edge_i2_non_int_dynamic_index_is_an_error`, `mod.rs`) et en pipeline réel (`typr debug --r` : `v[i]` transpile proprement en `v[[i]]` ; `typr check` sur un index `char` produit un diagnostic propre, pas un crash). | ~~Moyenne~~ |
| D1 | ~~`mod.rs:1544-1583` (`DataFrame`)~~ | ✅ **FIXÉ (2026-07-18)** : décision utilisateur = ajouter les deux vérifications. Nouvelles erreurs `TypeError::DataFrameColumnNotVector` (colonne non-vecteur, au lieu d'un index `Any` silencieux) et `TypeError::DataFrameColumnLengthMismatch` (deux colonnes de longueurs statiquement connues et différentes, comparées via l'index `Type::Integer(Tint::Val(n))` de chaque colonne). Vérifié par 3 tests (`edge_d1_dataframe_matching_column_lengths_is_allowed`, `edge_d1_dataframe_column_length_mismatch_is_an_error`, `edge_d1_dataframe_non_vector_column_is_an_error`, `mod.rs`) et en pipeline réel (`typr debug --r`). Note annexe (pré-existante, hors scope) : le runtime R de `data.frame(...)` lui-même échoue déjà à l'exécution (`as.data.frame.default` ne sait pas convertir la classe `typed_vec` interne) — confirmé identique sur `main` avant ce fix via `git stash`, donc un gap runtime séparé et non lié à cette vérification statique. | ~~Moyenne~~ |

---

## 5. Génériques, alias, unification

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| G1 | ~~`unification.rs:397-409` (`merge_substitutions`) + `unification_map.rs` (`SafeHashMap::insert`)~~ | ✅ **FIXÉ (2026-07-17)** : deux sites distincts avaient ce bug, tous les deux corrigés. (1) `merge_substitutions` (dans `unification.rs`, unification d'un **seul** type composite — ex. `Function(T,T)->T` unifié directement contre `Function(int,char)->ret`) retourne maintenant `bool`, et un binding contradictoire (`T↦int` puis `T↦char` après `resolve_in_chain`) fait échouer l'unification au lieu d'écraser silencieusement. (2) `SafeHashMap::insert` (dans `unification_map.rs`, le site qui reproduit *exactement* l'exemple de l'audit : `f(x: T, y: T)` appelé `f(1, "a")` — chaque argument est unifié **séparément** par `Context::get_unification_map`, donc c'est cette étape de fusion, pas `unification_helper`, qui voyait passer les deux bindings contradictoires) retourne aussi `bool` désormais ; `UnificationMap::new` → `try_new(..) -> Option<Self>`, propagé jusqu'à `get_unification_map`. Vérifié par test unitaire (`unification_map::tests::test_try_new_rejects_conflicting_generic_bindings`) et par pipeline réel : `typr check` sur `let same <- fn(x:T,y:T):T{x}; same(1,"a")` échoue maintenant avec `Function same<int> not defined in this scope` (rejeté), alors que `same(1,2)` continue de passer. | ~~**Haute**~~ |
| G2 | ~~`unification.rs:236-352` (`unification_helper`)~~ | ✅ **FIXÉ (2026-07-17)** : `unification_helper` retourne désormais `Option<Vec<(Type,Type)>>` (`None` = échec réel, `Some(vec![])` = succès sans nouveau binding) au lieu d'un `Vec` ambigu ; nouvelle fonction publique `try_unify` (le vieux `unify` devient un wrapper `unwrap_or_default()`) propagée jusqu'à `match_types_to_generic` (`mod.rs`), qui fait maintenant échouer tout le candidat dès qu'une sous-unification échoue réellement (au lieu de contribuer zéro binding silencieusement). Occurs-check ajouté dans le même passage (voir G3). Unions toujours non gérées explicitement (scope non étendu — seul le point signalé par l'audit, distinguer échec/succès, était visé) ; elles tombent maintenant sur un `None` correctement signifiant plutôt qu'un `vec![]` ambigu. | ~~**Haute**~~ |
| G3 | ~~`type_comparison.rs:47-56` (`reduce_alias`)~~ + occurs-check | ✅ **FIXÉ (2026-07-17)** : deux correctifs distincts. **Arité** : `reduce_type`/`reduce_alias` restent des fonctions pures `Type → Type` sans propagation d'erreur (changer leur signature aurait un rayon d'impact énorme, des centaines de call sites) — donc, suivant le patron déjà établi par `collect_undefined_aliases` (S4, appelé aux mêmes points d'entrée `let`/`fn`/`type`), l'arité est vérifiée **à côté** de `reduce_alias`, pas dedans : `let_expression.rs::collect_undefined_aliases` compare `params.len()` du site d'usage à `generics.len()` déclaré (via `get_matching_alias_signature`) et pousse `TypeError::AliasArityMismatch` sur mismatch (aliases opaques exclus — arité non trackée pour eux par ce mécanisme, cf. génériques phantom `Factor<L>`). **Occurs-check** : `unification_helper`'s cas `Generic` rejette maintenant un binding `T ↦ <type contenant T>` (ex. `T ↦ [T]`) via un nouveau helper `type_contains_generic`, au lieu d'écrire un binding auto-référentiel. Vérifié par tests unitaires (`let_expression::tests::test_option_*_arity_mismatch`, `unification::tests::test_occurs_check_*`) et pipeline réel : `typr check` sur `Option` nu ou `Option<int, char>` produit `Option expects 1 type argument, found 0/2` avec position exacte. | ~~**Haute**~~ |
| G4 | ~~`type_comparison.rs:116-126`~~ | ✅ **FIXÉ (2026-07-18)** : la réduction d'un `Union` recurse maintenant dans les deux branches (`reduce_type_helper`) avant de comparer, que la comparaison collapse ou non — un alias au fond d'un union non apparenté (`char \| Meters`) se résout désormais (`char \| int`) au lieu de rester `char \| Meters` tel quel. Test existant retourné (`test_reduce_union_unrelated_branches_left_unreduced` → `test_reduce_union_unrelated_branches_are_reduced_in_depth`, `type_comparison.rs`), assertion inversée pour refléter le comportement corrigé. | ~~Moyenne~~ |
| G5 | ~~`type_comparison.rs:98`~~ | ✅ **Investigué, pas un bug vivant (2026-07-18)** : `Type::If`/`Type::Condition` (syntaxe `T if <cond>`) n'a **aucun consommateur** ailleurs dans le crate — ni règle de sous-typage, ni unification, ni passe arithmétique n'inspecte jamais la liste de conditions ; seuls la construction (parsing) et ce `reduce_type_helper` existent. La fonctionnalité est parse-only/inerte, pas une dégradation silencieuse façon `Any` : il n'y a rien à « évaluer » contre. Lui donner un sens réel (quoi contraignent les conditions, vérifié où/quand) est une conception de fonctionnalité complète, hors scope de cet audit. Documenté dans `test_reduce_if_type_drops_conditions` (`type_comparison.rs`) plutôt que « corrigé ». | ~~Basse~~ |
| G6 | ~~`mod.rs:1948-1952, 2012-2016` (`UseModule`)~~ | ✅ **FIXÉ (2026-07-18) — confirmé, plus grave que « probablement »** : un test rouge a montré que ce n'était pas une dégradation silencieuse mais une **régression bloquante** : `use M::Box;` sur `type Box<T> <- list{value:T};` déclaré dans un module puis utilisé comme `Box<int>` échouait avec `Box expects 0 type argument, found 1` (le check d'arité de G3, ajouté en Phase 3, se retournait contre tout alias générique exporté par un module). Cause : le type exporté par `Lang::Module` ne porte que la cible déjà réduite, donc `UseModule` ré-enregistrait l'alias importé via le `push_alias(String, Type)` naïf, qui construit toujours un `Var` à 0 générique. Fix : nouveau helper `resolve_imported_alias` qui re-résout la signature (cible + liste de génériques) depuis le contexte interne du module déclarant, conservé via `Context::get_module_inner_context` (mécanisme déjà existant pour la transpilation/le cache incrémental) — au lieu de reconstruire depuis le type exporté. Bascule vers l'ancien comportement (0 génériques) si le contexte interne est indisponible, donc pas de régression sur le chemin existant. Vérifié par 2 tests (`edge_g6_generic_alias_arity_survives_module_import`, `edge_g6_imported_generic_alias_substitutes_field_type`, `mod.rs` — le second confirme que la substitution `T→int` a bien lieu, pas juste que l'arité passe) et en pipeline réel (`typr check` sur un vrai projet : `Box<int>` importé type-check ; sans le fix, échec identique confirmé). Note annexe (pré-existante, non liée) : `Box:{value=...}` (ConstructorCall) infère toujours l'alias nu sans arguments génériques attachés, avec ou sans module — bug distinct, croisé pendant la vérification ; **fixé en Phase 8**. | ~~Moyenne~~ |
| G7 | connu (mémoire) | Dispatch premier-paramètre sur `[T]`/array-of-interface cassé ; appel externe d'une fn à paramètre interface plante au runtime ; record anonyme `{x:int}` refusé comme type de param/retour. Reporté (chevauche déjà `project_interface_dispatch_hybrid`/`project_record_interface_intersection` en mémoire — traité séparément plutôt que dans cet audit). | — |

---

## 6. Assignation (`eval()` `Lang::Assign`, `mod.rs:370-407`)

| ID | Problème | Sévérité |
|---|---|---|
| A1 | ~~LHS non-`Var` (`p$x <- v`, `v[1] <- x`)~~ | ✅ **Pas un bug vivant (vérifié 2026-07-18)** : `Lang::Assign` n'est produit que par la syntaxe `expr!;` (mutation implicite, `implicit_mutate` dans `parsing/mod.rs`), jamais par un `<-` général — TypR n'a pas d'opérateur d'assignation générique. Le parser calcule `identifier` via `head_lang(&expr)`, qui n'accepte que `Lang::Variable` ou une chaîne `Dot`/`Pipe` se terminant par une `Variable` ; toute autre forme (dont `$` et l'indexation `[...]`, que l'audit citait) échoue **au parsing** avec `SyntaxError::MutationTargetNotAssignable`, avant même d'atteindre le type-checker. Le fallback silencieux de `eval()`'s `Lang::Assign` (si `Var::from_language` échoue) est donc du code mort défensif, pas un trou réel — confirmé par deux tests de régression `test_implicit_mutate_dollar_target_fails`/`test_implicit_mutate_array_indexing_target_fails` (`parsing/mod.rs`). | ~~**Haute**~~ |
| A2 | ~~Assignation à une variable jamais déclarée~~ | ✅ **Déjà couvert par S1 (vérifié 2026-07-18)** : `x!;` sur un `x` jamais déclaré type-checke `left_expr` (`Lang::Variable`) via le même chemin que toute lecture de variable — le check `UndefinedVariable` ajouté par S1 s'y applique donc déjà sans modification supplémentaire. Test de régression : `edge_a2_assign_to_undeclared_variable_is_an_error` (`mod.rs`). | ~~Moyenne~~ |
| A3 | ~~Aucune notion d'immutabilité vérifiée ici~~ | ✅ **Décidé (2026-07-18), pas de code à changer** : décision utilisateur = garder le comportement actuel — tout binding `let` reste implicitement mutable via `x!;`, sans marqueur `mut` à ajouter. `TypeError::ImmutableVariable` reste réservé aux conflits d'import (usage existant, non lié). Comportement volontaire, pas un bug. | ~~À décider~~ |

---

## 7. Couverture de test asymétrique

Tests inline par fichier (`#[test]`) : `mod.rs` 100, `function_application.rs` 55, `function.rs` 39, `type_arithmetic.rs` 20… mais **zéro** dans : `match_expression.rs`, `dollar_access.rs` (453 l.), `dot_pipe_access.rs`, `constructor_call.rs`, `type_comparison.rs` (le cœur de la réduction !), `field_access.rs`, `unification_map.rs`, `module_cache.rs`. Une partie est couverte indirectement depuis `mod.rs`, mais les chemins d'erreur de ces fichiers ne le sont quasiment pas.

---

## Plan de mitigation

**Avancement (2026-07-18) : Phase 1 (S1, S2, S3) faite. Phase 2 (M1-M5) faite. Phase 3 (G1, G2, G3) faite. Phase 4 (O1, O2, C1, C2) faite ; A1/A2 vérifiés non-bugs. Phase 5 (I1, A3, D1) faite. Phase 6 (harnais préventif) faite. Phase 7 (C3, C4, I2, G4, G5, G6) faite. Phase 8 (bug `ConstructorCall` générique découvert en vérifiant G6) faite.** Voir tests `edge_*` dans `type_checking::tests` (mod.rs) pour Phases 1/4/5/7, `edge_*` dans `match_expression::tests` pour Phase 2, les tests dans `unification::tests` / `unification_map::tests` / `let_expression::tests` pour Phase 3, `type_comparison::tests` pour Phase 6 + G4/G5 (Phase 7), `test_implicit_mutate_dollar_target_fails`/`test_implicit_mutate_array_indexing_target_fails` (`parsing/mod.rs`) pour A1, et `edge_p8_*` dans `constructor_call::tests` pour Phase 8. Détail dans les tables §1/§2/§3/§4/§5/§6 ci-dessus. Reste G7 (reporté, chevauche d'autres chantiers en mémoire).

### Phase 0 — Instrumentation avant correction (1 étape courte)
Confirmer chaque item par un test *rouge* avant de toucher au code : un fichier `lab/edge_cases/` + un test Rust `#[ignore]` par item (`edge_s1_undefined_variable`, `edge_m2_union_tag_binding`, …). Ça transforme cet audit en harnais exécutable et donne l'ordre de priorité réel (certains items sont peut-être déjà couverts ailleurs dans le dispatch).

> Fait pour S2/S3 : `edge_na_literal_types_as_na_not_any`, `edge_while_loop_condition_must_be_boolean`, `edge_while_loop_body_type_error_detected` (mod.rs, section « Edge-case audit » en fin de `mod tests`). Le reste des ~25 items n'a pas encore de test rouge.

### Phase 1 — Filet de sécurité systémique (S1-S3, le meilleur ratio effort/valeur) — ✅ FAITE
0. **S2/S3 faits** — voir §1. Les deux dispatchers (`typing()`, `eval()`) sont désormais des `match` exhaustifs sur les 62 variantes de `Lang` ; ajouter une variante à l'enum est maintenant une erreur de compilation tant que `typing()`/`eval()` n'ont pas explicitement décidé quoi en faire.
1. **S1 fait** : `Lang::Variable` indéfini → `TypeError::UndefinedVariable`. Le mécanisme redouté (faux positifs sur les builtins R non typés) ne s'est pas matérialisé : les placeholders `functions_R.txt` (`(Any, UnknownFunction)`) sont déjà présents dans `context.variables()`, donc `get_type_from_variable` réussit pour eux et le nouveau check ne se déclenche que pour les noms *vraiment* absents du contexte.
2. **S2/S3** : remplacer les catch-all par un match exhaustif (lister explicitement les variantes « neutres » restantes) ; le compilateur Rust force alors la décision à chaque nouvel ajout de variante `Lang` — c'est le garde-fou anti-régression le moins cher qui soit.
3. Politique de code (à ajouter dans CLAUDE.md) : **interdiction de retourner `any_type()` sans pousser une erreur**, sauf commentaire justifiant l'exception.

### Phase 2 — `match` (M1-M5) — ✅ FAITE (2026-07-17)
1. **M2/M5 faits** (bindings) : le scrutiné passe par `reduce_type` (`type_comparison::reduce_type`) avant dispatch ; `find_tag_variant`/`collect_tag_variants` marchent `Type::Operator(Union, …)` uniformément (users unions ET `Option<T>` via le même mécanisme `reduce_alias`, cas `"Option"` codé en dur supprimé) ; réduire le scrutiné en amont résout aussi l'alias record avant extraction des champs (M5, gratuit).
2. **M3 fait** : pattern non-`Variable` dans un tag/tuple/record pattern → `TypeError::UnsupportedPattern` au lieu d'un drop silencieux. (`.Some(.Some(x))` s'est avéré ne pas parser du tout aujourd'hui — non atteignable par le checker — mais le filet couvre les formes qui parsent bien.)
3. **M1 fait** : exhaustivité via `collect_tag_variants` (portage léger de la logique déjà écrite côté LSP `code_actions.rs::collect_tag_variants`, sans dépendance LSP) ; nouvelle `TypeError::NonExhaustiveMatch`. **Gardée inactive** sur un scrutiné `Type::Tag` isolé (voir note importante ci-dessous).
4. **M4 fait** : `TypeError::PatternTypeMismatch` si un pattern tag/tuple/record est incompatible avec la forme du scrutiné réduit — même garde d'activation que M1.
5. **Bonus (hors liste initiale)** : un pattern catch-all *nommé* (`other => ...`, pas seulement `_`) bind maintenant le type du scrutiné à ce nom — avant, ce nom restait non lié silencieusement.

**Découverte importante en écrivant les tests** : un `let x <- .Circle(3);` **non annoté** infère le type étroit `Type::Tag("Circle", ...)` pour la valeur littérale, **pas** l'alias union déclaré au complet (`Shape`/`Color`/...) — TypR ne l'élargit qu'avec une annotation explicite ou un paramètre de fonction typé. M1/M4 sont donc explicitement **désactivés** sur ce cas (`match_type` = un `Type::Tag` isolé, pas un `Operator(Union, …)` à 2+ variantes) : les vérifier dessus aurait donné des faux positifs sur l'usage TypR le plus courant. Les tests utilisent donc des **paramètres de fonction typés** (`fn(c: Color): ... { match c {...} }`), comme le fait déjà `Option<int>` en paramètre — pas des `let` non annotés.

**Piège méthodologique rencontré** : `parse2` ne parse qu'**une seule instruction top-level** (le reste est silencieusement tronqué) — un test qui concatène `"type X <- ...; fn(...) {...}"` en une seule string ne type-check en réalité que l'alias, et un test « pas d'erreur attendue » passe alors *dans le vide*. Les tests chaînent donc `parse2` + `typing_no_panic` séparément par instruction (`TypeChecker::typing_no_panic` réutilisé en fold), comme le fait déjà `test_function_param_called_in_match_branch` (function.rs).

Vérifié en pipeline réel : `typr build --release` sur un vrai projet TypR (union à 3 variantes + `Option<int>`), `typr run` produit les bonnes valeurs R, et un match non-exhaustif volontaire produit bien `This match doesn't cover every variant` avec position source précise dans la sortie CLI réelle. `cargo test --workspace` (642 tests) + `fmt --check` + `clippy` : zéro régression. Découverte annexe (non liée, non corrigée) : `plot.ty`/`system.ty` échouent déjà à `typr std` (`type: char` comme nom de champ → `Undefined variable 'type'`), pré-existant sur `main` avant cette session — probablement un effet de bord non catché de S1 (Phase 1). Signalé mais hors scope Phase 2.

### Phase 3 — Unification & génériques (G1-G3) — ✅ FAITE (2026-07-17)
1. **G2 fait** : `unification_helper` retourne `Option<Vec<(Type,Type)>>` (`None` = échec) ; nouvelle fonction publique `try_unify` (`unify` devient un wrapper `unwrap_or_default()`), propagée jusqu'à `match_types_to_generic` (`mod.rs`).
2. **G1 fait** — **deux sites**, pas un seul : le plan initial ne visait que `merge_substitutions` (unification d'un seul type composite), mais l'exemple concret de l'audit (`f(x:T,y:T)` appelé `f(1,"a")`) passe en réalité par un mécanisme différent : `Context::get_unification_map` unifie chaque argument **séparément** puis fusionne les bindings via `SafeHashMap::insert` (`unification_map.rs`) — c'est ce second site qui avalait silencieusement le conflit. Les deux retournent maintenant `bool`/échouent sur binding contradictoire ; `UnificationMap::new` → `try_new(..) -> Option<Self>`.
3. **G3 fait** — deux correctifs distincts, tous deux **à côté** de `reduce_alias`/`unification_helper` plutôt que dans leur signature (`reduce_type` reste une fonction pure `Type → Type`, la changer aurait un rayon d'impact énorme) : check d'arité dans `let_expression.rs::collect_undefined_aliases` (même point d'entrée que S4) + `TypeError::AliasArityMismatch` ; occurs-check dans le cas `Generic` de `unification_helper` via le nouveau helper `type_contains_generic`.

Vérifié en pipeline réel (`typr check`) : `same(1,"a")` (générique répété `T,T`) → rejeté (`Function same<int> not defined in this scope`), `same(1,2)` → toujours accepté ; `Option` nu et `Option<int,char>` → `Option expects 1 type argument, found 0`/`2` avec position exacte ; `Option<int>` → accepté. `cargo test --workspace` (553 tests, +11 vs Phase 2) + `fmt --check` + `clippy` : zéro régression. Note méthodologique : deux bugs de transpilation R **pré-existants et non liés** ont été croisés en testant (`as.FunctionN`/`as.Option` introuvables à l'exécution R pour tout appel de fonction générique ou tout usage de `Option`) — confirmés présents à l'identique sur `main` (avant cette session, via `git stash`), donc hors scope Phase 3 ; signalés ici pour visibilité future. Toutes les vérifications ci-dessus ont donc été faites au niveau `typr check` (parsing + type-checking), pas `typr run`.

### Phase 4 — Opérateurs, contrôle, assignation (O1-O2, C1-C2, A1-A2) — ✅ FAITE (2026-07-18)
1. **O1/O2 faits** : les 5 opérateurs de comparaison (`==`,`<=`,`>=`,`>`,`<`) réduisent les deux
   opérandes puis acceptent l'égalité réduite, un mélange `int`/`num`, ou l'un sous-type de l'autre
   (bidirectionnel) — plus seulement `Type::PartialEq` brut. `&&`/`||`, `Lang::Not` (`!`), la
   condition du `if` et (bonus, même défaut) celle du `while` réduisent maintenant le type avant
   `is_boolean()`, acceptant un alias `type Flag <- bool;`.
2. **C1 fait** : nouveau `Context::expected_return_type` (posé par `function()` sur le sub-contexte
   avant de typer le corps, comme `self_type`) ; `Lang::Return` dans `typing()` compare la valeur
   retournée contre ce type via `function::is_compatible_return_type` — fonction publique extraite
   de la logique déjà utilisée pour la dernière expression du corps (même 3 clauses : opacité,
   `reduce_and_subtype`, générique rigide contraint par interface), garantissant qu'un `return`
   précoce et le retour implicite final sont jugés à la même barre. Erreur réutilisée :
   `TypeError::UnmatchingReturnType` (déjà existante, via `builder::unmatching_return_type`).
3. **C2 fait** : nouveau flag `Context.config.in_loop` (même patron que `in_module_body`), posé à
   `true` sur le sous-contexte utilisé pour typer le corps de `Lang::Loop`/`WhileLoop`/`ForLoop` ;
   `Lang::Break`/`Lang::Next` vérifient `context.is_in_loop()` et poussent la nouvelle erreur
   `TypeError::LoopControlOutsideLoop("break"|"next", position)` sinon.
4. **A1/A2 vérifiés, pas de code à changer** : `Lang::Assign` n'existe que via la syntaxe de
   mutation implicite `expr!;` (jamais un `<-` général) et son `identifier` est garanti être un
   `Lang::Variable` par construction côté parser (`head_lang`, qui rejette `$`/indexation à la
   *syntaxe* avec `SyntaxError::MutationTargetNotAssignable`) — A1 n'est donc pas atteignable. A2
   (`x!;` sur `x` jamais déclaré) type-checke la cible via le même chemin `Lang::Variable` que toute
   lecture, donc le check `UndefinedVariable` de S1 s'y applique déjà sans changement.
5. **C3** : toujours ouvert, non touché par cette phase — cf. mémoire
   `bug_lines_single_statement_context_drop`.

Vérifié en pipeline réel (`typr build`+`typr run` sur un vrai projet, cf. `feedback_verify_via_real_build_pipeline`) et `cargo test --workspace` (567 tests typr-core, +14 vs Phase 3) + `fmt --check` + `clippy` (aucune régression, warnings pré-existants uniquement sur du code non touché) : zéro régression.

### Phase 5 — Sémantique à trancher (décisions de design, pas des bugs) — ✅ FAITE (2026-07-18)
- **I1** : pas une décision à prendre — déjà tranché et implémenté de longue date (commit `4bce238`,
  bien avant cet audit) : sémantique TypR (« depuis la fin »), avec codegen R dédié cohérent des deux
  côtés (type-checker + transpiler). L'audit était simplement obsolète sur ce point ; corrigé ici.
- **A3** : décision utilisateur = garder le statu quo (tout `let` mutable via `x!;`, pas de `let mut`).
  Documenté comme comportement intentionnel, aucun code changé.
- **D1** : décision utilisateur = ajouter les deux vérifications manquantes. `TypeError::DataFrameColumnNotVector`
  + `TypeError::DataFrameColumnLengthMismatch`, implémentées dans l'arm `Lang::DataFrame` de `typing()`
  (`mod.rs`), avec 3 tests de régression (`edge_d1_*`, `mod.rs`). `cargo test --workspace` (570 tests
  typr-core, +3 vs Phase 4) + `fmt --check` + `clippy` : zéro régression. Vérifié en pipeline réel
  (`typr debug --r`) pour les 3 cas (colonnes égales/OK, longueurs différentes/erreur, colonne scalaire/erreur) ;
  le runtime R de `data.frame(...)` a un gap pré-existant et non lié (`as.data.frame.default` ne convertit
  pas la classe interne `typed_vec`), confirmé identique sur `main` avant ce fix, hors scope ici.

### Phase 6 — Harnais préventif (contre les edge cases *futurs*) — ✅ FAITE (2026-07-18)
1. **Matrice constructions × contextes fait** : `crates/typr-core/tests/edge_case_matrix.rs`
   (nouveau fichier, harnais transverse — pas de logique de dispatch `Lang` à un seul fichier
   comme le reste de `type_checking/`, donc placé en test d'intégration top-level à côté de
   `snapshot_tests.rs` plutôt que dans `mod.rs`). 4 constructions (tag literal, match, lambda,
   validating cast) × 6 contextes (argument direct, `let` puis usage, branche de match, valeur de
   champ de record, membre public de module, corps de fonction) = 24 cellules. Chaque cellule
   fait transiter la construction par une fonction identité générique `id: (x: T): T { x }` — un
   `Type::Any` en sortie révèle directement une dégradation silencieuse (au lieu de mesurer le
   type du *wrapper*). Le contexte « corps de fonction » ne peut pas réutiliser cette astuce (le
   type d'un appel vient de la signature déclarée, pas d'une réinférence du corps) donc il est
   couvert à part, une déclaration par construction. **A trouvé un vrai bug** (pas juste vérifié
   l'absence de bug) : voir point 5 ci-dessous.
2. **Snapshots `insta` faits** : nouveau module `diagnostics` dans `crates/typr-core/tests/snapshot_tests.rs`,
   9 snapshots figeant le texte exact (`TypRError::simple_message()`, pas le rendu miette/ANSI) de
   S1 (`UndefinedVariable`), M1 (`NonExhaustiveMatch`), M4 (`PatternTypeMismatch`), C1
   (`UnmatchingReturnType`), C2 (`LoopControlOutsideLoop`), D1×2 (`DataFrameColumnNotVector`/
   `DataFrameColumnLengthMismatch`), G3 (`AliasArityMismatch`). M3 (`UnsupportedPattern`) n'a pas
   de repro textuel propre (la grammaire de pattern tag n'accepte qu'un identifiant simple entre
   parenthèses, donc un pattern imbriqué ne parse pas du tout — même raison que `.Some(.Some(x))`
   ne parse pas, cf. M3 plus haut) : snapshot sur la valeur d'erreur construite directement à la
   place. **A aussi trouvé un vrai bug** : voir point 5.
3. **Guard CI fait** : 3 tests dans `type_checking::tests` (mod.rs) qui lisent le propre source du
   fichier via `include_str!` (coupé juste avant `#[cfg(test)]` pour ne pas se contaminer avec ses
   propres littéraux de chaîne) — `guard_any_type_call_count_is_tracked` (baseline 16, non-
   décroissant), `guard_no_new_wildcard_any_type_fallback` (baseline 1, détecte un nouveau
   `_ => any_type()`/`_ => unknown_function_type()` réintroduit dans `eval()`/`typing()`),
   `guard_central_dispatchers_stay_exhaustive_matches` (les commentaires-ancres de Phase 1
   doivent rester présents). Vérifié que le guard mord vraiment : injection temporaire d'un faux
   `_ => any_type()` dans `eval()` → `guard_no_new_wildcard_any_type_fallback` échoue comme prévu,
   fichier restauré ensuite.
4. **`type_comparison.rs` comblé** : 17 tests unitaires couvrant chaque branche de
   `reduce_type_helper` (Record, Alias résolu/opaque/mémorisé/non-résolu, Tag, If, Function, Vec,
   Union collapse/non-collapse, opérateurs arithmétiques, Intersection, Access, fallback) — ce
   fichier avait 0 test alors que `reduce_type` est la fonction la plus appelée du checker. Le test
   `test_reduce_union_unrelated_branches_left_unreduced` documente explicitement le gap G4 connu
   (union non réduite en profondeur quand aucune branche n'est sous-type de l'autre) au lieu de le
   cacher.
5. **Deux vrais bugs trouvés et corrigés en construisant le harnais** (pas seulement catalogués —
   exactement le scénario que ce Phase 6 anticipait) :
   - **`Lang::Lines` retypait sa dernière instruction deux fois** pour tout corps de 3+
     instructions (`mod.rs`, arm `Lang::Lines`, branche `else`) : le `fold` qui construit le
     contexte itérait sur `exprs` (la liste complète, y compris la dernière instruction) au lieu de
     `exprs2` (la liste sans la dernière, déjà calculée juste au-dessus via `.pop()`) — la dernière
     instruction était donc typée une fois dans le fold *et* une fois via `final_tc`, dupliquant
     verbatim chacune de ses erreurs. Trouvé via les nouveaux snapshots de diagnostics (M1/M4
     sortaient deux fois le même message pour un programme à 2 instructions). Fix : une ligne
     (`exprs.iter()` → `exprs2.iter()`). Sans impact sur C3 (`Lines` à exactement 1 instruction,
     bug distinct toujours ouvert).
   - **`x as! int`/`num`/`char`/`bool` plantait (panic Rust, pas juste une erreur de compilation)**
     dès que la valeur castée atterrissait dans un contexte qui dérive un `HelpData` depuis son
     `Type` (ex. valeur de champ de record littéral : `:{ v = 5 as! int }`) : le parseur
     (`components/language/operators.rs::combine`) traite le membre droit de `as!` comme un
     `Lang::Variable` ordinaire quand ce n'est pas une forme structurelle connue (tableau/vec/
     record), donc `int`/`num`/`char`/`bool` — des mots-clés de type primitif, pas des alias
     résolvables — produisaient `Type::Alias("int", ...)` côté type-checker (`ValidatingCast`,
     branche `None`). Ce nom ne correspond à aucun des noms longs capitalisés que le fallback de
     `reduce_type_helper` connaît (`"Integer"`, pas `"int"`), donc même sans le panic le cast
     aurait fini en `Any` silencieux. Fix : nouveau helper `primitive_cast_type()` dans
     `operators.rs`, appelé dans `combine()` avant de retomber sur le chemin `Lang::Variable`
     générique — construit directement le `Type` primitif pour ces 4 mots-clés, symétrique à ce
     que fait déjà `lang_to_cast_type()` pour les casts vers un tableau/vec/record structurel.
     Trouvé par `matrix_construction_as_record_field_value` (le seul contexte de la matrice qui
     fait vraiment passer une valeur par une `ArgumentType` de record).

Vérifié en pipeline réel (`typr debug --r` sur le repro exact du bug `as!`, avant/après fix) et
`cargo test --workspace` (590 tests typr-core lib, +6 `edge_case_matrix`, +9 `diagnostics`
snapshots, +3 guards, +17 `type_comparison` — zéro régression sur les 587+40+56 tests
pré-existants) + `fmt --check` + `clippy` (mêmes warnings pré-existants uniquement, rien de
nouveau).

### Phase 7 — Items restants après Phase 6 (C3, C4, I2, G4, G5, G6) — ✅ FAITE (2026-07-18)

Périmètre choisi par l'utilisateur pour cette phase : C3+C4 (fix complet), I2 (fix complet),
G4+G5 (fix complet), G6 (investiguer d'abord, décider ensuite). G7 explicitement laissé de côté
(chevauche déjà `project_interface_dispatch_hybrid`/`project_record_interface_intersection` en
mémoire — mieux traité comme son propre chantier).

1. **C3 fait — pas un bug vivant** : reproduit directement le scénario exact décrit par la mémoire
   `bug_lines_single_statement_context_drop` (`typing_with_errors` sur un `Lang::Lines` à 1
   élément, et `FluentParser.push(...).run()`) ; dans les deux cas le binding survit. Le code
   actuel du fast-path utilise déjà `tc.context` (le contexte *retourné*), pas le contexte
   d'entrée que la mémoire décrivait. Mémoire et audit corrigés plutôt que du code changé.
2. **C4 fait** : `if`/`else` compare maintenant les branches réduites (`reduce_type`) et collapse
   sur égalité réduite ou sous-typage bidirectionnel — même patron que O1 (Phase 4). Élimine le
   faux `Point | Record` pour deux branches structurellement identiques.
3. **I2 fait — plus grave que documenté** : ce n'était pas juste « pas de check », c'était un
   **panic Rust** sur tout index dynamique (`v[i]`) — `Lang::len()` ne gère que
   `Integer`/`Array`/`Vector` et panique sur le reste (`Lang::Variable`, `FunctionApp`, …), et
   c'est exactement la forme qu'un index non littéral prend. Fix : un membre d'index qui n'est
   pas une des 3 formes littérales est type-checké et doit réduire vers `Type::Integer` (sinon
   `TypeError::WrongIndexing`), et dégrade vers `Tint::Unknown` pour la sonde de taille statique
   plutôt que de paniquer.
4. **G4 fait** : `reduce_type_helper` recurse maintenant dans les deux branches d'un `Union` avant
   de comparer/reconstruire, que la comparaison collapse ou non — un alias caché dans une branche
   non apparentée à l'autre se résout désormais.
5. **G5 investigué, pas un bug vivant** : `Type::If`/`Type::Condition` (syntaxe `T if <cond>`)
   n'a aucun consommateur ailleurs dans le crate (grep confirmé) — ni sous-typage, ni
   unification, ni passe arithmétique n'inspecte jamais la liste de conditions. La fonctionnalité
   est parse-only/inerte ; « jeter les conditions » à la réduction n'est donc pas une dégradation
   silencieuse comparable aux autres items de cet audit. Lui donner un sens réel est une
   conception de fonctionnalité à part entière, hors scope ici.
6. **G6 fait — confirmé, plus grave que « probablement »** : un test rouge a montré une
   régression bloquante, pas une dégradation silencieuse : un alias générique déclaré dans un
   module (`type Box<T> <- list{value:T};`) et importé via `use M::Box;` échouait systématiquement
   sur `Box<int>` avec `Box expects 0 type argument, found 1` — le check d'arité ajouté en Phase 3
   (G3) se retournait contre l'usage le plus courant d'un alias générique modulaire. Fix : nouveau
   helper `resolve_imported_alias` (`mod.rs`) qui re-résout la signature généricisée de l'alias
   depuis le contexte interne du module déclarant (`Context::get_module_inner_context`, mécanisme
   déjà existant pour la transpilation/le cache incrémental) au lieu de reconstruire depuis le
   type déjà réduit exporté par `Type::Module`. Dégrade vers l'ancien comportement (0 génériques)
   si le contexte interne est indisponible — aucune régression sur le chemin existant.

Vérifié en pipeline réel pour chaque item (`typr debug --r`, `typr check` sur de vrais projets
`typr new`+édition) et `cargo test --workspace` (596 tests typr-core lib, +8 vs Phase 6 : 2×C3/C4,
2×I2, 2×G6, renommage G4, commentaire enrichi G5 — zéro régression sur les 590 pré-existants) +
`fmt --check` + `clippy` (aucun nouveau warning ; un `if_same_then_else` introduit par le fix C4
a été simplifié avant de committer). Note annexe (pré-existante, non liée, croisée en vérifiant
G6) : `TypeName:{...}` (`ConstructorCall`) infère toujours l'alias generic nu sans les arguments
de type attachés (`Box:{value=5}` type en `Box`, pas `Box<int>`), avec ou sans module — confirmé
identique sur un alias top-level sans aucun module impliqué, donc bug distinct et non corrigé ici.

### Phase 8 — Bug `ConstructorCall` générique découvert en vérifiant G6 — ✅ FAITE (2026-07-18)

Périmètre choisi par l'utilisateur : uniquement le bug d'inférence générique croisé pendant la
vérification de G6 (`Box:{value=5}` type en `Box` nu au lieu de `Box<int>`), pas G7 (laissé
explicitement de côté, cf. Phase 7 — chevauche `project_interface_dispatch_hybrid`/
`project_record_interface_intersection` en mémoire).

1. **Root cause** : `constructor_call()` (`processes/type_checking/constructor_call.rs`)
   construisait toujours `target_type = Type::Alias(type_name, vec![], false, h)` — zéro argument
   générique, quel que soit le type inféré des champs fournis. Ce n'était pas qu'un problème
   d'affichage : `Box:{value=5}` (donc `Box<int>`) et `Box:{value='a'}` (donc `Box<char>`)
   produisaient exactement le même type `Box`, ce qui se répercutait ensuite dans toute
   comparaison de sous-typage en aval (paramètre de fonction, `let` annoté, …) — un vrai trou de
   sûreté de type, pas juste un type moins précis qu'espéré.
2. **Fix** : `constructor_call()` récupère maintenant `(target_brut, generics)` via
   `Context::get_matching_alias_signature` (le même accesseur que G3/G6 pour l'arité — cas
   `module_path` vide seulement ; le cas qualifié par module garde l'ancien comportement à 0
   générique, `resolve_module_member_type` ne renvoyant que le type déjà exporté/réduit, pas la
   paire `(target, generics)`). Chaque champ explicite typé (et chaque champ couvert par un
   `...spread`) alimente une liste `(type de la valeur, type de champ déclaré — encore générique,
   ex. `value: T`)`, unifiée à la fin via `Context::get_unification_map` +
   `UnificationMap::apply_unification_type` — exactement le mécanisme que
   `FunctionType::infer_return_type` utilise déjà pour le type de retour d'un appel de fonction
   générique. Le résultat substitue chaque générique déclaré de l'alias pour construire
   `Type::Alias(type_name, concret_types, false, h)`. Dégradation : un générique qu'aucun champ ne
   mentionne, ou une unification en conflit, laisse ce générique tel quel plutôt que de rejeter la
   construction — pas de nouveau faux positif introduit.
3. **Tests** (`constructor_call::tests`, 3 nouveaux) : `edge_p8_constructor_call_infers_generic_type_argument`
   (le type inféré est bien `Box<int>`, pas `Box`), `edge_p8_inferred_generic_argument_is_checked_at_call_sites`
   (un `Box:{value=5}` passé à un paramètre `Box<char>` est maintenant rejeté — c'est le vrai bug de
   sûreté, silencieusement accepté avant ce fix), `edge_p8_inferred_generic_argument_matches_compatible_call_site`
   (le même littéral contre un paramètre `Box<int>` type-check toujours sans erreur — pas de faux
   positif introduit).

Vérifié en pipeline réel (`typr check`/`typr run` sur un vrai projet `typr new`) : `Box:{value=5}`
passé à un paramètre `Box<char>` produit désormais `Function get_char<Box<int(5)>> not defined in
this scope` (rejeté) ; le même littéral contre `Box<int>` compile et `typr run` affiche `5`.
`cargo test --workspace` (599 tests typr-core lib, +3 vs Phase 7 — zéro régression sur les 596
pré-existants, `RUST_MIN_STACK` requis pour un test transpiling préexistant sans lien) +
`fmt --check` + `clippy` (aucun nouveau warning).

### Ordre recommandé
`P0 → P1 (S2/S3 puis S1, FAIT) → P2 (M2/M3 puis M1, FAIT) → P3 (G2 puis G1 puis G3, FAIT) → P4 (FAIT) → P5 (FAIT) → P6 (FAIT) → P7 (FAIT) → P8 (FAIT)`.
P1.2 (match exhaustif sur `Lang`) est le seul item qui protège *structurellement* contre les edge
cases pas encore inventés — c'est celui qui a été fait en premier ; P6 (le harnais génératif +
guards CI) est le second, et a déjà payé pour lui-même en trouvant 2 bugs réels pendant sa
construction. Reste hors-scope : G7 (dispatch premier-paramètre `[T]`/interface, record anonyme
comme type de param/retour — chevauche d'autres chantiers en mémoire).

---

## 8. Nouveaux edge cases trouvés via `syntaxe.md` (2026-07-18, pas encore corrigés)

Méthode : relecture de `syntaxe.md` (carte de la syntaxe, établie par lecture du parseur) section
par section, en cherchant les constructions dont le *type-checking* pourrait ne pas suivre ce que
la doc affirme comme légal — puis confirmation de chaque suspicion par un repro `typr debug`
et, pour le plus grave, par un aller-retour complet `typr new`+`build`+`run` (vraie exécution R,
suivant `feedback_verify_via_real_build_pipeline`). Contrairement aux sections 1-7 (issues d'une
lecture de code), celle-ci part de la doc utilisateur — donc plus proche de ce qu'un vrai
programme TypR écrirait naturellement.

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| U1 | `type_checking/mod.rs:2321-2329` (`Lang::UnionConstructor`) + `transpiling/mod.rs:1890-1903` | **`TagName:{ field = val }` / `Union.Variant:{ field = val }` (syntaxe qualifiée documentée en §7 de `syntaxe.md`) ne valide RIEN et casse systématiquement à l'exécution R.** Le bras de typage ignore complètement `variant_name` et `fields` — il ne vérifie même pas que `variant_name` est une variante réelle de `union_name` — et retourne toujours `Type::Alias(union_name, ...)`. Côté transpile, le nom de champ choisi par l'utilisateur (`r` dans `Shape.Circle:{ r = 3.0 }`) est émis tel quel comme argument nommé R (`Circle(r = 3.0)`), alors que le constructeur généré pour un tag (positionnel ou à corps record, aucune différence) n'a toujours qu'un seul paramètre positionnel appelé `x` (`Circle <- function(x) { list("Circle", body = x) }`). Confirmé par un aller-retour réel (`typr new`+`build`+`run`) : `Erreur dans Circle(r = as.Number(3)) : argument inutilisé (r = as.Number(3))` — compile sans le moindre diagnostic, crashe à coup sûr au runtime R. Reproduit identique que le tag soit scalaire (`.Circle(num)`) ou à corps record (`.Circle(list{r:num})`), qualifié (`Shape.Circle:{...}`) ou non (`Circle:{...}`). | **Haute** |
| I3 | `type_checking/mod.rs:1702-1753` (`Lang::ArrayIndexing`, branche générale) via `Type::to_array2` (`components/type/mod.rs:927-944`) | **Indexer un `Array`/`Vec` avec un index scalaire (`a[1]`) type systématiquement en `[1, int]`, quel que soit le vrai type d'élément.** `to_array2`'s cas de base (`args.len() == 1`) construit toujours le type élément avec `builder::integer_type_default()` codé en dur, au lieu de lire l'élément réel de `typ1` (le type de base indexé) — contrairement à la branche `v[-n]` juste au-dessus (ligne ~1694) qui, elle, extrait correctement `Type::Vec(_, _, t2, _) => *t2`. Conséquence concrète : `a[1]` sur `Array[3, char]`/`Array[N, MonRecord]`/tout élément non-`int` type toujours `[1, int]`, ce qui casse silencieusement toute utilisation en aval qui n'est pas immédiatement suivie d'une annotation de type coercitive — passer `a[1]` en argument d'une fonction typée produit un faux `Function f<[1, int]> not defined in this scope` sur du code par ailleurs parfaitement correct (vérifié : `f(a[1])` avec `f: (char) -> char` et `a: Array[3, char]` rejeté). Même pour un élément `int`, ça casse dès que le contexte consommateur n'est pas un `let` annoté qui coerce implicitement. Zéro test existant ne couvre le *type* d'un accès scalaire sur `Array`/`Vec` (seul `test_tuple_scalar_indexing` existe, et les tests I2 ne vérifient que l'absence de panic/l'erreur sur index non-`int`, pas le type retourné pour un index valide) — c'est le trou de couverture le plus net trouvé ici. | **Haute** |
| T1 | `parsing/mod.rs` (désucrage de `let :{a, b, ...} <- tuple_expr;`, cf. §2 `syntaxe.md`) + le check d'arité manquant côté `type_checking` | **Déstructuration de tuple sans vérification d'arité, dans les deux sens, avec deux symptômes différents.** *Sous-binding* (`let :{a, b} <- :{1, 2, 3};`, 2 variables pour 3 éléments) : accepté sans le moindre diagnostic, le 3e élément est silencieusement perdu (ni erreur, ni warning — juste deux `let` générés sur trois positions possibles). *Sur-binding* (`let :{a, b, c, d} <- :{1, 2, 3};`, 4 variables pour 3 éléments) : produit un message incompréhensible et mal localisé, `Unknown element `` in `std.ty` at `1`` (pointe vers `std.ty`, pas vers le fichier utilisateur), au lieu d'un diagnostic clair du type « le tuple a 3 éléments, 4 attendus » — le désucrage émet quand même un accès `.tuple_tmp.[[4L]]` hors bornes dans le R généré. Les deux cas devraient être détectés au désucrage (comparer le nombre de variables au nombre d'éléments de l'expression tuple source, quand elle est syntaxiquement un littéral tuple) plutôt que de laisser le sous-binding filer et le sur-binding retomber sur le chemin d'erreur générique et mal étiqueté de l'indexation de tuple hors bornes. | Moyenne |

**Note méthodologique** : U1 est probablement le plus significatif des trois — c'est une syntaxe
explicitement documentée dans `syntaxe.md` (« alternative à `.Circle(...)` ») qui ne fonctionne dans
*aucun* cas testé, pas un edge case marginal. Il est probable que personne n'ait jamais exercé
`Union.Variant:{...}`/`TagName:{...}` en dehors de la doc elle-même, sans quoi ce crash — immédiat,
déterministe, sur le premier essai — aurait été trouvé bien plus tôt.

### Phase 9 — À planifier (U1, I3, T1)

Pas encore commencée : ces trois items ont été confirmés (repro + inspection du code source pour
localiser la cause), mais aucun correctif n'a été écrit. Pistes de correctif esquissées pendant
l'investigation, à valider avant de coder :
- **U1** : soit rejeter `TagName:{...}`/`Union.Variant:{...}` à la syntaxe pour tout tag qui n'a pas
  un corps record réel (n'autoriser la forme `:{...}` que quand la variante encapsule elle-même un
  `list{...}`, où le nom de champ a un sens), soit — si l'intention est de la garder générale —
  générer un constructeur de tag qui accepte réellement un paramètre nommé d'après le champ déclaré.
  Nécessite de relire `interface_constructeurs.md`/le code de génération de constructeur de tag
  (§ Union variant pipeline en mémoire) pour ne pas casser les tags à corps record qui, eux,
  fonctionnent peut-être déjà correctement via `.Circle(record_expr)`.
- **I3** : faire lire à la branche générale de `Lang::ArrayIndexing` le vrai type élément de `typ1`
  (via `typ1.to_array()`/le motif déjà utilisé par la branche `v[-n]`) plutôt que de reconstruire
  aveuglément via `Type::to_array2(args_index)` — cette dernière fonction semble conçue pour les cas
  multi-dimensionnels où on *sous-array* (`m[1, ]`), pas pour l'accès scalaire complet ; il faut
  distinguer "l'index couvre toutes les dimensions → retourner l'élément" de "l'index n'en couvre
  qu'une partie → retourner un sous-tableau", ce que le code actuel ne fait pas du tout.
- **T1** : ajouter un check d'arité au désucrage de `let :{...} <- expr;` quand `expr` a une forme
  tuple statiquement connue (littéral tuple ou type tuple déjà inféré) — nouvelle erreur dédiée
  plutôt que de laisser retomber sur le chemin d'indexation générique.
