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
| M1 | `match_expression()` | **Aucune vérification d'exhaustivité.** Un `match` sur un union à 3 variantes n'en couvrant que 2 passe ; le type de sortie est l'union des branches présentes. (Le LSP a un quickfix « missing match arms » — la logique de calcul des variantes existe déjà côté `typr-lsp`, elle n'est pas branchée dans le checker.) | **Haute** |
| M2 | `build_match_branch_context:25-35` | Binding d'un pattern tag : le type interne n'est résolu que si le scrutiné est **exactement** `Type::Tag` du même nom, ou si l'alias s'appelle littéralement **`"Option"`** (codé en dur). Pour tout union utilisateur (`match s: Shape { .Circle(r) => ... }`), `r` est typé `Any` → le corps de la branche n'est pas vérifié. Le scrutiné n'est jamais passé par `reduce_type`. | **Haute** |
| M3 | `build_match_branch_context:104` (`_ =>`) | Patterns imbriqués (`.Some(.Some(x))`, `.Point(:{x, y})`) → bindings silencieusement **droppés** ; les variables du pattern sont ensuite indéfinies → `Any` (cf. S1) → aucun diagnostic. | **Haute** |
| M4 | `match_expression()` | Aucune compatibilité pattern↔scrutiné : `match 5 { .Circle(r) => ... }` ne produit aucune erreur (branche morte typée quand même). | Moyenne |
| M5 | `build_match_branch_context:80-83` | Pattern record : le scrutiné `Type::Alias("Point")` n'est pas réduit → `record_fields = None` → tous les champs bindés `Any`. | Moyenne |
| M6 | — | Pas de détection de branches dupliquées / inaccessibles. | Basse |

---

## 3. Opérateurs & flux de contrôle (`typing()` inline)

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| O1 | `mod.rs:1183` | Comparaisons (`==`, `<`, `<=`, …) : `tc1.value == tc2.value` — **égalité stricte de `Type`, sans `reduce_type`**. Un alias comparé à son type sous-jacent (`let m: Meters <- 5; m == 5`) et probablement `int` vs `num` (`1 < 1.5`) donnent un faux positif `WrongExpression`. Rappel : `Type::Eq` est volontairement lossy (mémo « cache reduce_type abandonné »), donc le comportement exact est à cartographier par test. | **Haute** |
| O2 | `mod.rs:1137`, `mod.rs:1729` | `&&`/`||` et `!` : `is_boolean()` sur le type **non réduit** → un alias vers `bool` (`type Flag <- bool`) est rejeté. Idem pour la condition du `if` (`mod.rs:1304`). | Moyenne |
| C1 | `mod.rs:1766` | `Lang::Return { value } => typing(context, value)` : un `return` précoce n'est **jamais unifié avec le type de retour déclaré**. Dans `Lines`, seul le type de la dernière expression compte : `fn(x: int): int { if (x > 0) { return "a" }; 1 }` risque de passer. | **Haute** |
| C2 | `mod.rs:1111-1112` | `break`/`next` typés `Empty` partout — acceptés hors de toute boucle, R plantera au runtime. | Basse |
| C3 | `mod.rs:1243-1254` | **Bug connu non corrigé** : `Lang::Lines` avec exactement 1 statement perd les bindings de ce statement (fast-path qui pousse `_out` mais repart du contexte interne différemment du chemin multi-statements). Cf. mémoire `bug_lines_single_statement_context_drop`. | Moyenne |
| C4 | `mod.rs:1310-1314` | `if/else` : union des branches faite sur types non réduits — `if (c) { p } else { q }` avec `p: Point` et `q: list{x:int,y:int}` structurellement identiques donne `Point \| Record` au lieu de collapser. | Basse |

---

## 4. Indexation, vecteurs, data frames

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| I1 | `mod.rs:1513-1536` | `v[-1]` typé « élément compté depuis la fin » (retourne le type élément). En R, l'index négatif est une **exclusion** (`v[-1]` = tout sauf le premier, retourne un vecteur). Si la transpilation émet `v[-1]` tel quel, le type statique et la valeur runtime divergent. À trancher : sémantique TypR + codegen dédié, ou alignement sur R. | **Haute** (si transpilé tel quel) |
| I2 | `mod.rs:1540-1567` | Index non-littéral : aucun check que l'index est `int` ; le chemin `linearize`/`args_index` construit les tailles depuis `x.len()` de l'AST, comportement opaque pour les index dynamiques. | Moyenne |
| D1 | `mod.rs:1431-1468` (`DataFrame`) | Longueurs de colonnes jamais comparées entre elles ; l'index du DF est dérivé **de la première colonne uniquement** ; colonne non-vecteur → index `Any` sans erreur. | Moyenne |

---

## 5. Génériques, alias, unification

| ID | Localisation | Problème | Sévérité |
|---|---|---|---|
| G1 | `unification.rs:397-409` (`merge_substitutions`) | **Bindings contradictoires écrasés silencieusement** : si l'unification produit `T ↦ int` puis `T ↦ char`, le second remplace le premier (`existing[pos] = (name, resolved_type)`) sans erreur. `f(x: T, y: T)` appelé avec `f(1, "a")` peut passer selon le chemin de dispatch. | **Haute** |
| G2 | `unification.rs:236-352` (`unification_helper`) | **Échec indistinguable du succès** : « types non unifiables » retourne `vec![]`, exactement comme « unifiés sans substitution ». Les appelants ne peuvent pas rejeter un appel sur échec d'unification pure. Les unions ne sont pas gérées (tombent dans `_ => vec![]`). Pas d'occurs-check (`T` peut se lier à `[T]`). | **Haute** |
| G3 | `type_comparison.rs:47-56` (`reduce_alias`) | **Arité des génériques d'alias non vérifiée** : `generics.zip(concret_types)` tronque silencieusement. `Option<int, char>` ou `Option` nu passent ; les génériques non substitués restent des `Generic` fantômes. | **Haute** |
| G4 | `type_comparison.rs:116-126` | Réduction d'un `Union` : si aucune branche n'est sous-type de l'autre, retour du type **tel quel sans réduire les branches** — un alias au fond d'un union ne se résout jamais par ce chemin. | Moyenne |
| G5 | `type_comparison.rs:98` | `Type::If(typ, conditions)` réduit en jetant les conditions sans les évaluer. | Basse |
| G6 | `mod.rs:1948-1952, 2012-2016` (`UseModule`) | Les membres non-fonction importés sont poussés comme alias avec leur type **déjà réduit** — les paramètres génériques d'un alias générique exporté par un module sont probablement perdus à l'import. À confirmer par test. | Moyenne |
| G7 | connu (mémoire) | Dispatch premier-paramètre sur `[T]`/array-of-interface cassé ; appel externe d'une fn à paramètre interface plante au runtime ; record anonyme `{x:int}` refusé comme type de param/retour. | — |

---

## 6. Assignation (`eval()` `Lang::Assign`, `mod.rs:370-407`)

| ID | Problème | Sévérité |
|---|---|---|
| A1 | LHS non-`Var` (`p$x <- v`, `v[1] <- x`) : le check de sous-typage passe, puis `Var::from_language` échoue → retour **silencieux** du type droit, contexte inchangé, aucune erreur. L'assignation est acceptée sans être suivie. | **Haute** |
| A2 | Assignation à une variable **jamais déclarée** : LHS type `Any` (S1) → `right <: Any` passe → la variable est créée silencieusement. `x <- 5` sans `let` devient un `let` implicite non voulu. | Moyenne |
| A3 | Aucune notion d'immutabilité vérifiée ici (`TypeError::ImmutableVariable` n'est utilisé que pour les conflits d'import) — à trancher : est-ce la sémantique voulue ? | À décider |

---

## 7. Couverture de test asymétrique

Tests inline par fichier (`#[test]`) : `mod.rs` 100, `function_application.rs` 55, `function.rs` 39, `type_arithmetic.rs` 20… mais **zéro** dans : `match_expression.rs`, `dollar_access.rs` (453 l.), `dot_pipe_access.rs`, `constructor_call.rs`, `type_comparison.rs` (le cœur de la réduction !), `field_access.rs`, `unification_map.rs`, `module_cache.rs`. Une partie est couverte indirectement depuis `mod.rs`, mais les chemins d'erreur de ces fichiers ne le sont quasiment pas.

---

## Plan de mitigation

**Avancement (2026-07-17) : Phase 1 (S1, S2, S3) faite.** Voir tests `edge_*` dans `type_checking::tests` (mod.rs) et le détail dans la table §1 ci-dessus. Reste de la phase 0 (items M/O/C/I/G/A, hors S1-S3) non fait.

### Phase 0 — Instrumentation avant correction (1 étape courte)
Confirmer chaque item par un test *rouge* avant de toucher au code : un fichier `lab/edge_cases/` + un test Rust `#[ignore]` par item (`edge_s1_undefined_variable`, `edge_m2_union_tag_binding`, …). Ça transforme cet audit en harnais exécutable et donne l'ordre de priorité réel (certains items sont peut-être déjà couverts ailleurs dans le dispatch).

> Fait pour S2/S3 : `edge_na_literal_types_as_na_not_any`, `edge_while_loop_condition_must_be_boolean`, `edge_while_loop_body_type_error_detected` (mod.rs, section « Edge-case audit » en fin de `mod tests`). Le reste des ~25 items n'a pas encore de test rouge.

### Phase 1 — Filet de sécurité systémique (S1-S3, le meilleur ratio effort/valeur) — ✅ FAITE
0. **S2/S3 faits** — voir §1. Les deux dispatchers (`typing()`, `eval()`) sont désormais des `match` exhaustifs sur les 62 variantes de `Lang` ; ajouter une variante à l'enum est maintenant une erreur de compilation tant que `typing()`/`eval()` n'ont pas explicitement décidé quoi en faire.
1. **S1 fait** : `Lang::Variable` indéfini → `TypeError::UndefinedVariable`. Le mécanisme redouté (faux positifs sur les builtins R non typés) ne s'est pas matérialisé : les placeholders `functions_R.txt` (`(Any, UnknownFunction)`) sont déjà présents dans `context.variables()`, donc `get_type_from_variable` réussit pour eux et le nouveau check ne se déclenche que pour les noms *vraiment* absents du contexte.
2. **S2/S3** : remplacer les catch-all par un match exhaustif (lister explicitement les variantes « neutres » restantes) ; le compilateur Rust force alors la décision à chaque nouvel ajout de variante `Lang` — c'est le garde-fou anti-régression le moins cher qui soit.
3. Politique de code (à ajouter dans CLAUDE.md) : **interdiction de retourner `any_type()` sans pousser une erreur**, sauf commentaire justifiant l'exception.

### Phase 2 — `match` (M1-M5)
1. **M2/M5 d'abord** (bindings) : passer le scrutiné par `reduce_type`, gérer `Type::Operator(Union, …)` et les alias d'union génériques uniformément (supprimer le cas `"Option"` codé en dur), réduire l'alias record avant extraction des champs.
2. **M3** : pattern imbriqué non supporté → erreur explicite « pattern non supporté » plutôt que drop silencieux (le support réel peut venir après).
3. **M1** : exhaustivité — réutiliser/mutualiser la logique du quickfix LSP « missing match arms » (elle sait déjà énumérer les variantes manquantes) dans un helper typr-core partagé ; nouvelle `TypeError::NonExhaustiveMatch`.
4. **M4** : vérifier `pattern_type <: scrutinee_type` par branche.

### Phase 3 — Unification & génériques (G1-G3)
1. **G2** : changer la signature de `unification_helper` en `Option<Vec<(Type,Type)>>` (`None` = échec) — mécanique mais transversal ; c'est le prérequis de tout le reste.
2. **G1** : dans `merge_substitutions`, un binding contradictoire (après `resolve_in_chain`) devient un échec au lieu d'un écrasement.
3. **G3** : check d'arité dans `reduce_alias` + `TypeError::AliasArityMismatch` ; occurs-check simple dans le cas `Generic`.

### Phase 4 — Opérateurs, contrôle, assignation (O1-O2, C1-C2, A1-A2)
1. **O1/O2** : réduire les deux opérandes et tester la compatibilité par sous-typage bidirectionnel (pas `==`) ; accepter `int`↔`num` explicitement.
2. **C1** : collecter les types de tous les `Lang::Return` du corps (petite passe récursive) et les unifier avec le type de retour déclaré dans `function()`.
3. **A1/A2** : LHS non-suivi → erreur explicite ; assignation sans `let` préalable → erreur (ou warning selon la sémantique voulue — décision de design à acter).
4. **C2** : flag `in_loop` dans `Context` (comme `self_type`/`in_module_body`), erreur sur `break`/`next` hors boucle.
5. **C3** : supprimer/aligner le fast-path `Lines` len==1 sur le chemin général.

### Phase 5 — Sémantique à trancher (décisions de design, pas des bugs)
- **I1** : index négatif — sémantique R (exclusion) ou sémantique TypR (depuis la fin) + codegen dédié ? Décider et documenter ; l'état actuel (type ≠ runtime) est le pire des deux.
- **A3** : immutabilité des bindings `let` réassignés via `<-`.
- **D1** : niveau de vérification voulu sur les littéraux DataFrame.

### Phase 6 — Harnais préventif (contre les edge cases *futurs*)
1. **Matrice constructions × contextes** : un test génératif qui insère chaque construction (tag, match, lambda, constructor call, spread, cast, …) dans chaque position (corps de fn, champ de record, argument, branche de match, corps de module, valeur par défaut de param) et vérifie qu'on obtient soit un type précis, soit une erreur — jamais `Any` silencieux. C'est exactement le genre de croisement (« X dans Y ») d'où sortent les bugs des mémos (`ConstructorCall` context drop, alias hoisting, Lines single-statement).
2. Dupliquer en snapshots `insta` les messages d'erreur des nouveaux diagnostics.
3. Guard CI : `grep` interdisant tout nouveau `_ =>` dans les deux matchs centraux, et compte de `any_type()` non-décroissant à justifier en revue.
4. Combler les fichiers à zéro test (`type_comparison.rs` en premier : c'est la fonction la plus appelée du checker).

### Ordre recommandé
`P0 → P1 (S2/S3 puis S1, FAIT) → P2 (M2/M3 puis M1) → P3 → P4 → P5/P6 en continu`.
P1.2 (match exhaustif sur `Lang`) est le seul item qui protège *structurellement* contre les edge cases pas encore inventés — c'est celui qui a été fait en premier.
