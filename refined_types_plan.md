# Types raffinés — plan d'implémentation

Plan pour `refined_types.md`, écrit après lecture de `typr-core` (état au 2026-09-29, branche `develop`).

---

## 0. Ce qui existe déjà (et ce qui change la donne)

L'analyse du code montre que plusieurs pièces de la spec existent déjà, sous une autre forme.

| Élément de la spec | État actuel dans le code |
|---|---|
| `&` dans les types | Parsé (`parsing/types.rs:1062`) en `Type::Operator(TypeOperator::Intersection, ..)`, mais **limité aux Records/Interfaces** (`type_arithmetic.rs::accepts_record_kind`, `norm_intersection`). `int & int` échoue. |
| Aplatissement, commutativité, déduplication | `IntersectionType` (`components/type/intersection_type.rs`) aplatit l'arbre binaire dans un `HashSet<Type>`. L'égalité `A & B == B & A` est déjà en place (`type/mod.rs:1229`). |
| `length(N)` sur les vecteurs | **La longueur est déjà dans le type** : `Type::Vec(VecType, index, elem, h)`. `[5, int]` donne `Vec(S3, Integer(5), int)` et `[int]` donne `Vec(S3, Any, int)`. |
| `[5,int] <: [int]` | Fonctionne déjà (`Integer(5) <: Any`, `type/mod.rs:221`). |
| Contradiction `length(5)` / `length(3)` | Déjà une erreur statique : `let x: [5, int] <- [1,2,3]` est rejeté. |
| Inférence `[1,2,3,4,5] : [5, int]` (§14) | Déjà en place. |
| `c()` produit une longueur (§28) | Déjà en place : `c([1,2],[3,4,5])` a le type `Vec[5, int]`, `c(x, x)` a le type `Vec[10, int]`. |
| Préservation par opération vectorisée | `x + 1` garde `[5, int]` (lifting, `Type::lift`). |
| Validations runtime | Seulement en mode test `--checked` (`transpiling/checked_assertions.rs`, `typr_assert_type`). Elles contrôlent la classe, pas la longueur. |

Les sondes faites contre le binaire actuel ont aussi révélé des défauts à corriger en chemin :

- **`int & (> 0)` fait paniquer le parseur** (`parsing/types.rs:974`, `compute_operators` → `_ => panic!()`), parce que `>` est lu comme un `Op` dans `index_algebra`.
- `[int] & length(5)` est parsé en `[any, int] & length` : `length` devient un `Type::Variable` et le `(5)` est perdu.
- `x[1:3]` et `x[x > 2]` sont typés **`int`** alors que ce sont des vecteurs. C'est un bug existant, et c'est précisément la propagation dont parlent les §15-16.
- `[int]` passé là où `[3, int]` est attendu est **une erreur statique** aujourd'hui. La spec (§22-23) en fait un site de validation runtime : **c'est un changement de comportement**, voir D3.

---

## 1. Décisions de conception

### D1. Représentation : un variant `Refined` plus un variant transitoire `Property`

```rust
// components/type/mod.rs — à ajouter À LA FIN de l'enum (voir §Risques : sérialisation .bin)
Refined(Box<Type>, RefinementSet, HelpData),   // base & propriétés, forme normalisée
Property(Refinement, HelpData),                // sortie du parseur uniquement ; ne doit jamais survivre à reduce_type
```

C'est la forme « `RefinedType { base, refinements }` » que recommande la spec au §5. On n'étend pas `Operator(Intersection)` à la place, pour deux raisons :

- dans le code actuel, `&` sur des Records veut dire *fusion structurelle*, pas prédicat ;
- un arbre binaire ne donne ni l'ordre canonique ni la détection des contradictions sans renormaliser à chaque fois.

Le parseur continue de produire `Operator(Intersection, base, Property(p))`. C'est `norm_intersection` qui les replie en `Refined`.

### D2. `RefinementSet` : raisonner par *mesures* et *intervalles*

Plutôt qu'une liste de propriétés à comparer deux à deux, on normalise chaque propriété sur une **mesure** munie d'un **intervalle** :

```rust
// components/type/refinement.rs (nouveau)
pub enum Measure { Value, Length }          // extensible : Nchar, Nrow, Ncol...
pub struct Interval { lo: Bound, hi: Bound } // Bound = Open(c) | Closed(c) | Unbounded
pub struct RefinementSet(BTreeMap<Measure, Interval>);  // ordonné → Hash/Eq déterministes

pub enum Refinement {                        // forme de surface (parse / affichage)
    Length(i32),                             // length(5)   → Length ∈ [5,5]
    Gt(Lit), Lt(Lit),                        // (> 0)       → Value ∈ (0, +∞)
}
```

Avec cette forme, les exigences de la spec tombent d'elles-mêmes :

| Exigence | Traduction |
|---|---|
| dédup, commutativité, associativité (§6, invariants 1-2) | intersection d'intervalles par mesure |
| `contradicts(P,Q)` (§10-11) | intervalle vide (`(>10)&(<5)`, `length(5)&length(10)`) |
| `implies(P,Q)` | inclusion d'intervalles (`(>5)` ⊂ `(>0)`) |
| `compatible(P,Q)` | intersection non vide |
| extensions §36 (`length(> 0)`, `>=`, plages) | autres bornes sur le même modèle, sans changer le principe |
| narrowing §24 | on intersecte l'intervalle de la condition |

Bonus : sur `int`, on peut repérer `int & (> 0) & (< 1)` comme vide, en arrondissant les bornes aux entiers.

Il faut une représentation hachable des constantes numériques (`Type: Hash + Eq`). On réutilise celle de `Tnum` plutôt qu'un `f64` brut.

### D3. Trois états, et `is_subtype` reste strict

`is_subtype` est un booléen mis en cache (`SUBTYPE_CACHE`) et utilisé partout : dispatch, unification, sélection de surcharge. **Il ne doit répondre `true` que pour `Proven`.** L'état `Unknown` passe par une fonction séparée, appelée uniquement aux frontières :

```rust
// processes/type_checking/refinement_check.rs (nouveau)
pub enum Proof { Proven, Refuted, Unknown }
pub enum Coercion { Static, Runtime(RefinementSet /* résidu non prouvé */), Reject }
pub fn coerce_to(found: &Type, expected: &Type, ctx: &Context) -> Coercion
```

La règle : `coerce_to` renvoie `Runtime(résidu)` si `base(found) <: base(expected)` et que le résidu est `Unknown`. Il renvoie `Reject` si le résidu est `Refuted`.

Si on laissait `is_subtype` devenir tolérant, un `[int]` sélectionnerait une surcharge `[5, int]`, et le code deviendrait insoundable sans que rien ne le signale.

### D4. La longueur des vecteurs reste dans l'index de `Type::Vec`, pour l'instant

La spec (§4) veut la forme canonique `Intersection(Vector(Int), Length(5))`. On fait **l'inverse en v1** : `[int] & length(5)` se normalise en `Vec(S3, Integer(5), int)`. Une vue `refinements_of(&Type) -> RefinementSet` relit la longueur depuis l'index.

Pourquoi :
- `Type::Vec(` apparaît dans **135 motifs** répartis sur 20 fichiers ;
- le lifting vectoriel (`Type::lift`) et les génériques d'index `#N` (`[#N, T]`, arithmétique `#N+1`) dépendent tous de ce slot ;
- une migration en une fois casserait les 868 tests pour un gain nul en v1.

Le §5 l'autorise explicitement (« optimisation sémantique, ne doit pas changer le comportement »), à condition de retrouver `A & P` à partir de la structure : c'est le rôle de `refinements_of`.

Les intervalles de longueur non ponctuels (`length(> 0)`, futur) iraient dans le `RefinementSet` d'un `Refined(Vec(S3, Any, T), …)`. La migration complète vers `Length` est la phase 9, optionnelle.

### D5. Les obligations runtime sont décidées au typage, pas au codegen (§32)

Quand `coerce_to` renvoie `Runtime(résidu)`, le type checker enregistre une **obligation** dans le `Context` : une table latérale indexée par la position (`HelpData`) de l'expression, sur le modèle de `type_recorder.rs`. Le transpileur se contente de la consommer et ne raisonne jamais sur les raffinements.

Ensuite, la variable entre dans l'environnement avec le type raffiné. C'est le « check at boundary » du §20 : aucune nouvelle vérification aux usages suivants.

### D6. Forme du R généré

Le public cible est constitué de développeurs R qui lisent le code généré. Il faut donc que ce soit **court, lisible et inconditionnel** (hors du mode `--checked`). Je recommande de petits helpers du prélude qui renvoient la valeur, pour qu'ils fonctionnent aussi en position d'expression (arguments, `return`) :

```r
a <- typr_refine_length(read_coordinates(), 2L, "main.ty:12")
distance(typr_refine_length(a, 2L, "main.ty:14"), b)
```

Le message produit est `Type refinement violation at main.ty:14: expected length 2, got 3` (§30).

Pour les comparaisons, le test est `isTRUE(all(x > 0))`, qui est sûr face aux `NA` et aux vecteurs. Une variante « `if (...) stop(...)` inline » reste possible pour les `let`. Le choix est à trancher, voir Q4.

---

## 2. Phases

Chaque phase laisse `cargo test --workspace` et `typr case run` au vert.

### Phase 0 — Préparation
- Commiter d'abord le travail en cours sur les angles morts (cas 0068-0072). Il touche `type/mod.rs`, `parsing/types.rs` et `type_checking/mod.rs`, exactement les fichiers de la phase 1, et mélanger les deux diffs serait pénible.
- Trancher les questions ouvertes (§3 ci-dessous), surtout Q1 à Q3.

### Phase 1 — Modèle (`typr-core/components/type`)
- Créer `refinement.rs` avec `Measure`, `Interval`, `RefinementSet`, `Refinement`, et les opérations `meet`, `is_empty`, `implies`, `compatible`, `contradicts`.
- Ajouter `Type::Refined` et `Type::Property` à la fin de l'enum. Compléter les `match` exhaustifs dans `typr-core`, `typr-graph`, `typr-lsp` et `typr-wasm` : le compilateur les listera.
- Ajouter `refinements_of(&Type)`. Elle lit l'index `Vec` → `Length`, les littéraux `Integer(Val(3))`/`Number` → `Value ∈ [3,3]`, et `Refined` → son ensemble.
- Mettre à jour `type_category.rs` (catégorie `Refined`) et `type_printer.rs` : affichage `[int] & length(5)` et `int & (> 0)`.
- **Tests unitaires** : l'algèbre des intervalles, plus les lignes « A&B==B&A », « A&A==A », « (A&B)&C » et « contradiction » du §34.

### Phase 2 — Parser et syntaxe
- Dans `parsing/types.rs`, ajouter un parseur `refinement_property`, essayé **avant** `parenthese_value` et `type_variable` dans `single_type` :
  - `(` `>`|`<` littéral `)` → `Type::Property(Gt/Lt)`
  - `length(` entier `)` → `Type::Property(Length)`
- Corriger la panique de `compute_operators` (erreur de parse au lieu de `panic!`) et ajouter un cas `cases/` pour `int & (> 0)`.
- `[5, int]` ne change pas : c'est déjà la forme de stockage d'après D4.
- **Manifeste de syntaxe** (`components/syntax/mod.rs`) : déclarer `length` comme mot de type et `>`/`<` en position de type. Les deux tests d'invariant l'exigent. Ensuite, lancer `typr syntax --write` pour régénérer la grammaire tmLanguage.
- ⚠️ Côté `typr.github.io`, `scripts/syntax-glossary.mjs` doit recevoir la glose des nouveaux lexèmes, sinon le build de la doc casse. Le changement doit être fait **en même temps** dans les deux dépôts.

### Phase 3 — Normalisation et validation (`type_arithmetic.rs`)
Étendre `norm_intersection` :
- `(X, Property p)` → `Refined(X, {p})` après le contrôle de capacité. Même chose en symétrique.
- `(Refined(A,ps), Property q)` → `Refined(A, ps ⊓ q)`.
- `(Refined(A,ps), Refined(B,qs))` → `Refined(norm(A&B), ps ⊓ qs)`.
- `(Vec(_, Any, T), Length n)` → `Vec(_, n, T)` (D4). Si `(Vec(_, m, T), Length n)` avec `m ≠ n`, c'est une contradiction.
- Littéral + propriété : `3 & (> 0)` → `3` si prouvé, contradiction si réfuté.
- Un ensemble vide donne l'erreur **`UnsatisfiableRefinement`** (§11, §29).
- Un `Property` seul qui survit à la réduction donne une erreur « une propriété n'est pas un type ».

Les **capacités** (§8) passent par une simple fonction `capabilities(base) -> {Lengthable, Ordered}` :
- `Vec` → Lengthable ;
- `Integer`/`Number` → Ordered ;
- générique non contraint → rien en v1.

Si la capacité manque, on lève **`InvalidRefinement("length(5)", int)`**.

Le cas `&` sur des Records reste inchangé.

Ajouter les nouveaux variants dans `error_message/type_error.rs` avec leurs codes `T0xx` (utilisés par le MCP) et leurs messages au format du §29.

`type Coordinates = [num] & length(2)` fonctionne sans travail spécifique, par la réduction des alias.

### Phase 4 — Sous-typage (`type/mod.rs::is_subtype_raw`)
- `Refined(A, ps) <: B` si `A <: B` (invariant 4, §9).
- `A <: Refined(B, qs)` si `A <: B` et `decide(refinements_of(A), qs) == Proven`.
- `Refined(A, ps) <: Refined(B, qs)` si `A <: B` et `ps implies qs`.
- Placer ces règles **avant** les règles génériques `Operator(Intersection, ..)` existantes, qui feraient sinon `t1 <: typ || t2 <: typ` sur un `Property`.
- Vérifier l'unification (`unification.rs`) : un générique `T` lié à un argument raffiné prend le type raffiné complet (paramétricité, donc sûr). En revanche, les opérateurs arithmétiques et les fonctions non annotées renvoient le **type de base**. L'invariant 7 est ainsi garanti par construction.

### Phase 5 — Frontières et obligations (§12, 13, 19-23)
- Créer `refinement_check.rs` avec `coerce_to` (D3) et la table d'obligations dans le `Context` (D5).
- Brancher `coerce_to` là où un type attendu rencontre un type trouvé :
  - `let_expression.rs` : annotation de `let` ;
  - `function_application.rs` : arguments, côté appelant (§37) ;
  - `function.rs` : type de retour ;
  - les constructeurs de record, dans un second temps.
- Paramètres : à l'intérieur du corps, `x` a le type raffiné sans aucune vérification (§12).
- **Changement de comportement** : `[int]` → `[3, int]` passe d'une erreur statique (T001/T002) à une vérification runtime. Il faut repérer les tests et `cases/` qui attendaient l'erreur (`typr case run` les signalera en REGRESS) et les réécrire comme témoins de l'absence ou de la présence de vérification.

### Phase 6 — Codegen (`transpiling/`)
- Écrire le module `transpiling/refinement_checks.rs`. Il consomme les obligations et génère les appels du prélude (D6). Chaque `Measure` sait produire son test (§21) : `Length` → `length(x) == n`, `Value` → `x > c` / `x < c`.
- Ajouter les helpers `typr_refine_*` au prélude R, là où vit `typr_assert_type`.
- Aucune vérification n'est émise quand l'obligation est absente (invariants 5 et 6, §18, §22).
- Plus tard, en v1.1 (Q3) : un prologue de vérification pour les fonctions **exportées** d'un package, puisque l'appelant y est du R non typé.

### Phase 7 — Propagation et invalidation (§15-17, §27-28)
- Corriger au passage `x[1:3]` et `x[x > 2]`, aujourd'hui typés `int`. Ils doivent donner respectivement `[3, int]` (si les bornes sont littérales, sinon `[int]`) et `[int]`.
- `length(x)` avec `x : [5, int]` doit donner le type littéral `5`, ce qui prépare le narrowing.
- Règle par défaut, conservatrice : une fonction dont l'effet n'est pas déclaré renvoie son type de retour déclaré, **sans** raffinement.
- Les effets déclaratifs (preserve/produce/invalidate) sont d'abord codés en dur pour une poignée de primitives (`[`, `:`, `c`, `length`, `rev`, `head`). Une syntaxe d'annotation dans `configs/std/*.ty` pourra venir plus tard.

### Phase 8 — Tests, cas, documentation
- Les **tests unitaires** couvrent les 14 lignes du §34.
- Côté **`cases/`**, prévoir un cas par règle de codegen. Le format `expect.toml` (des règles sur des sous-chaînes) convient bien à « présence de check » et à « absence de check inutile » : lire `cases/README.md` pour la règle d'absence. Au minimum :
  - un littéral prouvé ne produit pas de check ;
  - `[int]` → `[5,int]` produit un check ;
  - raffinement de paramètre ;
  - raffinement de retour ;
  - contradiction ;
  - capacité invalide ;
  - `int & (> 0)` (la panique actuelle).
- Mettre à jour `syntaxe.md`, dans ses **deux** copies, et la page de référence des types dans `typr.github.io` (Diátaxis : reference, plus une explication dans `concepts/`). Les exemples doivent passer `typr check`, car la CI `check:examples` les vérifie.
- Lancer `typr std` si les signatures du std changent (effets de la phase 7).

### Phase 9 — Plus tard (hors v1)
- Narrowing par condition (§24) : `if (length(x) == 5)` intersecte l'intervalle dans la branche. Le modèle par intervalles le permet directement.
- Génériques `[T] & length(> 0)` (§25) et bornes d'intervalle sur `Length`.
- Migration de l'index de `Type::Vec` vers `Measure::Length`, et remplacement des types dépendants `#N` : c'est l'objectif final annoncé par la spec au §1.

---

## 3. Questions ouvertes sur la spec

1. **`string & length(10)`** (§26) : en R, `length("abc")` vaut `1`. Parle-t-on de `nchar` ? Si oui, il faut une mesure distincte (`Nchar`), sans quoi le raffinement serait trompeur pour un dev R.
2. **`(> 0)` sur un vecteur** : `[int] & (> 0)` veut-il dire « tous les éléments » ? D'ailleurs un `int` TypR est un vecteur R de longueur 1. Je propose le sens « tous », vérifié par `isTRUE(all(x > 0))`, ce qui règle aussi le cas `NA`.
3. **Où placer les vérifications de paramètres** : côté appelant (§37) ou dans le prologue de l'appelé ? Je recommande l'appelant pour le code TypR (on évite le check quand c'est prouvé, invariant 5), plus un prologue pour les fonctions exportées (appels depuis du R non typé).
4. **Le narrowing implicite avec vérification runtime** (`[int]` → `[5,int]` accepté puis vérifié) remplace une erreur statique actuelle. Veut-on un avertissement, ou une option pour exiger une conversion explicite ?
5. **Forme canonique du §4** : la v1 stocke la longueur dans `Type::Vec` (D4). Il faut confirmer que c'est acceptable tant que `refinements_of` redonne `A & length(N)`.
6. **Bornes `num` vs `int`** : `int & (> 0.5)` doit-il être accepté (équivalent à `>= 1`) ou rejeté ?

---

## 4. Risques

- **Les `.bin` du std sont sérialisés** (`serde`) : ajouter les variants **à la fin** de `enum Type` pour garder les indices, puis lancer `typr std`.
- **Le cache de sous-typage** : `Refined` doit avoir un `Hash`/`Eq` canonique (d'où le `BTreeMap`), sinon `A&B` et `B&A` occupent deux entrées différentes.
- **Récursion** : la normalisation passe par `reduce_type`. Garder `RUST_MIN_STACK=8388608` en tête si un test déborde.
- **Soundness** : ne jamais laisser `is_subtype` répondre `true` sur `Unknown` (D3). C'est l'invariant qui empêche de « transformer silencieusement l'incertitude en vérité » (§31).

## 5. Ordre de livraison

| PR | Contenu | Impact utilisateur |
|---|---|---|
| 1 | Phase 0 + Phase 1 + correction de la panique `int & (> 0)` | aucun (modèle interne) |
| 2 | Phases 2-4 : syntaxe, normalisation, sous-typage, erreurs statiques | `int & (> 0)` et `[int] & length(5)` acceptés ; contradictions et capacités invalides détectées |
| 3 | Phases 5-6 : obligations et codegen | vérifications runtime (changement de comportement Q4) |
| 4 | Phase 7 : propagation, avec la correction de `x[1:3]` | types plus précis |
| 5 | Phase 8, docs (dans les deux dépôts) | — |
