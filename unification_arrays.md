# Unification vecteurs / arrays — étape ③ : un seul type de surface `[N, T]`

> **Statut : FAIT (2026-07-19).** P1/P2/P3 implémentées et vérifiées (631 tests lib,
> 56 snapshots dont le module `array_unification`, catalogue cases 42 PASS / 0 REGRESS,
> run R réel `typr new` + `typr run --checked` exerçant les deux branches).

Suite des étapes ① (vapply typé + fix `|> ()`) et ② (prédicat de vectorisabilité →
appel direct), faites le 2026-07-19. Cette étape ferme le chantier : la
représentation R d'un tableau est choisie **à la compilation** en fonction du type
d'élément, et `Vec[N, T]` cesse d'être une seconde surface distincte.

## Constat de départ (mesuré sur le code + R 4.5 réel)

Aujourd'hui `[N, T]` (VecType::S3) et `Vec[N, T]` (VecType::Vector) divergent sur
toute la chaîne alors que `Type::PartialEq` les considère déjà égaux (l'impl
ignore le `VecType`) :

| | `[N, int]` (S3) | `Vec[N, int]` (Vector) |
|---|---|---|
| Littéral | `typed_vec(1L \|> as.Integer(), …, dim = c(3))` — boxing par élément | `c(1L \|> as.Integer(), …)` — atomique nu (c() strip les classes) |
| Annotation `let` | `\|> as.ArrayN()` | `\|> identity()` |
| Appel fn scalaire lifté | `vec_apply(f, a)` (boucle liste, 20–100×) | `vapply`/appel direct (étapes ①+②) |
| Suffixe dispatch param | `name.ArrayN` (classe portée par as.ArrayN ✓) | `name.ArrayN` **cassé** : la valeur est nue, jamais de classe ArrayN au runtime |
| stdlib runtime | méthodes `.typed_vec` | rien de dédié |

Le boxing par élément de `typed_vec` est redondant avec le typage statique. Son
seul apport irremplaçable : les arrays d'éléments **non atomiques** (records,
unions, fonctions) où la liste R est la seule représentation possible.

## La règle de représentation (source de vérité unique)

> Un type tableau `Type::Vec(_, N, T)` — quel que soit son `VecType` — a la
> **représentation atomique** (vecteur R nu, `c(...)`) ssi son type d'élément
> résout, à travers la chaîne d'alias **non-opaques** (miroir exact de ce que
> `reduce_type` perce ; l'opacité se lit sur la `Var` de *déclaration*, pas
> sur le nœud d'usage), vers un primitif structurel :
> `int` / `num` / `char` / `bool`. Tout le reste (records, unions/tags,
> fonctions, tableaux imbriqués, alias opaques — `Factor`, … — génériques non
> résolus, interfaces, `Any`) garde la représentation liste `typed_vec`.

Implémentation : `VarType::atomic_array_elem(&Type) -> Option<Type>`
(components/context/vartype.rs — résolution d'alias par nom, bornée, même
technique que `resolves_to_foreign`), exposée via `Context`. Chaque site de
décision appelle **ce prédicat et rien d'autre** ; ne jamais re-dériver la règle
localement, sinon deux sites peuvent choisir des représentations différentes
pour la même valeur.

Conséquences runtime pour la branche atomique :
- Valeur = vecteur atomique nu (pas de conteneur classé : le dispatch S3
  fonctionne sur la classe implicite `integer`/`numeric`/`character`/`logical`,
  et les Ops natives R restent à vitesse C). Le conteneur classé
  (`Integer(c(...))` + `typed_dim`) envisagé initialement est réservé à une
  éventuelle v2 multi-dimensionnelle — en v1, 1-D seulement, `length()` suffit.
- Le dispatch des fonctions utilisateur à 1er param tableau-atomique se fait sur
  la classe implicite (`name.integer`, …) — même mécanisme que les params
  scalaires. Bonus : cela répare le dispatch des params `Vec[N,T]`, aujourd'hui
  suffixés `name.ArrayN` alors que la valeur n'a jamais cette classe.

## Périmètre v1

- **1-D uniquement** : `[N, [M, T]]` a un élément tableau → composite →
  typed_vec, inchangé (lin_alg, littéraux imbriqués linéarisés, tout reste tel
  quel). Le multi-dim atomique plat (`typed_dim = c(...)`) est une v2 possible.
- Les alias `ArrayN` continuent d'être **enregistrés** (stabilité de la
  numérotation RecordN/ArrayN dans les goldens) ; la branche atomique cesse
  simplement de les *utiliser* (annotation `identity`, suffixe implicite).
- `Vec[N, T]` reste parsable (alias de surface) ; les types étant déjà égaux
  côté checker, les deux syntaxes convergent sur les mêmes chemins de codegen.
  Dépréciation syntaxique = suivi séparé, hors périmètre.

## Sites modifiés

### P1 — typr-core (bascule de représentation)

1. `vartype.rs` : `atomic_array_elem` (le prédicat) ;
   `get_type_anotation` → `identity` pour tableau atomique (l'arm Vec existant
   est élargi) ; `get_class`/`get_class_unquoted` → classe implicite pour
   tableau atomique (avant le fallback alias).
2. `transpiling/mod.rs`, arm `Lang::Array` + `array_literal_raw` : élément
   primitif → `c(el1, …)` (les éléments gardent leur transpilation scalaire,
   `c()` strip le boxing) ; littéral vide → `integer(0)`/`numeric(0)`/
   `character(0)`/`logical(0)`.
3. `transpiling/mod.rs`, arm `Lang::VecFunctionApp` : la branche
   vapply/appel-direct (étapes ①+②) s'applique aussi quand les arguments
   vectoriels sont des tableaux atomiques (condition + filtre `vec_positions`
   élargis via le prédicat) ; `vec_apply` ne reste que pour les tableaux
   composites.
4. `checked_assertions.rs` : tableau atomique → descripteur `typeof`
   (`"integer"`/…, vecteur-safe), composite → inchangé (classe `ArrayN`).

### P2 — std.R (méthodes .default atomiques)

`map.default`/`filter.default` ne doivent plus renvoyer un `typed_vec` pour une
entrée atomique (adaptatif : résultats tous scalaires atomiques de même typeof →
`unlist`, sinon `typed_vec`) ; `reduce.default`, `extend.default` ajoutés ;
`fold.default` sans wrap `typed_vec`. `sum`/`max`/`min`/`mean`/`sd`/`set_at`/
`rev`/`as_vec` ont déjà des `.default` corrects sur atomique.

### P3 — vérification

`cargo test --workspace`, snapshots (les snapshots arrays-primitifs churment
légitimement), `typr case run` (freeze des goldens concernés après revue),
run R réel `typr new` + `typr run` exerçant les deux branches, lab/vec_step3*.

## Non-couvert / risques assumés

- Overloads même nom `fn(x: int)` + `fn(x: [N, int])` : même suffixe de dispatch
  `.integer` → collision. Cas non rencontré dans la stdlib ni les cases ; à
  détecter plus tard par un lint si besoin.
- `reduce.typed_vec` renvoie un `typed_vec` d'1 élément (contradiction avec la
  signature `-> T`) — quirk préexistant de la branche composite, non traité ici.
- Tableaux composites : rien ne change, y compris leurs lenteurs.
- Découvert en vérifiant (hors périmètre, non corrigé) : le manifest des builds
  incrémentaux ne hash pas le `STD_R` embarqué — éditer `configs/src/std.R`
  sans bump de version laisse le `R/std.R` d'un projet chaud périmé. `typr
  clean` en dev ; candidat fix : intégrer hash(STD_R) au sel du manifest.
