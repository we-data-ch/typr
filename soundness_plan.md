# Soundness Plan — Prochaines étapes après les 4 phases

**Ce document décrit le backlog priorisé après l'implémentation des phases A, B, C et D v1
de `soundness_transpilation.md` (2026-07-18).** Chaque section correspond à une unité de
travail autonomisable, avec objectif, fichiers à modifier, critères de fin, et risques.

Le fil rouge : chaque case `cases/` ajoutée est un critère d'acceptation. Une phase est
terminée quand `typr case run --status open` donne le verdict attendu (PASS ou OPEN avec
un `expect.toml` qui capture le comportement réel) pour toutes ses cases.

---

## Phase D — Compléter la matrice d'interop (priorité 1)

### D.1 — Lignes RC et R6 (colonnes a, b, c, f)

**Pourquoi maintenant :** `R6` est installé dans le dev/CI (`methods::` aussi, nécessaire
pour RC). Les objets à sémantique référence (mutation via environnement partagé) sont
qualitativement différents des S3/S4/base-attributs déjà couverts : le marshalling
`to_native`/`from_native` peut interagir différemment avec un objet dont l'identité
importe (une copie superficielle = perte de la référence). 4 colonnes × 2 systèmes = 8 cases.

| Case | Row | Col | TypR code | Comportement attendu |
|------|-----|-----|-----------|----------------------|
| D.1.1 | RC | a | `f(refclass_instance)` | PASS — dispatch `.default` |
| D.1.2 | RC | b | `let x <- rc_fn();` | PASS — valeur retournée, pas de re-classing |
| D.1.3 | RC | c | `let x: T <- rc_fn();` | PASS — `Foreign<T>` → `identity()`, pas `as.X()` |
| D.1.4 | RC | f | `x$field` sur RC | `wontfix` (même documented hole que colonne f existante) |
| D.1.5 | R6 | a | `f(r6_obj)` | PASS |
| D.1.6 | R6 | b | `let x <- r6_fn();` | PASS |
| D.1.7 | R6 | c | `let x: T <- r6_fn();` | PASS |
| D.1.8 | R6 | f | `x$method()` sur R6 | `wontfix` (même documented hole) |

**Fichiers à créer :**
- `cases/0030-foreign-rc-function-arg/` (D.1.1)
- `cases/0031-foreign-rc-function-return/` (D.1.2)
- `cases/0032-foreign-rc-let-annotation/` (D.1.3 + `checked = true`)
- `cases/0033-foreign-rc-dot-access-rejected/` (D.1.4, `status = wontfix`)
- `cases/0034-foreign-r6-function-arg/` (D.1.5)
- `cases/0035-foreign-r6-function-return/` (D.1.6)
- `cases/0036-foreign-r6-let-annotation/` (D.1.7 + `checked = true`)
- `cases/0037-foreign-r6-dot-access-rejected/` (D.1.8, `status = wontfix`)

**Fichiers R à créer/modifier :** `tools/gen_interop_fixtures.R` — ajouter la génération
des fixtures `.rds` pour un objet `setRefClass` simple et un objet `R6::R6Class` simple.

**Critères de fin :**
- `typr case run` sur les 8 cases → les 4 colonnes a/b/c passent, les 4 colonnes f sont
  `wontfix` avec le message d'erreur attendu.
- La grille `interop_matrix.md` est mise à jour (lignes 3/4 remplies, √ colonnes a/b/c,
  `wontfix` colonne f).

**Risques :**
- RC/R6 peuvent avoir un class vector inattendu qui fait échouer `UseMethod` — à
  documenter dans `expect.md` si c'est le cas (c'est le bug qu'on cherche).
- `to_native.default` est `identity` — si un objet R6 a besoin d'être « dépaqueté » de
  son environnement, ça peut échouer silencieusement.

---

### D.2 — Colonne i : frontière de module

**Pourquoi maintenant :** L'exploration du code (juillet 2026) a révélé un gap réel : le
`.default` de dispatch `Foreign<T>` est généré *à l'intérieur* de `local({...})` mais
**n'est pas ré-exporté** à l'extérieur. Une fonction de module avec un paramètre
`Foreign<T>` est inatteignable depuis l'extérieur — c'est une variante du bug #4 de la
matrice, non corrigée.

**Structural gap (à corriger d'abord) :** Dans `processes/transpiling/mod.rs`, le bloc
de post-traitement du module (lignes ~2165-2224) exporte les méthodes typées
(`M$name.Foreign0 <- name.Foreign0`) et les stubs génériques (`name <- function(x, ...)
UseMethod("name", x)`), mais **pas** le fallback `.default`. Ajouter l'export
`name.default <- M$name.default` au même endroit.

**Fichiers à modifier :**
- `crates/typr-core/src/processes/transpiling/mod.rs` — ajouter l'export du `.default`
  dans le post-processing du module (~ligne 2222).

**Fichiers à créer (après la correction) :**
- `cases/0038-foreign-s3-module-boundary/` — S3 `lm` passé à une `@pub fn` dans un module.
- `cases/0039-foreign-s4-module-boundary/` — S4 `Matrix` passé à une `@pub fn` dans un module.
- `cases/0040-foreign-factor-module-boundary/` — `factor` (base-attribué) dans un module.

**Critères de fin :**
- `typr case run` sur les 3 cases → PASS.
- Le gap structural est corrigé ET testé (test unitaire dans le module de transpilation
  ou snapshot).

**Risques :** L'export du `.default` pourrait créer un doublon si la même fonction est
ré-exportée deux fois (une fois comme `.Foreign0`, une fois comme `.default`) —
vérifier que Rust/R gère les assignments redondants sans erreur (`name.default <-
name.default`).

---

### D.3 — Colonne j : instanciation générique

**Objectif :** Vérifier que `id(foreign_val)` (ou tout appel de fonction générique avec
un argument `Foreign<T>`) ne force pas une classe synthétique sur la valeur.

**Contexte :** La transpilation d'un `let id <- fn(x: T): T { x };` monomorphise sur le
type concret à l'appel. Pour `Foreign<T>`, le type concret est un alias opaque — le
dispatch `get_class`/`get_type_anotation` doit déjà donner `identity()` et pas `as.X()`.
À confirmer avec des cases réelles.

**Fichiers à créer :**
- `cases/0041-foreign-s3-generic-instantiation/` — `id(lm_model)`.
- `cases/0042-foreign-s4-generic-instantiation/` — `id(s4_matrix)`.
- `cases/0043-foreign-factor-generic-instantiation/` — `id(factor_val)`.

**Critères de fin :**
- Les 3 cases passent (PASS) — ou le cas échéant, un bug est trouvé et catalogué.

---

### D.4 — Colonnes d/e/g/h (record field, `match` subject, pipe, `for` iterable)

**Objectif :** Étendre la grille horizontalement pour les 4 nouvelles colonnes, d'abord
sur les lignes déjà stables (S3, S4, base-attributs) avant d'attaquer RC/R6.

| Colonne | Frontière | Priorité |
|---------|-----------|----------|
| d | Champ de record `Point:{ x = foreign_val }` | Haute (usage fréquent) |
| e | Sujet de `match` sur union avec payload `Foreign<T>` | Moyenne |
| g | Pipe `foreign_val \|> f()` | Haute (usage fréquent) |
| h | Itérable de `for x in foreign_vec { ... }` | Basse (rare avec étranger) |

**Fichiers à créer :** 3 lignes × 4 colonnes = 12 cases, mais prioriser les
intersections les plus probables dans un projet réel :

- **Phase 1 (haute priorité) :**
  - `cases/0044-foreign-s3-record-field/` (S3 × d)
  - `cases/0045-foreign-s3-pipe/` (S3 × g)
  - `cases/0046-foreign-s4-record-field/` (S4 × d)
  - `cases/0047-foreign-s4-pipe/` (S4 × g)
  - `cases/0048-foreign-factor-record-field/` (factor × d)
  - `cases/0049-foreign-factor-pipe/` (factor × g)

- **Phase 2 (moyenne priorité) :**
  - `cases/0050-foreign-s3-match/` (S3 × e)
  - `cases/0051-foreign-s4-match/` (S4 × e)
  - `cases/0052-foreign-factor-match/` (factor × e)
  - `cases/0053-foreign-s3-for/` (S3 × h) — si applicable
  - `cases/0054-foreign-s4-for/` (S4 × h)
  - `cases/0055-foreign-factor-for/` (factor × h)

**Critères de fin :** Phase 1 complète (6 cases, toutes PASS ou OPEN documenté).
Phase 2 : au moins 2 cases.

**Risques :**
- Colonne e : le parser exige que le sujet du `match` soit une variable — les valeurs
  étrangères arrivent via `readRDS`, normalement liées à un `let`. Si le `let` est
  annoté avec un type union alias, la valeur étrangère ne portera jamais les tags de
  l'union → `match` typR ne peut pas matcher dessus. Solution probable : documenter
  comme `wontfix` dès le départ.
- Colonne g : le pipe `.`/`|>` passe par `dot_pipe_access.rs`. `Foreign<T>` résout
  `Any` → pas de champ structurel → probablement le même comportement que colonne f.

---

### D.5 — Ligne S7

**Objectif :** Installer le package `S7` dans l'environnement de dev/CI et répéter le
quartet a/b/c/f pour une classe S7.

**Prérequis :** `Rscript -e 'install.packages("S7")'` (ou équivalent).

**Fichiers à créer :**
- `cases/0056-foreign-s7-function-arg/`
- `cases/0057-foreign-s7-function-return/`
- `cases/0058-foreign-s7-let-annotation/` (+ `checked = true`)
- `cases/0059-foreign-s7-dot-access-rejected/` (`status = wontfix`)

**Critères de fin :** `typr case run` sur les 4 cases donne le verdict attendu. La ligne
S7 de la grille passe de `n/a` à cases remplies.

---

### D.6 — Ligne 6 : closure / environnement pur

**Objectif :** Une closure R capturée par une factory — probablement la ligne la plus
simple (pas de class vector à corrompre). Vérifier qu'elle traverse sans encombres les
4 colonnes a/b/c/f.

**Fichiers à créer :**
- `cases/0060-foreign-closure-function-arg/`
- `cases/0061-foreign-closure-function-return/`
- `cases/0062-foreign-closure-let-annotation/`
- `cases/0063-foreign-closure-dot-access-rejected/` (`status = wontfix`)

**Critères de fin :** 4 cases PASS.

---

## Phase B — Étendre le générateur de programmes

### B.1 — Couverture : kinded generics, interfaces, modules, pipes, lambdas

**Objectif :** Ajouter des productions au générateur (`program_gen.rs`) pour couvrir
les variants `Lang`/`Type` manquants. La couverture rapportée par `typr fuzz --stats`
est le guide.

**Production par production, par ordre de priorité :**

| Production | Variants `Lang` | Complexité | Effet sur couverture |
|------------|-----------------|------------|----------------------|
| Lambda | `Lambda` | Faible | Unbound params → simple |
| Pipe (`\|>`) | `FunctionApp` via `Op::Pipe` | Faible | Croisement pipe × tout |
| Module | `Module`, `UseModule` | Moyenne | 2 nouveaux variants + frontière |
| Partial application | `PartialApp` | Moyenne | Désucrage en `Function` |
| Kinded generics | `%T`, `^T`, `#N` | Haute | Beaucoup de variants génériques |

**Fichiers à modifier :**
- `crates/typr-core/src/utils/program_gen.rs` — ajouter les nouvelles productions
  dans `ALL_PRODUCTIONS`, `applicable`, `weight`, et l'implémentation dans `gen_expr`.
- `crates/typr-core/tests/generative.rs` — aucun changement (le test existant roule
  le générateur, qui inclura les nouvelles productions automatiquement).

**Détail par production :**

**B.1.1 — Lambda :**
```rust
Production::Lambda => {
    let ptype = random_primitive(rng);
    let pname = env.fresh_name("p");
    // let body_env = env.clone() + pname: ptype;
    let body = gen_expr(&body_env, goal, depth - 1, rng, cov);
    format!("\\({pname}: {ptype}) {body}")
}
```
Applicable quand `goal` est `Type::Function`. Poids modéré (~6).

**B.1.2 — Pipe (`|>`) :**
Pipet une variable de l'env dans une fonction : `x |> f()` = `f(x)`. Applicable
quand il existe une fonction dont le premier paramètre matche le type d'une var.
Détection : `env.vars` × `env.fns` croisement.

**B.1.3 — Module + Use :**
Déclarer `module M { @pub let f <- fn(...)...; }` dans les déclarations, puis
`use M::f;` dans les déclarations suivantes — la variable `f` devient disponible.
Le module lui-même n'est pas exporté hors du programme généré (pas besoin).

**Risques :**
- Kinded generics `#N` nécessitent de suivre les contraintes de kind (`%T` kind,
  `^T` kind, `#N` cardinal). Le générateur devrait pouvoir les éviter en v1
  (garder la règle "record fields / vector elements / union payloads = primitives")
  et ne les ajouter qu'après que la couverture de base est stable — exactement
  comme la v1 du plan original a exclu les interfaces.

**Critères de fin :**
- Lambda + Pipe + Module implémentés (`applicable`/`gen_expr` + tests).
- `typr fuzz --stats 256` montre couverture `Lang ≥ 40/51`, `Type ≥ 25/30` (vs
  `34/51` et `22/30` en v1, cf. `generative.rs` couplé au générateur).

---

### B.2 — Structural shrinking (Phase B)

**Objectif :** Ajouter un shrinker structurel qui retire les déclarations redondantes
d'un programme échoué *en respectant les dépendances* (un binding n'est retiré que si
aucune autre déclaration ne l'utilise). Actuellement, les programmes sont assez petits
(≤ 2 alias/fn/val, depth ≤ 4) pour que ce ne soit pas urgent — mais le deviendra
quand B.1 ajoutera des productions plus grosses.

**Approche :**
1. Analyser le programme généré pour construire un DAG de dépendances : chaque
   déclaration `let name <- ...` ou `type Name <- ...` a des dépendances vers
   les noms qu'elle référence.
2. Shrinker par élimination : retirer une déclaration si son nom n'est référencé
   par aucune autre déclaration et par l'expression terminale.
3. Itérer jusqu'à point fixe (algorithme glouton).

**Fichiers à modifier :**
- `crates/typr-core/src/utils/program_gen.rs` — ajouter `fn shrink(program: &GenProgram)
  -> GenProgram`.
- `crates/typr-cli/src/fuzz.rs` — appeler `shrink` sur le programme avant persistance.

**Critères de fin :**
- Un programme artificiel avec une déclaration inutilisée voit celle-ci retirée.
- `typr fuzz promote <hash>` persiste un programme minimal.

---

## Phase A — Deep checking v2

### A.1 — Vérification récursive des champs de records

**Objectif :** `--checked` est actuellement shallow (class vector / typeof uniquement).
Une v2 `--checked=deep` ajouterait la vérification récursive des champs pour les
types records.

**Conception :**
```r
# deep assertion pour un record
typr_assert_type_deep(x, list(
  class = c("Point", "list"),
  fields = list(
    x = "integer",
    y = "double"
  )
), "file.ty:12", "let p")
```

Implémentation côté Rust : `checked_descriptor` déjà existe. Pour le mode deep,
produire une structure R imbriquée. Le runtime R (`typr_assert_type_deep` dans
`std.R`) vérifie chaque champ récursivement.

**Fichiers à modifier :**
- `crates/typr-core/src/processes/transpiling/checked_assertions.rs` — nouveau
  `checked_descriptor_deep` ou flag dans `Config::checked_mode` (passer à `enum`
  au lieu de `bool`).
- `crates/typr-cli/configs/src/std.R` — ajouter `typr_assert_type_deep`.
- `crates/typr-cli/src/cli.rs` — `--checked=deep` ou `--checked --deep`.

**Critères de fin :**
- `typr build --checked=deep` sur un projet record simple produit une assertion
  récursive.
- Test snapshot en mode deep (`crates/typr-core/tests/snapshot_tests.rs`).

**Risques :**
- Coût runtime × profondeur. Documenter que deep est encore moins fait pour la
  production que shallow.
- Les records contenant des `Foreign<T>` doivent passer sans assertion (même règle
  que shallow : `checked_descriptor` retourne `None` pour tout type résolvant
  `Foreign<T>`).

---

## Phase C — Lint optionnel `--lint-r`

### C.1 — `codetools::checkUsage` sur le R généré

**Objectif :** Passer chaque `R/*.R` généré dans un `Rscript -e 'parse(...);
codetools::checkUsage(...)'` pour attraper les variables libres et les fonctions
inexistantes dans le R émis. Fail-open si `Rscript` absent (même politique que les
cases `@run`).

**Fichiers à modifier :**
- `crates/typr-cli/src/project.rs` — nouvelle fonction `lint_r_code(r_files: &Path)`,
  appelée dans `build_project_impl` après la génération des `R/*.R`.
- `crates/typr-cli/src/cli.rs` — flag `--lint-r` sur `Commands::Build`,
  `Commands::Run`, `Commands::Check`.
- `crates/typr-cli/src/r_name_lint.rs` — peut-être fusionner les deux lints sous
  une même fonction `lint_generated_r`.

**Critères de fin :**
- `typr build --lint-r` sur un projet valide ne produit pas de faux positifs.
- L'introduction volontaire d'une variable libre dans le généré (via un test mock)
  est attrapée.

**Risques :**
- `codetools::checkUsage` peut être bavard sur du code S3 standard (variables
  utilisées via `UseMethod` qu'il ne voit pas). Prévoir des suppressions par
  commentaire ou config.
- Coût : chaque fichier R est parsé + analysé par R. Négligeable pour un projet
  typique mais à garder en tête.

---

## Backlog et dépendances

```
D.1 (RC/R6)         ──► D.2 (module boundary) ──► D.3 (generics)
                          │
                          └───────────────────────► D.4 (d/e/g/h)
                                                          │
                                                          └──► D.5 (S7)
                                                               └──► D.6 (closures)

B.1 (generator ext)  ──► B.2 (shrinker)  ──► (feedback vers D : plus de programmes
                                               générés avec Foreign<T> testés)

A.1 (deep checking)  ──► (peut être parallélisé après D.1)
C.1 (--lint-r)       ──► (indépendant de tout le reste)
```

**Recommandation d'ordre d'exécution :**

1. **D.1 d'abord** — le plus gros impact immédiat (RC et R6 sont les systèmes les plus
   fréquents après S3 dans l'écosystème R package). ~2-3 jours.
2. **D.2 ensuite** — gap structural connu et corrigeable. ~1 jour.
3. **D.3/D.4 en parallèle** — les colonnes j et d/g sont indépendantes. ~2-3 jours.
4. **B.1 en fond** — ajouter Lambda + Pipe (coût faible, ~1 jour) puis Module + Use
   (coût moyen, ~2 jours). Commencer après D.2 pour capitaliser sur la couverture
   Foreign<T> du générateur étendu.
5. **D.5/D.6** — les lignes manquantes (S7 nécessite d'abord d'installer le package).
   ~1 jour.
6. **A.1, C.1, B.2** — améliorations secondaires, à planifier selon les besoins.

**Métrique de progression :** `interop_matrix.md` se remplit. Chaque `—` restant est
un trou documenté. Chaque nouvelle case `cases/` est un pas vers la couverture
exhaustive des frontières interop.

**Métrique secondaire :** `typr fuzz --stats 256` → la couverture de variants `Lang`
et `Type` augmente à chaque incrément du générateur. Objectif final : 51/51 Lang,
30/30 Type.
