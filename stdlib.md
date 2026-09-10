# 📘 RFC-STDLIB-0001 — Bibliothèque standard R typée : plan d'application

> **Résumé.** Construire un catalogue unique et curaté des fonctions de la
> standard library R (`base`, puis `stats`, `utils`, …), typées pour TypR,
> qui alimente **deux artefacts distincts** :
> 1. la doc technique consommée par le **MCP** (riche, tous les degrés de
>    confiance, jamais chargée par le compilateur) ;
> 2. la surcouche typée embarquée dans le compilateur (sous-ensemble
>    « haute confiance », seul à finir dans les `.bin`).
>
> La dépendance est inversée : **la stdlib compilée devient un sous-produit du
> catalogue de doc**, pas l'inverse.

---

## 1. 🎯 Objectif

Offrir aux utilisateurs (et au MCP) des signatures typées pour les fonctions
R classiques, pour ne pas avoir à écrire de signature ni mémoriser les types.
Deux bénéficiaires, deux tolérances différentes :

| Bénéficiaire | A besoin de | Tolérance à l'erreur |
|---|---|---|
| **MCP / doc** | signatures + sémantique (params, exemples, règles de coercion, pièges) | faible — un mauvais exemple induit en erreur |
| **Compilateur** | signatures *vérifiables* uniquement | très faible — une signature fausse produit des erreurs de type déroutantes ou des casts R erronés |

**Le risque principal n'est jamais la mémoire — c'est la soundness.**
Une signature fausse est pire que pas de signature.

---

## 2. État des lieux (ce qui existe déjà)

Le mécanisme est en place et fonctionnel. Voici l'état après Phase 0 + 1 + 2 (migration) :

### 2.1. Inventaire

- **Noms non typés** : `crates/typr-cli/configs/src/functions_R.txt` (761 noms)
  → `.std_r.bin` (`build_function_list_vartype`, chaque nom = `UnknownFunction`).
- **Introspection R** : `configs/src/r_name_db.json` (1283 noms, seed base-R,
  pkg, s3_generic, s4_generic).

### 2.2. Catalogue de signatures (fichiers `.ty`)

Tous les fichiers `.ty` portent désormais des annotations `#!` (pkg, tier,
param, ret, coercion, example, seealso) conformes au format RFC.

**Sink B — compilateur** (`R_T1_SOURCES`, → `.std_r_typed.bin`) :

| Fichier | Signatures `@` | Let bindings | Description |
|---|---|---|---|
| `std_R.ty` | 29 | 0 | Opérateurs (+,-,*,/,%%,&&,\|\|), sum, print, reduce/fold/extend, testing, interop |
| `default.ty` | 20 | 0 | TypR-specific : as_vec, map, filter, set_at, add/minus/mul/div, get, seq, join, startsWith, endsWith, contains |
| `file.ty` | 11 | 0 | Système de fichiers (getwd, setwd, dir, file__exists, ...) |
| `option.ty` | 3 | 5 | Option<T> type + unwrap/expect/is_some/is_none |
| `factor.ty` | 6 | 0 | Factor<L> opaque + factor/levels/nlevels |
| `state.ty` | 7 | 0 | State<T> mutable cell + get/set/update/map/derive |
| `ord.ty` | 2 | 0 | Eq/Ord interfaces + unique/sort |
| `lin_alg.ty` | 2 | 2 | dot, t, lvec, cvec |
| `plot.ty` | 1 | 2 | plot signature + Plot type (⚠️ SKIPPED parser — type record) |
| `system.ty` | 3 | 0 | system2, bsystem2, exec (⚠️ SKIPPED parser — type record) |
| `foreign.ty` | 0 | 0 | Foreign<T> opaque type |

**Sink A — MCP/doc uniquement** (`R_DOC_ONLY_SOURCES`) :

| Fichier | Signatures `@` | Description |
|---|---|---|
| `base.ty` | 128 | Fonctions R base (math, stats, comparaison, string, set, matrix, vector) — **doc SPG seulement** |
| `stats.ty` | 103 | Fonctions du package stats (dnorm…rhyper, t.test, cor.test, lm, …) — **doc SPG seulement** |
| `utils.ty` | 55 | Fonctions du package utils (read.csv, write.csv, head, …) — **doc SPG seulement** |

> ⚠️ **Contrainte architecturale** : `base.ty` est dans `R_DOC_ONLY_SOURCES`
> (pas `R_T1_SOURCES`) car `stdlib_declared_names()` ne doit contenir que les
> fonctions **TypR-propriété** (celles pour lesquelles TypR fournit sa propre
> implémentation dans `std.R` ou la codegen du transpileur). Les fonctions R
> base (`abs`, `sqrt`, `mean`, etc.) ne sont pas TypR-propriété — TypR ne
> fait que les annoter en types pour la doc MCP. Les inclure dans
> `stdlib_declared_names()` casserait le test
> `stdlib_declared_names_covers_the_bundled_ty_signatures`.

### 2.3. Infrastructure SPG (Phase 1 ✅)

- **`StdlibMeta`** (`model.rs`) : champs optionnels `tier`, `param_docs`,
  `ret_doc`, `coercion_notes`, `examples`, `seealso`, `pkg` — tous `Option<>`,
  rétro-compatibles.
- **`stdlib_meta.rs`** : parser `parse_meta_from_source()` — accumulate les
  blocs `#!` jusqu'à la ligne `@name:`, puis attache au nom. 8 tests unitaires.
- **`doc_attach.rs`** : lit les annotations `#!` des `.ty` (attach par nom).
- **`build_stdlib_docs()`** : chaîne `R_T1_SOURCES` + `R_DOC_ONLY_SOURCES`
  → SPG enrichi avec méta.
- **Tests** : 7 tests `standard_library` + 8 tests `stdlib_meta` = 15/15 ✅.

### 2.4. Commandes

- `typr std` : régénère `.std_r.bin` + `.std_r_typed.bin` + `.std_js.bin` +
  `.std_js_typed.bin`.
- `typr std doc` : émet le SPG JSON-LD enrichi (T1+T2, avec méta).

### 2.5. Chargement compilateur

- `load_r()` / `load_typed_r()` dans `crates/typr-core/src/components/context/vartype.rs`,
  blobs bincode embarqués par `include_bytes!` dans `configs/bin/`.

### 2.6. Blacklist

- 60 noms refusés (`c`, `lapply`, `sapply`, `rep`, `str`, `cat`, `length`, …)
  dans `crates/typr-core/src/utils/standard_library.rs`
  (+ `not_in_blacklist` / `validate_vectorization`).

### 2.7. SKIPPED pré-existants

- `plot.ty` et `system.ty` sont SKIPPED par `typr std` car le parser ne
  supporte pas encore les déclarations `type` (record type aliases). Ceci est
  pré-existant et non introduit par les changements Phase 2.

> ⚠️ **Constat clé** : la blacklist n'existe pas pour la mémoire mais pour la
> soundness. `c()`, `lapply()`… sont intrinsèquement non typables fidèlement
> en base R (coercion, recycling, `...`, dispatch S3). Tout le plan doit
> préserver ce principe.

---

## 3. Principes directeurs

1. **Une seule source de vérité** : le catalogue (fichiers `.ty` enrichis,
   un par package). Rien n'est obtenu ailleurs — ni les `.bin`, ni le SPG doc.
2. **Deux sinks, deux exigences** :
   - Sink A — **MCP/doc** : consomme *tout* (T1 + T2), enrichi (exemples,
     notes de coercion, relations). Jamais chargé par le compilateur.
   - Sink B — **compilateur** : ne charge que le sous-ensemble **T1**
     (haute confiance) dans `.std_*_typed.bin`.
3. **Dépendance inversée** : on ne dérive pas la doc de la stdlib compilée ;
   on dérive la stdlib compilée du catalogue de doc.
4. **Soundness over coverage** : en cas de doute → `UnknownFunction` + pas
   d'erreur (l'utilisateur n'est jamais forcé d'écrire une signature, mais on
   ne ment jamais sur un type).
5. **Le compilateur est l'oracle du MCP** : la doc réduit l'espace de recherche
   du LLM, `typr check` reste l'arbitre final (philosophie déjà appliquée en CI
   par `npm run check:examples`).
6. **TypR-propriété vs R base** : seules les fonctions pour lesquelles TypR
   fournit sa propre implémentation (std.R, codegen transpileur) entrent dans
   `stdlib_declared_names()`. Les fonctions R base annotées en types vont
   dans `R_DOC_ONLY_SOURCES` (doc SPG uniquement).

---

## 4. Architecture cible

```
                    CATALOGUE (source de vérité)
      configs/std/<package>.ty   — signatures @ + métadonnées
      (base.ty, stats.ty, utils.ty, …)   TIERs T1/T2/T3
                              │
              ┌───────────────┴────────────────┐
              ▼                                ▼
       Sink A — MCP/doc                  Sink B — compilateur
       typr std doc                       typr std
       SPG JSON-LD enrichi                .std_r_typed.bin  (T1 seul)
       (T1+T2, exemples, pièges)          .std_r.bin  (UnknownFunction, T3)
              │                                ▼
              ▼                          vartype.rs load_typed_r()
       digest markdown/JSON                  │
       → ressource MCP (read-only)           ▼
              │                          Context typechecking
              └──► MCP propose avec la doc, VÉRIFIE avec `typr check`
```

Règles de flux :

| Symbole | Dans `functions_*.txt` | Dans `.ty` T1 | Dans `.ty` T2 | Doc MCP |
|---|---|---|---|---|
| pur, typable (ex. `abs`, `sqrt`, `nrow`) | oui | oui | — | oui |
| typable mais souple (ex. `paste` T2, unions/`Any`) | oui | non | oui | oui |
| non typable (ex. `c`, `lapply`, `with`) | oui | non | non | oui, signale « à typer soi-même » |

- Le **compilateur** ne voit jamais T2 ni T3 : on garde zéro coût mémoire pour
  les tiers 2–3 (un symbole T2/T3 = `UnknownFunction`, pas de blob en plus).
- Le **MCP** voit tout, y compris T3, avec l'avertissement approprié.

### Sources en pratique

| Constante | Fichiers | Consommé par |
|---|---|---|
| `R_T1_SOURCES` | std_R.ty, default.ty, file.ty, option.ty, lin_alg.ty, factor.ty, state.ty, ord.ty | `typr std` (compilateur) + `typr std doc` (SPG) |
| `R_DOC_ONLY_SOURCES` | base.ty | `typr std doc` (SPG uniquement) |
| `R_T1_SOURCES` (⚠️ SKIPPED) | plot.ty, system.ty | parser ne supporte pas `type` record |

---

## 5. Le catalogue — format

### 5.1. Organisation

Un fichier `.ty` par package R : `base.ty`, `stats.ty`, `utils.ty`, puis
`graphics.ty`, `grDevices.ty`, `methods.ty` (S4, faible priorité), enfin
`datasets.ty`. On s'inspire de la structure existante (`configs/std/*.ty`).

### 5.2. Signature

Réutiliser la syntaxe existante `@name: (T) -> U;` — les pipeline et tests
(`preprocess_ty_source`, `build_typed_vartype`) fonctionnent déjà avec.

### 5.3. Métadonnées par entrée

Format `#!` implémenté et testé. Chaque bloc `#!` précède la ligne `@name:` :

```
#! pkg: base
#! tier: T1
#! param x: Valeurs à sommer, NA exclues si na.rm = true
#! ret: NA si x vide et na.rm = false
#! coercion: logical/int -> num par R (TypR: exiger num explicitement ?)
#! example: sum(c(1, 2, 3))         # -> 6
#! example: sum(c(1, NA), na.rm = true)   # -> 1 (décisif : comportement NA)
#! seealso: cumsum, prod, rowSums
@sum: (vec[N, num]) -> num;
```

Champs disponibles : `pkg`, `tier`, `param` × n, `ret`, `coercion`/`note`,
`example` × n, `seealso`. Le parser (`stdlib_meta.rs`) gère les formes
compactes `#! key:value` et `#! key value`.

---

## 6. Étapes d'implémentation

### Phase 0 — Inventaire et estimation ✅

1. **Boucler la première liste de candidats** depuis `functions_R.txt` (761 noms)
   + `r_name_db.json` (pkg, s3_generic). 1283 noms base-R identifiés.
2. **Classer T1/T2/T3** — classification heuristique disponible via
   `r_name_db.json` (s3_generic, s4_generic) + liste noire 60 noms.
3. **Créer `configs/std/base.ty`** — squelette vide avec conventions `#!` ✅.

### Phase 1 — Étendre le format SPG ✅

1. `model.rs` : `StdlibMeta` avec tous les champs `Option<>` ✅.
2. `stdlib_meta.rs` : parser `parse_meta_from_source()` avec 8 tests ✅.
3. `doc_attach.rs` : lit les annotations `#!` ✅.
4. `build_stdlib_docs()` : chaîne T1+T2, SPG enrichi ✅.
5. Tests : `spg_nodes_carry_stdlib_meta_end_to_end`,
   `doc_only_sources_are_in_spg_but_not_in_typed_bin` ✅.

### Phase 2 — Rédiger le catalogue (base → stats → utils) 🔄

**Migration des signatures existantes** ✅ :
- Tous les fichiers `.ty` (std_R.ty, default.ty, option.ty, factor.ty,
  state.ty, file.ty, system.ty, plot.ty, lin_alg.ty, ord.ty, foreign.ty)
  portent désormais des annotations `#!` complètes.
- 10 doublons inter-fichiers identifiés et résolus (substr, sub, gsub,
  strsplit, tolower, toupper, grepl → base.ty ; seq → default.ty ;
  unique, sort → ord.ty).

**Catalogue R base** ✅ :
- `base.ty` : 128 signatures R base (math, stats, comparaison, string, set,
  matrix, vector, type-checking, type-coercion) avec annotations `#!`
  complètes (pkg, tier, param, ret, coercion, example, seealso).
- `stats.ty` : 103 signatures du package stats (dnorm…rhyper, pwilcox,
  t.test, cor.test, IQR, …), annotations `#!` complètes.
- `utils.ty` : 55 signatures du package utils (read.csv, write.csv, head,
  adist, …), annotations `#!` complètes.
- Positionnement `R_DOC_ONLY_SOURCES` (doc SPG uniquement, pas compilateur).

> **Noms pointés / variadiques** : les signatures dont le nom de fonction
> contient un `.` (`read.csv`, `cor.test`, …) sont quantifiées par des
> backticks (``@`read.csv` : …``) et les paramètres variadiques `...name:`
> sont conservés tels quels — le préprocesseur ne strip que les vrais noms de
> paramètres (`na.rm`, `row.names`), jamais le marqueur variadique. Les types
> 3D (`[#N, #M, int]`) et les listes `[#, T]` sont ré-écrits sous forme
> imbriquée `[#N, [#M, int]]` (le parseur les rejette sinon, et une ligne
> rejetée empoisonne silencieusement tout ce qui suit dans le fichier).

**Reste à faire** :
- [x] `stats.ty` : fonctions du package stats (rnorm, dnorm, t.test, lm, …)
- [x] `utils.ty` : fonctions du package utils (read.csv, write.csv, …)
- [ ] Validation `typr case run` (aucun REGRESS)
- [ ] Circuit de validation : `cargo test --workspace` + `typr std` + `typr std doc`

> Garde-fou : `build_typed_vartype` **SKIP** silencieusement tout fichier qui
> panique (standard_library.rs). Un `.ty` trop ambitieux qui fait paniquer
> le parseur ⇒ entrées absentes du binaire ⇒ régression silencieuse. Toujours
> traiter un SKIPPED comme un bug critique dans ce circuit.

### Phase 3 — Pack MCP

1. **Digest compact** : nouveau sous-commande `typr std doc --format md` (ou
   renderer SPG→markdown, dans le style de `Rd_doc.md`). Sortie : un digeste
   groupable par catégorie (limit ~10–20 Ko par paquet, hiérarchisé
   `### nom(pkg): params → type` + 1 ligne doc + 1 exemple + badge T1/T2/T3).
2. **Fichiers de référence** : `typr std doc --format md --output stdlib-base.md`,
   ou un `stdlib.pack.json` versionné, consommé par le MCP comme ressource
   **lecture seule** (pas de contexte chargé d'office ; lookup par outil/ressource
   à la demande → zéro surcharge de contexte).
3. **Oracle** : exposer `typr check` / `typr build` comme outils du MCP pour
   vérifier « signé ou pas » à la volée, au lieu de faire confiance à la doc.
4. **Vérification des exemples** : les `#! example:` du catalogue passent dans
   un circuit de validation (cf. Phase 4) — jamais de faux exemple dans la doc.

### Phase 4 — Circuit de maintenance (anti-régression)

1. **Tests Rust** dans `crates/typr-cli` (suite `standard_library.rs`) :
   - toute signature `@…` du catalogue **parse et type-check** (zéro SKIPPED) ;
   - `stdlib_declared_names()` ⊇ noms `.ty` TypR-propriété (pas R base) ;
   - cohérence des tiers : un nom T3 dans `functions_R.txt` n'est dans aucun
     `.ty` T1 (inverse de la blacklist = erreur de build) ;
   - tous les `#! example:` sont compilables par `typr check` (isolation,
     pire cas : `noplayground`/skip pourquoi explicitement).
2. **Consigne blacklist ↔ catalogue** : un nom blacklisté qui « se typifie
   un jour » doit passer par un audit explicite (déblacklister = décision,
   pas accident).
3. **CI** : `cases` job étendu ou nouveau job `stdlib` compact (génération +
   tests Phase 1/4). Rien n'est committé sans régénération des `.bin`.

### Phase 5 — (Optionnel, plus tard) Packages externes

Même mécanisme pour `dplyr`, `ggplot2`, `stringr`, … via
`configs/std/ext/dplyr.ty` et la résolution `@extern pkg::name:` déjà supportée
(`stdlib_declared_names`). La doc MCP s'étend, le compilateur reste à T1
seulement, et pour les packages externes on exige une **preuve** (test case
`cases/`) par entrée T1 promue.

---

## 7. Porte de sortie T2 → T1 (checklist de promotion)

Une fonction **T2 ne monte en T1** que si toutes les cases sont cochées :

- [ ] comportement pur et stable (pas de dispatch S3 variable sur args) ;
- [ ] pas de `...`, ou variadique strictement contraint et vérifiable ;
- [ ] pas de coercion silencieuse entre types TypR distincts ;
- [ ] comportement `NA`/`NULL`/vide documenté (`na.rm`, empreinte `vec[0]`) ;
- [ ] au moins un `#! example:` qui verrouille un piège (NA, recycling…) ;
- [ ] le compilateur, avec la signature, ne crée aucune erreur nouvelle sur
      `cases/` + suite complète `cargo test --workspace`.

Promouvoir sans passer la porte = régression soundness.

---

## 8. Réponse au « risque mémoire »

Quantifié : une entrée typée ≈ 100–300 octets bincode. Même 800 fonctions T1 ≈
**quelques centaines de Ko au pire**, chargées une fois via `Arc` et partagées —
négligeable en natif comme en WASM (le blob `.std_r_typed.bin` fait déjà ça).

Pour minimiser malgré tout :
- T2/T3 ne pèsent **rien** dans le compilateur (jamais embarqués) ;
- la surcouche T1 reste un blob séparé → on peut la passer en chargement
  **paresseux** plus tard (désérialisation à la demande par symbole) sans
  changer le format ;
- les `<package>.ty` ne sont jamais embarqués dans le binaire : ils sont
  compilés en `.bin` une seule fois (`typr std`), comme aujourd'hui.

Donc : **le poids n'est pas un argument pour restreindre la couverture —
la soundness l'est.**

---

## 9. Risques & mitigations

| Risque | Mitigation |
|---|---|
| Signature fausse → erreurs de type / casts erronés | Porte de promotion T1 (section 7), blacklist maintenue |
| `.ty` qui panique → entrées absentes du binaire | SKIPPED = erreur critique ; test « zéro SKIPPED » en CI |
| Exemple faux dans la doc MCP | Circuit `#! example:` → `typr check` en CI (Phase 4) |
| Régression suite `cases/` | `typr case run` bloque ; toute promotion vérifie zéro REGRESS |
| Dérive catalogue ↔ code compilé | `.bin` régénérés et committés à chaque changement ; tests de cohérence |
| Surcharge de contexte MCP | Pack lookup-à-la-demande (ressource read-only), jamais inondé d'office |
| Fonctions R base dans `stdlib_declared_names()` | `base.ty` dans `R_DOC_ONLY_SOURCES`, exclu de `stdlib_declared_names()` |

---

## 10. Critères de succès

1. `typr std` + `typr std doc` régénèrent, sans SKIPPED non voulus, des
   artefacts qui passent `cargo test --workspace` et `typr case run`.
2. Un utilisateur peut écrire `sum(x)` / `mean(x, na.rm = true)` **sans
   signature** et le compilateur infère `num` (par exemple), avec une erreur
   claire si l'argument est manifestement mal typé.
3. Le MCP, face à « comment fait-on X en TypR ? », répond correctement en
   s'appuyant sur le digest et **vérifie** son code par `typr check` (oracle).
4. Zéro régression de soundness : la suite `cases/` ne comporte aucun REGRESS
   à la fin de chaque phase.
5. Le catalogue couvre base (T1+T2) puis s'étend à stats/utils sans nouveau
   coût mémoire compilateur.

---

## 11. Hors scope (V1)

- Typage « parfait » de `c()` et des foncteurs dynamiques (`lapply`, `do.call`) —
  restent en `UnknownFunction` + note doc.
- S4 `methods` complet.
- Génération de la doc **site** (`typr.github.io`) depuis le catalogue — c'est
  un renderer de plus sur le SPG, à faire après le pack MCP.
- Chargement paresseux du blob T1 (optimisation, future).
- Support du `type` (record type aliases) dans le parser — prerequisite pour
  débloquer `plot.ty` et `system.ty` (SKIPPED actuellement).
