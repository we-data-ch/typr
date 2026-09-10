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

Le mécanisme est déjà en place, il reste à l'étendre et à l'inverser :

- **Noms non typés** : `crates/typr-cli/configs/src/functions_R.txt` (761 noms)
  → `.std_r.bin` (`build_function_list_vartype`, chaque nom = `UnknownFunction`).
- **Surcouche typée** : `crates/typr-cli/configs/std/*.ty` (`std_R.ty`,
  `default.ty`, `file.ty`, `option.ty`, `plot.ty`, `lin_alg.ty`, `system.ty`,
  `factor.ty`, `state.ty`, `ord.ty`, `foreign.ty`), signatures `@name: (T) -> U;`
  → `.std_r_typed.bin` (`build_typed_vartype`).
- **Commandes** : `typr std` (régénère les `.bin`) et `typr std doc` (émet le
  **SPG JSON-LD** de la stdlib via `build_stdlib_docs` → `build_spg_from_items`),
  dans `crates/typr-cli/src/standard_library.rs`.
- **Chargement compilateur** : `load_r()` / `load_typed_r()` dans
  `crates/typr-core/src/components/context/vartype.rs`, blobs bincode embarqués
  par `include_bytes!` dans `configs/bin/`.
- **Blacklist** : 60 noms refusés (`c`, `lapply`, `sapply`, `rep`, `str`,
  `cat`, `length`, …) dans `crates/typr-core/src/utils/standard_library.rs`
  (+ `not_in_blacklist` / `validate_vectorization`).
- **SPG** : `crates/typr-core/src/processes/spg/{model,builder,edges,doc_attach}.rs`
  — graphe JSON-LD (nœuds Type/Fonction/Module/Exemple, arêtes typées), déjà
  ciblé « outils IA ».
- **Introspection R existante** : `tools/gen_r_name_db.R` →
  `configs/src/r_name_db.json` (seed base-R committée), et
  `configs/src/introspect_pkg.R` (sondage S3/S4 générique sur machine user).

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

Étendre le SPG (section 6) avec des champs utiles au MCP, portés par une
convention de commentaires dans les `.ty` (`#!` pour les méta-annotations,
ou un bloc `@meta` adjacent) :

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

Champs minimal V1 : `pkg`, `tier`, `param` × n, `ret`, `coercion`/`note`,
`example` × n, `seealso`. V2 possible : `deprecated`, `alternative`,
`signature_r_officielle` (la signature R d'origine, pour traçage).

---

## 6. Étapes d'implémentation

### Phase 0 — Inventaire et estimation (livrable : `tools/` + catalogue vide)

1. **Boucler la première liste de candidats** depuis `functions_R.txt` (761 noms)
   + `r_name_db.json` (pkg, s3_generic). Outil : étendre `tools/gen_r_name_db.R`
   ou ajouter un script jumeau qui sort, pour chaque nom, pkg + S3/S4 + présence
   de `...` dans les args (via `formals`).
2. **Classer T1/T2/T3** (heuristique puis relecture humaine) :
   - *probable T1* : pur, arithmétique/distribution/stats simple, pas de `...`,
     pas de dispatch S3, pas de coercion surprenante (`abs`, `sqrt`, `log`,
     `sum`, `mean`, `nrow`, `ncol`, `length` est à débattre, …) ;
   - *probable T2* : `...`, variadique contraint, unions (`paste`, `match`,
     `rep` peut-être) ;
   - *T3 d'office* : les 60 de la blacklist + `do.call`, `with`, `eval`,
     subsetting `[`/`[[`/`$`.
3. **Créer `configs/std/base.ty`** (vide, squelette avec la convention de méta).

### Phase 1 — Étendre le format SPG (livrable : SPG enrichi, code compilable)

1. Étendre `crates/typr-core/src/processes/spg/model.rs` : champs optionnels
   `tier`, `params` (docs), `coercion_notes`, `examples`, `seealso` sur le nœud
   `Function`. **Rétro-compatible** : les champs nouveaux sont `Option<>`,
   l'émission sans méta reste identique → aucun test existant ne casse.
2. Étendre `doc_attach.rs` pour qu'il lise les annotations `#!` des `.ty`
   (attach par nom de fonction).
3. Mettre à jour `build_stdlib_docs()` (`standard_library.rs`) pour inclure les
   fichiers T2 dans le SPG **mais pas** dans `.std_r_typed.bin`.
4. Test : `typr std doc` émet un nœud par fonction T1+T2 avec méta ; `typr std`
   n'embarque que T1. Vérifier par un test Rust (les deux sorties, diff).

### Phase 2 — Rédiger le catalogue (base → stats → utils)

1. **base.ty V1** : reclasser finement les candidats de base, rédiger les 150–250
   entrées T1 (cible) + T2. **Objectif : aucune régression compilateur** — la
   fusion `load_typed_r` ne doit jamais produire de nouveau type-erreur sur les
   `cases/` et le suite de tests.
2. **stats.ty / utils.ty** ensuite, selon la même recette.
3. Conservation des fichiers existants : migrer `std_R.ty`/`default.ty`/… dans le
   nouveau format `#!` sans perdre les signatures déjà validées.
4. Circuit de validation à chaque lot :
   `cargo test --workspace` puis `typr case run` (aucun REGRESS) puis
   `typr std` + `typr std doc` sans SKIPPED non voulus.

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
   - `stdlib_declared_names()` ⊇ noms `.ty`, et réciproquement (dérivé, déjà
     le cas) ;
   - cohérence des tiers : un nom T3 dans `functions_R.txt` n'est dans aucun
     `.ty` T1 (inverse de la blacklist = erreur de build) ;
   - tous les `#! example:` sont compilables par `typr check` (isolation,
     pire cas : `noplayground`/skip pourquoi explicitement).
2. **Consigne noirelist ↔ catalogue** : un nom blacklisté qui « se typifie
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
| Surcharge de contexte MCP | Pack lookup-à-la-demande (ressource read-only), jamai inondé d'office |

---

## 10. Critères de succès

1. `typr std` + `typr std doc` régénèrent, sans SKIPPED ni erreur, des
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