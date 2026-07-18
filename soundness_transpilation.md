# Soundness de transpilation — attraper les edge cases « TypR valide → R invalide »

**Statut : Phase A implémentée (2026-07-18) — voir CLAUDE.md § Runtime Soundness Checks.
Phase B Stage 1 ET Stage 2 implémentées (2026-07-18) — voir CLAUDE.md § Generative Testing
(Phase B). Phase C implémentée (2026-07-18) — voir CLAUDE.md § Static Base-R/S4 Name Lint
(Phase C). Phase D v1 implémentée (2026-07-18) — voir `interop_matrix.md` : grille
S3/S4/RC/R6/S7/base + `cases/0018`-`0029` (12 cases, rows {S3, S4, base-attrs} ×
columns {argument, retour, `let` annoté, accès `.`/`$`}). Construire la toute première
case a immédiatement fait remonter 4 bugs réels bloquant l'usage documenté de
`Foreign<T>`/`@extern` (arité d'alias opaque, `@extern` shadowé par son propre stub
`UseMethod`, corruption de classe S4 via `as.X`/`struct()`, dispatch S3 inatteignable
sur paramètre `Foreign<T>`) — tous corrigés, détail dans `interop_matrix.md`. Le reste
de la grille (RC/R6, colonnes d/e/g/h/i/j, ligne S7) reste un backlog priorisé.**

## Le problème

TypR garantit statiquement que le code source est bien typé, puis le monomorphise en R basé
sur S3. La garantie implicite « TypR bien typé ⇒ R généré valide » n'est aujourd'hui vérifiée
que ponctuellement : snapshots (`crates/typr-core/tests/snapshot_tests.rs`), tests `lab/`,
catalogue `cases/`. Trois familles d'edge cases échappent à cette couverture :

1. **Collisions avec les autres philosophies objet de R** — S4, R6, RC, S7, R « pur ». Exemple
   historique : le bug `nlevels` (shadower une fonction base-R *non générique* sans `.default`
   de repli, cf. la règle dure dans CLAUDE.md). Un stub `UseMethod` qui écrase un générique S4
   casse silencieusement tout le dispatch S4 d'un package attaché.
2. **Le système de modules** — TypR compile les modules en environnements R via `local()` ;
   la visibilité S3 à travers cette frontière a déjà produit des bugs (case 0002 : méthode S3
   `@pub` invisible depuis un autre fichier).
3. **Les valeurs construites à la volée** — `struct(c(...))`, casts de `types.R`,
   constructeurs de records : un class vector faux est invisible dans le texte R généré et
   explose trois appels plus loin avec une erreur S3 cryptique.

Ce document décrit un système en quatre phases, dans l'ordre d'implémentation :

| Phase | Nom                                            | Nature                | Effort estimé |
|-------|------------------------------------------------|-----------------------|---------------|
| **A** | Mode `--checked` : contrats de types runtime   | Oracle sémantique     | moyen         |
| **B** | Génération de programmes dirigée par les types | Fuzzing bien typé     | gros          |
| **C** | Oracle statique : base de noms base-R/S4       | Lint à la compilation | petit         |
| **D** | Matrice d'interop S4/R6/RC/S7/base             | Suite de conformité   | incrémental   |

A est prérequis de B (le générateur a besoin d'un oracle). C est indépendant et petit.
D se remplit au fil de l'eau.

---

## Phase A — Mode `--checked` : contrats de types au runtime

### Objectif

`typr build --checked` (et `typr run --checked`) émet, à chaque **frontière typée** du code
généré, une assertion R dérivée mécaniquement du type statique. Tout écart entre le type
statique et la classe runtime devient une erreur immédiate, localisée à la frontière exacte —
au lieu d'un `no applicable method` trois appels plus loin.

Ce mode n'est **jamais** destiné à la production : c'est un oracle pour les tests. Son gros
levier : chaque test `lab/` et chaque case Phase 2 (`layer = "r-run"`) existant devient un
oracle sémantique gratuit dès qu'on le relance sous `--checked`.

### Les frontières instrumentées (v1)

On instrumente uniquement les points où le type est **déclaré dans l'AST** — pas besoin d'un
AST annoté par l'inférence, le transpileur a déjà tout sous la main :

| Frontière | Nœud `Lang` | Ce qu'on vérifie |
|---|---|---|
| `let x: T <- expr` | `Lang::Let` avec annotation | la valeur liée est un `T` |
| Entrée de fonction | `Lang::Function` (params typés) | chaque argument reçu est du type du param |
| Sortie de fonction | `Lang::Function` (type de retour) | la valeur retournée est du type déclaré |
| Retour de constructeur | `Lang::ConstructorCall` | le class vector produit est le bon |
| Cast explicite | `as` / `as!` | le résultat du cast satisfait le type cible |

Les `let` non annotés, les expressions intermédiaires et les lambdas non typées ne sont
**pas** instrumentés en v1 — le but est la frontière statique↔runtime, pas une trace
exhaustive.

### Forme générée

```r
# let p: Point <- f(x);          →  (mode normal)
p <- f(x)
# let p: Point <- f(x);          →  (mode --checked)
p <- typr_assert_type(f(x), c("Point", "list"), "main.ty:12", "let p")

# fn(a: int, b: Point): Point { ... }   →  (mode --checked, prologue + retour)
name.default <- function(a, b) {
  typr_assert_type(a, "integer", "main.ty:3", "param a")
  typr_assert_type(b, c("Point", "list"), "main.ty:3", "param b")
  typr_assert_type({ <corps> }, c("Point", "list"), "main.ty:3", "return")
}
```

Le runtime R (une seule fonction, ~30 lignes) vit dans `configs/src/std.R` :

```r
typr_assert_type <- function(x, expected, loc, what) {
  ok <- if (is.character(expected) && length(expected) > 1L) {
    inherits(x, expected[1L])              # types nommés : tête du class vector
  } else {
    switch(expected,
      integer   = is.integer(x) || (is.numeric(x) && all(x == trunc(x))),
      double    = is.double(x),
      character = is.character(x),
      logical   = is.logical(x),
      list      = is.list(x),
      inherits(x, expected))
  }
  if (!ok) stop(sprintf(
    "[typr --checked] %s at %s: expected %s, got class <%s> (typeof %s)",
    what, loc, paste(expected, collapse = "/"), paste(class(x), collapse = ","),
    typeof(x)), call. = FALSE)
  x
}
```

> ⚠️ Règle `std.R` (CLAUDE.md) : `generic_functions.R` clobbe tout nom de fonction stdlib
> **typée**. `typr_assert_type` n'a pas de signature `.ty` — c'est un helper interne, comme
> `typr_spread_record` (`configs/src/std.R:45`), donc un nom nu est sûr ici. Ne jamais lui
> ajouter de `@signature:` sans le renommer en `.default`.

**Dérivation du descripteur** : côté Rust, le descripteur `expected` se dérive du `Type` via
l'infrastructure déjà utilisée par les casts de `types.R` — `Context::get_classes`
(`components/context/mod.rs:793`, class vector complet ordonné pour S3) et
`Context::get_class` (`:681`). Pour les primitifs on émet la chaîne `typeof` simple. Les cas
sans vérification possible sont émis comme pass-through (pas d'assertion) : `Any`, les
génériques non résolus, les types fonction (v1 vérifie juste `is.function`), les interfaces
(structurelles, pas de classe runtime fiable — v1 : no-op).

**Profondeur** : v1 est **shallow** (class vector / typeof uniquement). Une v2 `--checked=deep`
pourrait vérifier récursivement les champs des records et échantillonner les éléments des
arrays — à ne faire que si la v1 laisse passer des bugs réels.

### Plan d'implémentation

1. **Flag de config** — ajouter `pub checked_mode: bool` (avec `#[serde(default)]`) à `Config`
   (`components/context/config.rs:62`), calqué exactement sur `test_mode` (`:73`) :
   `Context::set_checked_mode` / `get_checked_mode` à côté de `set_test_mode`
   (`components/context/mod.rs:250`).
2. **CLI** — flag `--checked` sur `Commands::Build` et `Commands::Run`
   (`crates/typr-cli/src/cli.rs:46`), threadé dans `build_project` →
   `build_project_impl` (`project.rs:826-852`) jusqu'au `Context::default().set_checked_mode(...)`.
3. **⚠️ Cache incrémental** — `checked` doit invalider le cache exactement comme `test_mode` :
   l'intégrer à `BuildManifest::is_compatible` (`project.rs:841`) **et** à
   `cache::module_cache_salt` (`project.rs:870`). Sinon un build warm mélange du R checked et
   non-checked. Test d'intégration à ajouter dans `crates/typr-cli/tests/incremental_builds.rs`.
4. **Runtime** — `typr_assert_type` dans `configs/src/std.R` (helper nu, cf. avertissement
   ci-dessus). Il est présent dans tous les builds mais n'est *appelé* que par du code généré
   en mode checked — pas besoin de version conditionnelle de `std.R`.
5. **Émission** — dans `processes/transpiling/mod.rs`, aux arms `Lang::Let`,
   `Lang::Function`, `Lang::ConstructorCall` : si `context.get_checked_mode()`, wrapper
   l'expression émise. Extraire la logique dans un fichier satellite
   `processes/transpiling/checked_assertions.rs` (convention CLAUDE.md : un fichier par
   feature, un `pub fn`, arm délégant — ce serait la **première** extraction du god-file
   `transpiling/mod.rs`, autant établir le pattern proprement). Signature type :
   `pub fn wrap_checked(context: &Context, r_expr: String, typ: &Type, loc: &HelpData, what: &str) -> String`
   qui retourne `r_expr` inchangé si le mode est off ou le type invérifiable.
6. **Localisation** — `loc` vient du `HelpData` déjà porté par chaque nœud `Lang`.
7. **Déterminisme** — les descripteurs passent par `get_classes` (déjà déterministe depuis le
   fix `Graph::get_supertypes`) ; ne rien introduire qui itère un `HashMap`/`HashSet` (règle
   CLAUDE.md sur le R rendu).

### Critères de fin

- `typr build --checked` sur un projet `typr new` de démo produit un R qui s'exécute à
  l'identique du mode normal (les assertions passent toutes).
- Un bug injecté à la main (constructeur émettant un mauvais class vector) est attrapé avec
  un message localisé.
- Test snapshot d'un programme représentatif en mode checked (fixe la forme émise).
- `nu debug.nu --check` relancé avec checked activé : zéro assertion déclenchée sur `lab/`
  (ou : chaque assertion déclenchée = un bug réel à cataloguer dans `cases/` — c'est le
  premier dividende de la phase).
- Doc : section dans CLAUDE.md + `typr build --help`.

### Risques / pièges connus

- **Faux positifs** sur les valeurs volontairement polymorphes (union types : la valeur porte
  la classe d'une *variante*, pas de l'union). Règle v1 : pour un type union, `expected` est
  la liste des classes canoniques des variantes (`c("V","U","Tag","list")`, cf. pipeline des
  variantes d'union) et on accepte si `inherits` matche l'une d'elles.
- **`int` vs `double`** : R ne distingue pas toujours (`5` est un double). L'assertion
  `integer` doit accepter un double à valeur entière (cf. le `switch` ci-dessus), sinon la
  moitié des programmes valides échouent.
- **Coût runtime** : négligeable pour des tests ; documenter que `--checked` n'est pas un mode
  de production.

---

## Phase B — Génération de programmes dirigée par les types

> **Stage 1 implémenté (2026-07-18)** — voir CLAUDE.md § Generative Testing (Phase B Stage 1)
> pour l'état exact et deux déviations volontaires par rapport à la description ci-dessous :
> pas de dépendance `proptest` (générateur "à la main" sur `rand`, déjà présent dans le
> workspace et inutilisé jusqu'ici), et l'oracle utilise `TypeChecker` directement plutôt que
> `FluentParser` (`FluentParser::type_next()` avale silencieusement les erreurs de typage via
> `TypeContext::to_tuple()` — aurait rendu l'assertion « type-checks sans erreur » vide de
> sens). Stage 1 a immédiatement trouvé un vrai bug de type-checking
> (`cases/0017-char-if-widening`, encore ouvert).
>
> **Stage 2 implémenté (2026-07-18)** — `typr fuzz run/stats/promote` (`crates/typr-cli/src/
> fuzz.rs`). Réutilise le même générateur via le feature `fuzz-gen` de typr-core (activé
> normalement pour typr-cli, pas seulement en dev-dependency). Chaque itération lance `typr run
> --checked` **en sous-processus** (`current_exe()`, même pattern que `cases.rs::build_sandbox`
> — nécessaire car `run_project`/`build_project_impl` font `std::process::exit` sur échec, ce
> qui tuerait la boucle en place). Classification par le texte combiné stdout+stderr : succès →
> `Pass` ; contient `"[typr --checked]"` → `CheckedAssertionFailure` (la vraie cible) ; sinon →
> `OtherFailure`. Pas de shrinking en v1 (le générateur produit déjà des programmes courts).
> Échecs persistés dans `fuzz_failures/<hash>/TypR/main.ty` (hash de contenu, dédup entre runs),
> directement curate-copyables par `typr case add --from` (déjà existant, pas de changement
> requis) — `typr fuzz promote <hash>` fait ça pour toi. **220 exécutions R réelles lancées
> pendant l'implémentation, zéro échec** — cohérent avec le fait que Stage 1 filtre déjà
> agressivement en amont avec le même générateur ; la pipeline persist/promote a été vérifiée
> manuellement avec un échec synthétique plutôt qu'un vrai (voir CLAUDE.md § Generative Testing
> pour le détail). Pas branché en CI par défaut, conforme au plan.

### Objectif

La seule approche réellement « exhaustive » : générer des programmes TypR **bien typés par
construction** (technique QuickCheck/Csmith), les faire traverser tout le pipeline, et
utiliser le mode checked de la phase A comme oracle sémantique. Tout échec à n'importe quel
étage est un bug attrapé :

```
générer (bien typé par construction)
  → `typr check` doit passer        sinon: bug du générateur OU du type-checker
  → transpiler sans panic           sinon: bug de transpilation (crash)
  → Rscript sous --checked          sinon: bug de soundness (le cœur de la cible)
  → shrink → cas minimal → `cases/`
```

### Architecture

Deux étages, car l'exécution R ne peut pas tourner dans les tests unitaires de typr-core :

- **Générateur** : module pur dans typr-core — `crates/typr-core/src/utils/program_gen.rs`
  (sous `#[cfg(any(test, feature = "fuzz"))]` ou compilé normalement avec les `allow` usuels).
  Dépendance : `proptest` en `dev-dependency` de typr-core, **et** en dépendance normale de
  typr-cli (pour l'étage 2). Proptest fournit le shrinking et la persistance des seeds ; on
  n'utilise pas `Arbitrary` dérivé (il produirait des AST mal typés) mais des `Strategy`
  manuelles dirigées par les types.
- **Étage 1 (rapide, sans R, CI)** — test proptest dans typr-core :
  `crates/typr-core/tests/generative.rs`. Propriété : tout programme généré parse
  (`parse_from_string`), type-check **sans erreur**, transpile **sans panic**. Attrape les
  bugs de crash et les incohérences générateur↔type-checker. Tourne à chaque `cargo test`.
- **Étage 2 (avec R, opt-in)** — sous-commande `typr fuzz` dans typr-cli
  (`crates/typr-cli/src/fuzz.rs`), symétrique de `typr case` : génère N programmes, écrit
  chacun dans un projet temporaire (même mécanique de replay que `cases.rs` — copie temp +
  `current_exe`), `typr build --checked` + exécution `Rscript`, collecte les échecs. Fail-open
  si `Rscript` absent (comme les cases `@run`). Pas dans le CI par défaut ; lancement manuel
  ou job nightly.

### Le générateur dirigé par les types

Principe : on ne génère jamais une expression puis on espère qu'elle type — on tire un **type
but** puis on construit une expression *de ce type*, en maintenant un environnement des
bindings générés.

```rust
struct GenEnv {
    vars: Vec<(String, Type)>,      // bindings `let` déjà émis, utilisables
    aliases: Vec<(String, Type)>,   // records/unions/opaque déclarés
    depth: u32,                     // budget de récursion
}

// Cœur : produire une expression du type demandé.
fn gen_expr(env: &GenEnv, goal: &Type, depth: u32) -> BoxedStrategy<String>
```

Productions pour un `goal` donné (pondérées, décroissantes avec `depth`) :

| Production | Exemple généré | Variants `Lang` couverts |
|---|---|---|
| Littéral du type but | `42`, `"a"`, `true` | littéraux |
| Variable de l'env au bon type | `x3` | `Var` |
| Appel d'une fn générée/stdlib dont le retour unifie au but | `add(x1, 2)` | `FunctionApp`, unification |
| Constructeur de record (si but = alias record) | `Point:{ x = ..., y = ... }` | `ConstructorCall` |
| Record anonyme | `:{ a = ... }` | `List` |
| Tag d'une union (si but = union) | `.Some(...)` | `Tag` |
| `match` sur une valeur d'union, toutes branches au type but | `match v { .A(x) => ..., }` | `Match` |
| `if/else` aux deux branches du type but | | contrôle |
| Lambda / fn (si but = type fonction) | `fn(x: int): int { ... }` | `Function`, `Lambda` |
| Pipe / dot-access (si un champ du bon type existe) | `p.x`, `p \|> f()` | `Op::Dot`, `Op::Pipe` |
| Vector homogène (si but = array) | `[1, 2, 3]` | `Vector` |

Au niveau programme : une séquence de déclarations (`type` alias record/union/opaque,
`let` de fonctions typées, `module M { ... }` + `use M::...`, `let` de valeurs) qui
enrichissent `GenEnv`, terminée par des expressions consommatrices. **La pondération force
les croisements de features** — c'est aux intersections (record × module × générique,
spread × constructeur, union × match × pipe) que vivent les bugs, comme le montre tout
l'historique de `cases/`.

Ce qu'on **exclut du générateur en v1** (pour garder « bien typé par construction » vrai) :
les génériques kindés (`%T`/`^T`/`#N`), les interfaces, la partial application, `@testable`.
On les ajoute par incréments *après* que la couverture de base est stable — chaque ajout au
générateur est petit une fois le squelette en place.

### Boucle d'exploitation des échecs

1. Proptest **shrinke** automatiquement vers un programme minimal (les stratégies doivent
   être écrites shrink-friendly : les `prop_oneof!` avec les productions simples en premier).
2. `typr fuzz` écrit le programme minimal + la seed dans
   `fuzz_failures/<hash>/` (projet complet rejouable).
3. Promotion manuelle (ou `typr fuzz --promote <hash>`) vers le catalogue :
   `typr case add fuzz-<hash> --from fuzz_failures/<hash> --cmd run --layer r-run`, puis on
   écrit l'`expect.toml` — la boucle `cases/` existante (OPEN → fix → freeze) prend le relais.
4. Les seeds des échecs passés sont persistées (mécanisme natif proptest,
   `proptest-regressions/`) : chaque bug trouvé reste un test de régression gratuit.

### Métrique d'exhaustivité

Sans métrique, « exhaustif » ne veut rien dire. Le générateur logge quels variants
`Lang`/`Type` chaque programme exerce (simple visite de l'AST généré) ; `typr fuzz --stats N`
affiche la couverture agrégée sur N programmes :

```
Lang coverage: 34/51 variants   (missing: PartialApp, TestBlock, ...)
Type coverage: 22/30 variants   (missing: Interface, GenericKinded, ...)
Pair coverage (feature × feature): 61%
```

Les variants manquants = la todo-list des prochains incréments du générateur.

### Critères de fin (v1)

- Étage 1 stable dans le CI : 256 cas/run, zéro flake, < 30 s.
- Étage 2 : `typr fuzz 500` tourne en local, produit un rapport, et a trouvé **au moins un
  bug réel** (c'est quasi certain — sinon augmenter les croisements de features).
- Couverture rapportée ≥ 60 % des variants `Lang` non exclus.
- Un échec fuzz promu en case et freezé, prouvant la boucle complète.

### Risques / pièges connus

- **Le générateur a des bugs lui-même** : un programme « bien typé par construction » rejeté
  par `typr check` est ambigu (bug générateur ou type-checker ?). Règle : chaque rejet est
  trié à la main au début ; les rejets légitimes deviennent des contraintes du générateur,
  les autres des bugs type-checker. Ce tri *est* de la valeur (il documente la sémantique).
- **Explosion du temps R** : chaque exécution Rscript coûte ~200 ms+. D'où la séparation
  étage 1 (sans R, massif) / étage 2 (avec R, échantillonné).
- **Shrinking cassé par le contexte** : retirer une déclaration peut invalider la suite.
  Les stratégies doivent shrinker *en respectant les dépendances* (retirer un binding
  seulement si rien ne l'utilise) — c'est le point délicat de l'implémentation proptest.

---

## Phase C — Oracle statique : base de noms base-R / S4

> **Implémentée (2026-07-18)** — voir CLAUDE.md § Static Base-R/S4 Name Lint (Phase C) pour
> l'état exact. Une déviation volontaire par rapport à la description ci-dessous : `--lint-r`
> (le lint optionnel `codetools::checkUsage` sur le R généré) n'a **pas** été implémenté — il
> était explicitement hors des critères de fin du plan, contrairement aux 4 règles du tableau
> qui, elles, sont implémentées et testées.
>
> **Rentabilisée immédiatement, avant même d'être branchée en continu** : lancer le lint sur un
> projet `typr new` frais (zéro code utilisateur) a fait remonter **18 collisions réelles**
> déjà présentes dans le propre stdlib de typr (`nchar`, `sd`, `substr`, `sub`, `gsub`,
> `strsplit`, `tolower`, `toupper`, `startsWith`, `endsWith`, `grepl`, `setwd`, `unlink`,
> `system2`, `version`, plus `dir`/`getwd` — voir plus bas) — parce que
> `Context::get_all_generic_functions` parcourt tout le contexte préchargé, pas seulement les
> noms déclarés par l'utilisateur : **tout** projet TypR aurait planté au premier appel à l'une
> de ces fonctions, exactement comme `nlevels` historiquement. Confirmé avec une exécution R
> réelle (`UseMethod` sur un objet de classe `Character` → `no applicable method`) avant
> correction. Les 14 fonctions à arité ≥ 1 ont reçu un `.default` dans `configs/src/std.R`
> déléguant à leur équivalent `base::`, dans le même style que `max.default`/`nlevels.default`
> (`unlink.default` normalise le code de sortie en bool, `system2.default` et `strsplit.default`
> ajustent la forme de retour ; `version.default` — spécifique à `State<T>`, pas d'équivalent
> base-R sensé — lève une erreur claire plutôt que de faire semblant). La ligne bare
> `version <- function(a, ...) UseMethod("version")` (violation de la règle dure `std.R` du
> CLAUDE.md, `generic_functions.R` la régénère de toute façon) a été supprimée au passage.
>
> **`dir`/`getwd` restent exclus du lint** (`ZERO_ARITY_STDLIB_EXEMPT` dans `r_name_lint.rs`) :
> ce sont des signatures stdlib à **zéro paramètre** (`@dir: () -> [#N, char];`,
> `@getwd: () -> char;`), mais elles reçoivent quand même le stub générique standard
> `name <- function(x, ...) UseMethod('name', x)` — appeler `dir()`/`getwd()` sans argument (ce
> que leur propre signature exige) échoue sur le paramètre `x` manquant **avant même** que
> `UseMethod` ne dispatche, qu'un `.default` existe ou non. C'est un bug de transpilation
> distinct et plus profond (désaccord entre une signature stdlib à arité zéro et la génération
> systématique du stub générique), pas une collision de noms qu'un `.default` peut réparer —
> non corrigé ici, volontairement cataloqué à part plutôt que masqué par un `.default`
> inatteignable.
>
> **Validation** : `typr case run` (catalogue complet, binaire debug et release) donne le même
> résultat avant et après ce travail — `REGRESS` sur 0007/0010/0012 et `OPEN` sur 0017 sont
> confirmés préexistants sur `main` (vérifié via `git stash`/rebuild/replay), aucune régression
> nouvelle introduite par le lint ou les correctifs `std.R`. Couvert par
> `crates/typr-cli/tests/r_name_lint.rs` (les 4 règles du tableau, plus le garde-fou "projet
> frais sans code utilisateur ne déclenche aucune erreur").

### Objectif

Le transpileur connaît tous les symboles top-level qu'il émet (stubs `UseMethod` via
`Context::get_all_generic_functions`, constructeurs, casts de `types.R`). Il suffit de les
intersecter **à la compilation** avec une base de données de l'écosystème R pour attraper
toute la famille « collision de noms » — celle du bug `nlevels` — de façon exhaustive et
sans exécuter R.

### La base de données

Un script `tools/gen_r_name_db.R` (lancé à la main, résultat commité — comme
`functions_R.txt`) produit `crates/typr-cli/configs/src/r_name_db.json` :

```json
{
  "r_version": "4.4.1",
  "names": {
    "nlevels":  { "pkg": "base", "s3_generic": false, "s4_generic": false, "has_default": false },
    "print":    { "pkg": "base", "s3_generic": true,  "s4_generic": false, "has_default": true },
    "show":     { "pkg": "methods", "s3_generic": false, "s4_generic": true, "has_default": false }
  }
}
```

Sources côté R : `ls(baseenv())` + les namespaces standards (`stats`, `utils`, `methods`),
`utils::isS3stdGeneric` / présence dans le dispatch interne, `methods::getGenerics()` pour
S4, existence de `name.default`. Embarqué via `include_str!` dans typr-cli.

### Les règles de lint (au `typr build`)

Pour chaque nom top-level que le build s'apprête à émettre :

| Situation | Verdict |
|---|---|
| Nom émis comme stub `UseMethod` ∧ nom base-R non générique ∧ pas de `name.default` émis | **Erreur** — c'est exactement le bug `nlevels` : le shadow ne laisse aucun repli |
| Nom émis ∧ générique S4 connu | **Warning** (erreur si `--strict`) — le stub S3 masque le dispatch S4 pour tout code aval |
| Nom émis ∧ nom base-R générique avec `.default` | OK (pattern `max`/`max.default` établi) |
| Constructeur de record dont le nom = classe S4 connue | **Warning** — `structure(class=)` vs classe S4 formelle du même nom |

Implémentation : une passe dans `crates/typr-cli/src/project.rs` juste avant l'écriture des
`R/*.R`, sur la liste des noms déjà collectée pour `generic_functions.R`. Aucune modification
de typr-core nécessaire.

### Lint optionnel du R généré (`--lint-r`)

Étape opt-in de `typr build` : passer chaque `R/*.R` généré dans un
`Rscript -e 'parse(...); codetools::checkUsage(...)'` — attrape variables libres et fonctions
inexistantes dans le R émis. Fail-open si `Rscript` absent (même politique que les cases
`@run`). Utile surtout combiné à la phase B (chaque programme fuzzé passe le lint gratuitement).

### Critères de fin

- La suppression du `nlevels.default` de `std.R` fait échouer `typr build` d'un projet qui
  utilise `nlevels` — le bug historique est désormais impossible à réintroduire.
- Un test d'intégration typr-cli couvre les 4 règles du tableau.
- Le script de génération de la DB est documenté (quand la régénérer : montée de version R).

---

## Phase D — Matrice d'interop S4 / R6 / RC / S7 / base

> **v1 implémentée (2026-07-18)** — voir `interop_matrix.md` pour la grille complète et le
> détail. Périmètre v1 exactement celui prescrit ci-dessous : lignes {1 S3, 2 S4, 7 base
> attribué} × colonnes {a argument, b retour, c `let` annoté, f accès `.`/`$`}, soit les 12
> cases `cases/0018`-`cases/0029`. Mécanique : `Foreign<T>`/`@extern` (déjà implémentés dans
> le code, contrairement à ce que suggérait la doc d'interop externe non committée) +
> fixtures `.rds` pré-générées (`tools/gen_interop_fixtures.R`, même convention que
> `tools/gen_r_name_db.R`) plutôt qu'un fichier R compagnon sourcé au run — le loader de
> projet TypR (`load_module.R`) source chaque `R/*.R` dans un environnement isolé et ne
> partage les bindings qu'via des tags `@include` générés par TypR lui-même, donc un fichier
> R écrit à la main n'y serait pas visible sans plomberie dédiée.
>
> **Construire la toute première case a immédiatement trouvé 4 bugs réels**, tous corrigés :
> l'usage `type X <- Foreign<Any>;` documenté dans `foreign.ty` lui-même ne fonctionnait pas
> du tout. (1) `Foreign<T>` avec un argument de type explicite levait un faux
> `AliasArityMismatch` — `collect_undefined_aliases` ignorait l'arité seulement quand la
> *référence* était opaque, jamais quand la *cible résolue* l'était (le cas exact de
> `Foreign<T>`). (2) un `@extern` à nom nu (`readRDS`) recevait quand même le stub
> `UseMethod` générique de `generic_functions.R`, qui l'aurait shadowé — `nlevels` bug mais
> pour `@extern`. (3) `let x: T <- expr` (et un retour de fonction typé, même helper)
> passait la valeur dans `as.X()`, qui ajoute des classes TypR sur le vecteur de classes
> *réel* via `struct()`/`class(x) <-` — inoffensif pour S3, mais **casse silencieusement les
> objets S4** (R perd le statut S4 dès qu'une deuxième classe est assignée). (4) une fonction
> TypR de premier niveau prenant un paramètre `Foreign<T>` est émise comme méthode S3 sur un
> nom d'alias synthétique (`f.Foreign0`) qu'aucune valeur étrangère réelle ne porte jamais —
> dispatch **inatteignable**. Les quatre corrections + leur rationale complet sont dans
> `interop_matrix.md` et les `expect.md` des cases concernées.
>
> **`--checked` (interaction Phase A/D)** : les 3 cases colonne c tournent avec
> `checked = true` (nouveau champ `CaseMeta.checked`, ajoute `--checked` au replay) — la
> question posée par le plan ("que doit faire `typr_assert_type` face à un objet R6/S4
> annoté d'un type précis ?") est tranchée par le fix (3) ci-dessus : aucune assertion,
> `checked_descriptor` retourne `None` pour tout type résolvant vers `Foreign<T>`, sans quoi
> ce serait un faux positif garanti sur tout usage légitime de l'échappatoire.
>
> **Trous documentés, pas des bugs** : les 3 cases colonne f (`status = "wontfix"`)
> confirment que `val.field` sur un `Foreign<T>` échoue toujours à la compilation (Any n'a
> aucun champ structurel) — accès volontairement fermé, contournable via un accesseur
> `@extern` dédié. Ligne S7 marquée `n/a` (package absent de l'environnement de dev/CI, pas
> testé) plutôt que silencieusement ignorée. Reste en backlog : lignes RC/R6 (packages
> installés, pas encore exercées), colonnes d/e/g/h/i/j (record field, `match`, pipe, `for`,
> frontière de module, instanciation générique) pour toutes les lignes.

### Objectif

Les valeurs étrangères viennent *de l'extérieur* — la génération aléatoire (B) ne peut pas
les couvrir. Il faut une suite de conformité manuelle mais **systématique** : une grille dont
chaque case est un mini-projet `cases/` Phase 2 (`cmd = "run"`, oracle `@run`) où une vraie
valeur étrangère traverse une frontière TypR. La valeur de la grille est autant dans ses
**trous visibles** (ce qu'on sait ne pas couvrir) que dans ses cases vertes.

### La grille

Doc versionné `interop_matrix.md`, mis à jour à chaque case ajoutée :

Lignes (provenance de la valeur) :

| # | Système | Exemple de valeur |
|---|---|---|
| 1 | S3 externe (package tiers) | objet `lm`, `data.frame` exotique |
| 2 | S4 | objet `Matrix::Matrix`, classe S4 maison |
| 3 | RC (Reference Classes) | `setRefClass` maison |
| 4 | R6 | objet `R6::R6Class` |
| 5 | S7 | classe `S7::new_class` |
| 6 | Closure / environnement pur | fonction fabriquée par une factory R |
| 7 | Vecteur de base avec attributs | vecteur nommé, `factor` externe, `Date`/`POSIXct` |

Colonnes (frontière TypR traversée) :

| # | Frontière | Syntaxe |
|---|---|---|
| a | Argument de fonction TypR | `f(val_etrangere)` |
| b | Valeur de retour vers TypR | `let x <- foreign_fn();` |
| c | `let` annoté | `let x: T <- ...` |
| d | Champ de record | `Point:{ x = val_etrangere }` |
| e | Sujet d'un `match` | `match val { ... }` |
| f | Accès `$` / `.` | `val$field`, `val.method()` |
| g | Pipe | `val \|> f()` |
| h | Itérable de `for` | `for x in val { ... }` |
| i | Traversée de frontière de module | passée à une fn `@pub` d'un module |
| j | Instanciation d'un générique | `id(val_etrangere)` |

Soit 70 cases. Toutes ne sont pas sensées (S7 × `for` peut être marqué `n/a`) et toutes
n'ont pas la même valeur — **priorisation v1** : les lignes 1, 2 et 7 (les plus fréquentes
dans l'écosystème) × les colonnes a, b, c, f (les frontières les plus communes), soit ~12
cases pour commencer. Chaque cellule de la grille pointe vers son ID `cases/` ou porte
`—` (trou connu) / `n/a`.

### Mécanique

- Chaque case suit le format existant : `repro/` avec un vrai projet dont le `main.ty` reçoit
  la valeur étrangère (fabriquée dans un fichier R compagnon sourcé au run), `expect.toml`
  avec des règles `file = "@run"` sur la sortie réelle.
- **Interaction avec la phase A** : chaque case de la matrice se lance aussi sous `--checked`
  — c'est là que les assertions sur les valeurs étrangères révèlent la politique à trancher
  au cas par cas : que doit faire `typr_assert_type` face à un objet R6 annoté `Any` ?
  (no-op, déjà décidé) — et face à un R6 annoté d'un type précis ? (probablement : erreur,
  c'est le but). Ces décisions se documentent dans la grille.
- Les cases qui échouent restent `status = open` dans `cases/` : la matrice **est** un
  backlog priorisé du chantier interop, elle alimente directement le plan d'interop externe
  (marshalling `to_native`/`from_native`, `@extern`) déjà esquissé — chaque case ouverte est
  un critère d'acceptation tout prêt pour ce futur chantier.

### Critères de fin (v1)

- `interop_matrix.md` existe avec la grille complète (y compris `n/a` motivés).
- Les ~12 cases prioritaires existent dans `cases/` et tournent via
  `typr case run --status open` (verdict honnête : OPEN si ça casse, PASS si ça marche).
- CLAUDE.md pointe vers la matrice depuis la section `cases/`.

---

## Récapitulatif des dépendances et de l'ordre

```
A (--checked)  ──────► B (générateur, utilise A comme oracle)
     │
     └──────────────► D (chaque case matrice relancée sous --checked)
C (base de noms) — indépendant, insérable à tout moment
```

1. **A d'abord** : rentabilise immédiatement `lab/` + `cases/` existants, prérequis de B.
2. **B ensuite** : le gros morceau, seule source d'exhaustivité réelle ; démarre dès que
   l'oracle A est stable.
3. **C en parallèle** de B (petit, indépendant, un après-midi + le script R).
4. **D en continu** : la grille se remplit case par case, en priorité les 12 cellules v1,
   et sert de backlog au futur chantier interop externe.
