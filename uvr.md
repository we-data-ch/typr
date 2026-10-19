# Remplacer `renv` par `uvr`

Plan établi le 2026-10-19, à exécuter dans une prochaine session.

uvr : <https://github.com/nbafrank/uvr> (doc : <https://nbafrank.github.io/uvr/>), v0.4.x, binaire
Rust unique, inspiré de `uv`. Fichiers d'un projet : `uvr.toml` (manifeste, commité), `uvr.lock`
(commité), `.r-version` (optionnel, commité), `.uvr/library/` (ignoré par git).
Commandes utiles : `uvr init [--bare]`, `uvr add <pkg…>` (`--bioc`, `user/repo@ref`, `git::…`,
URL de tarball), `uvr remove`, `uvr sync [--frozen]`, `uvr lock`, `uvr run [script.R]`,
`uvr tree`, `uvr doctor`.

## Décisions prises

1. **renv est supprimé net**, sans release de transition : personne ne l'utilise, il n'y a donc
   aucun projet existant à migrer. (`uvr import --lock` existe si un jour quelqu'un en a besoin.)
2. **Double écriture + vérification de cohérence** : `typr use <pkg>` écrit dans `DESCRIPTION`
   (`Imports`) **et** dans `uvr.toml`, et `typr check` signale tout écart entre les deux (quelqu'un
   peut éditer l'un des deux fichiers à la main).
3. **Toute installation est explicite et passe par uvr** : plus aucune installation implicite
   (en particulier dans le type-checker).

## Résultats de l'étape 0 (faite le 2026-10-19, uvr 0.4.7 via `cargo install --git`)

1. **`uvr run -e` n'existe pas.** `uvr run` n'accepte qu'un fichier script (`[SCRIPT] [-- ARGS]`) ;
   `uvr run /dev/stdin` n'affiche rien. **Repli retenu** : lancer `R`/`Rscript` directement avec
   `R_LIBS=<racine>/.uvr/library` dans l'environnement du sous-processus (vérifié : `.libPaths()[1]`
   pointe sur la bibliothèque du projet, les paquets s'y chargent). Pas de script temporaire, et pas
   de dépendance à uvr pour *exécuter* R, seulement pour *installer*. `REnv::Uvr` ⇒ `R_LIBS` posé.
   (`uvr run script.R` marche et reste utile pour un script réel, avec `--r-version` si besoin.)
2. **`uvr add --dev <pkg…>`** existe → section `[dev-dependencies]`. `--no-lock`/`--no-install`
   permettent de grouper plusieurs ajouts puis de faire un seul `uvr lock` + `uvr sync`.
3. **Installer le paquet local** : `remotes::install_local(".")` exécuté sous `uvr run script.R`
   installe bien dans `.uvr/library` (vérifié). Il faut donc `remotes` (ou devtools) en dev-dep, et un
   script temporaire (pas de `-e`) ; avec le repli `R_LIBS`, `R -e "devtools::install_local('.')"`
   marche sans script, mais installe dans la **première** entrée de `.libPaths()` = `.uvr/library`.
4. **`uvr init` vs `--bare`** : le non-bare écrit un `.Rprofile` (bloc `# >>> uvr >>>`, ajoute
   `.uvr/library` à `.libPaths()`) ; `--bare` n'écrit que `uvr.toml`, `.uvr/library/`, `.gitignore`.
   `typr new` n'écrit aucun `.Rprofile` aujourd'hui → pas de conflit. **Choix : non-bare** (RStudio
   voit alors la bibliothèque), donc `^\.Rprofile$` entre dans `.Rbuildignore`. `uvr init <name>`
   crée un dossier ; dans un dossier existant, utiliser `uvr init --here <name>`. Pour la CI/les
   tests : `--unattended` (aucune écriture dans l'arbre, pas de paquet compagnon).
5. **Nom du projet** : `uvr.toml [project] name` est libre, sans lien avec `Package:` ; on met le
   même nom par cohérence.
6. **`uvr add` dans un paquet R** : ne lit **pas** DESCRIPTION et ne la modifie pas, ne gêne pas
   (DESCRIPTION/NAMESPACE/R/ sont ignorés). D'où la double écriture. Bonus : `uvr scan` liste les
   paquets utilisés dans `R/*.R` mais absents de `uvr.toml` (`library(x)` et `x::f`).
7. Le **paquet compagnon** `uvr` s'installe dans la bibliothèque (`--no-companion` pour l'éviter) :
   à passer systématiquement depuis typr, et à exclure du compte des paquets (déjà fait côté uvr).

Conséquence pour l'étape 1 : `r_eval`/`rscript_eval` = `R`/`Rscript` + `R_LIBS` (plus d'argv `uvr run -e`) ;
`add_packages` = `uvr add [--dev] --no-companion …`.

## Inventaire de l'existant (ce qui dépend de renv / installe des paquets)

| Endroit | Rôle actuel |
|---|---|
| `crates/typr-cli/src/cli.rs:46` | flag `typr new --renv` (défaut `true`) |
| `crates/typr-cli/src/project.rs:737` | appel de `renv_init()` dans `new()` |
| `crates/typr-cli/src/project.rs:1816` | `renv_init()` → `R -e "renv::init()"` |
| `crates/typr-cli/src/project.rs:1850` | `renv_snapshot()` |
| `crates/typr-cli/src/project.rs:1879` | `is_renv_active()` (présence de `renv.lock`) |
| `crates/typr-cli/src/project.rs:1885` | `pkg_install` : `renv::install(...)` ou `install.packages(...)` + snapshot ; sans argument : `devtools::install_local` |
| `crates/typr-cli/src/project.rs:1973` | `pkg_uninstall` : `remove.packages(...)` |
| `crates/typr-cli/src/project.rs:2214` | `use_package` : `usethis::use_package` (DESCRIPTION seulement) |
| `crates/typr-cli/src/r_deps.rs:147,174` | scan de `renv/library/**` pour la vérification de démarrage (+ tests `renv_lib_dirs_*` l. 452-468) |
| `crates/typr-cli/src/r_deps.rs:328` | `typr init` : `renv::install` ou `install.packages` de devtools/testthat |
| `crates/typr-core/src/processes/type_checking/mod.rs:168-205,403` | **installation implicite** : chaque `library(x)` lance `install.packages(x)` hors de tout environnement |
| `crates/typr-cli/src/cases.rs:540`, `cases/README.md:116,160` | exclusion de `renv/` lors de `typr case add` |
| `crates/typr-cli/configs/.gitignore`, `.Rbuildignore`, `instructions.md`, `README.md` | templates de `typr new` |
| `typr.github.io/docs/tutorials/typr-for-dummies.md:108,378` | mentions de renv dans la doc |

Il y a aussi 27 `Command::new("R"|"Rscript")` répartis dans 9 fichiers (`io.rs`, `r_name_cache.rs`,
`gen_types.rs`, `fuzz.rs`, `r_deps.rs`, `registry_validate.rs`, `project.rs`,
`tests/incremental_builds.rs`, `typr-core/.../type_checking/mod.rs`). Aucun ne sait dans quel
environnement il s'exécute.

## Étape 0 : vérifier uvr à la main (avant de coder)

uvr n'était pas installé le 2026-10-19. On l'installe (`curl -fsSL
https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | sh`, ou
`cargo install --git https://github.com/nbafrank/uvr`), puis on teste sur un projet `typr new`
jetable. Ces points ne figurent pas dans le README et décident de la conception :

1. **`uvr run -e "<expr>"`** est-il accepté ? Tous les appels de typr sont des `R -e`. Sinon,
   prévoir un repli : script temporaire + `uvr run script.R`, ou lancer `R` avec
   `R_LIBS_USER=<projet>/.uvr/library` (et le R choisi par uvr) dans l'environnement du
   sous-processus.
2. **Dev-dependencies** : existe-t-il `uvr add --dev` ? (`uvr.toml` a bien une section
   `[dev-dependencies]`.) On en a besoin pour devtools/testthat.
3. **Installer le paquet local** (l'équivalent de `devtools::install_local`) : ce n'est pas
   documenté. Hypothèse : `devtools::install_local('.')` exécuté *via* `uvr run`, pour que le
   paquet atterrisse dans `.uvr/library`.
4. **`uvr init` contre `uvr init --bare`** : le premier écrit un `.Rprofile` (et une config IDE),
   ce qui rend la bibliothèque visible depuis RStudio. Le second non. Choisir, et vérifier que
   `.Rprofile` n'entre pas en conflit avec ce qu'écrit `typr new`.
5. Le **nom du paquet** dans `uvr.toml [project] name` doit-il valoir le `Package:` de DESCRIPTION ?
6. `uvr add` dans un projet qui *est* lui-même un paquet R : le comportement est-il correct ? Faut-il
   lire DESCRIPTION ?

Consigner les réponses en tête de ce fichier avant de passer à l'étape 1.

## Étape 1 : une couche d'abstraction `crates/typr-cli/src/r_env.rs` — FAITE le 2026-10-19

Fait : `r_env.rs` (`REnv::{System,Uvr}`, `project_env`/`current_env`, `Cmd` pur testable, `r_eval`,
`rscript_eval`, `rscript_file`, `add_packages`, `remove_packages`, `sync`, `init`, `require_uvr`,
9 tests). `R_LIBS=<root>/.uvr/library` posé devant l'éventuel `R_LIBS` de l'utilisateur. Routés par
`r_env` : les appels R « projet » de `project.rs` (run, profile, test, document, pkgdown, load_all,
check, `use_package`, `pkg_uninstall`), `io.rs` (`execute_r_with_path*`), la sonde `.libPaths()` de
`r_deps.rs`. **Restent volontairement sur le R système** : `gen_types.rs`, `r_name_cache.rs`,
`registry_validate.rs`, `fuzz.rs` (sondes/introspection CRAN), `tests/incremental_builds.rs`, et
`typr-core` (étape 3). Les 4 appels renv (`renv_init`, `renv_snapshot`, `pkg_install`, `typr init`)
sont laissés tels quels : l'étape 2 les remplace. `#![allow(dead_code)]` dans `r_env.rs` jusqu'à l'étape 2.


- Détection : `fn project_env(root: &Path) -> REnv` avec `REnv::Uvr` (présence de `uvr.toml`) ou
  `REnv::System`. Pas de variante renv.
- Des fonctions **pures** qui renvoient un argv (`Vec<OsString>` + env), testables sans R ni uvr :
  - `r_eval(env, expr)` : `R -e expr` ou `uvr run -e expr` (ou le repli de l'étape 0.1)
  - `rscript_eval(env, expr)`
  - `add_packages(pkgs, dev: bool)` → `uvr add …`
  - `remove_packages(pkgs)` → `uvr remove …`
  - `sync()` → `uvr sync`
  - `lib_dirs(root)` → `[root/.uvr/library]`
- Faire passer les 27 appels à R par `r_eval`/`rscript_eval`. Exception possible : les outils
  internes qui interrogent le R système volontairement (`tools/`, `gen_r_name_db`). À trancher
  appel par appel et à noter.
- Sur une commande d'installation sans uvr dans le PATH : erreur claire avec la commande
  d'installation de uvr. Jamais de repli silencieux sur `install.packages`.

## Étape 2 : remplacer les commandes — FAITE le 2026-10-19

Fait : `typr new` (`--no-uvr` remplace `--renv` ; `uvr init --here` puis `uvr add --dev devtools testthat usethis`,
avertissement non fatal si uvr manque), `typr use`/`typr pkg install <p…>` (`use_packages` : `uvr add` puis
`usethis::use_package` pour chaque paquet ; erreur claire sans uvr ou sans `uvr.toml`), `typr pkg install` seul
(`install_local` sous `R_LIBS`), `typr init` (`uvr add --dev` + `uvr sync`), `candidate_lib_dirs` (`.uvr/library`).
`renv_init`/`renv_snapshot`/`is_renv_active`/`renv_lib_dirs` supprimés. Vérifié à la main : `typr new demo`,
`typr use cli` (DESCRIPTION + `uvr.toml`), `typr init`, et les erreurs hors projet uvr. Notes : `usethis` ajouté
aux dev-deps (nécessaire à `typr use`) ; `uvr init` écrit un `.Rprofile` qui n'est pas encore dans `.Rbuildignore`
(étape 5) ; `remove_packages` reste inutilisé (aucune commande `typr` ne retire de dépendance).

(Plan initial :)

| Commande | Après |
|---|---|
| `typr new <name>` | supprimer `--renv` ; ajouter `--no-uvr` (uvr par défaut). `uvr init` dans le projet, puis `uvr add --dev devtools testthat` (selon l'étape 0.2). Si uvr est absent : créer le projet quand même, avertir, et indiquer `uvr init` à lancer plus tard |
| `typr pkg install <p…>` | `uvr add <p…>` (manifeste + lock + installation d'un coup), **plus** l'ajout dans DESCRIPTION, comme `typr use` |
| `typr pkg install` (sans argument) | `devtools::install_local('.')` via `uvr run` |
| `typr pkg uninstall` | `remove.packages` via `uvr run` (bibliothèque du projet) |
| `typr use <p>` | DESCRIPTION (`usethis::use_package`, via `uvr run`) **et** `uvr add <p>` |
| `typr init` | `uvr add --dev devtools testthat` + `uvr sync` ; hors projet uvr : erreur qui propose `uvr init` |
| vérification de démarrage (`r_deps.rs`) | `candidate_lib_dirs` : `.uvr/library` en premier, à la place de `renv_lib_dirs` |

À supprimer : `renv_init`, `renv_snapshot`, `is_renv_active`, `renv_lib_dirs` et leurs tests
(remplacés par des tests `.uvr/library`). Le commentaire en tête de `r_deps.rs` est à mettre à jour.

## Étape 3 : supprimer l'installation implicite dans typr-core — FAITE le 2026-10-19 (`install_package`/`is_package_installed` supprimés, arm `Lang::Library` sans sous-processus ; le diagnostic « non déclaré » reste pour l étape 4)

`type_checking/mod.rs` : supprimer `install_package` et `is_package_installed` (natif et stub
WASM). L'arm `Lang::Library` ne lance plus aucun sous-processus. Si le paquet n'est pas déclaré,
le signaler. Le diagnostic lui-même vit plutôt dans typr-cli (étape 4), car typr-core doit rester
pur et compatible WASM. Le message doit dire : « paquet `x` non déclaré : lancez `typr use x` ».

Effet de bord bienvenu : le type-checking devient déterministe et n'a plus d'effets de bord. Il faut
vérifier qu'aucun test ou cas ne dépendait de l'installation automatique (`cargo test --workspace`,
`typr case run`).

## Étape 4 : cohérence DESCRIPTION ↔ `uvr.toml` dans `typr check` — FAITE le 2026-10-19

Fait : `crates/typr-cli/src/r_manifest.rs` (pur : `description_deps`, `uvr_deps`, `library_uses`, `diagnose`
→ `Vec<Drift>` ; wrapper `check_project_dir`/`warn`), appelé en fin de `check_project` et `check_file`
(avertissements sur stderr, `typr check` réussit quand même). 6 tests unitaires + cas `0125-description-uvr-drift-warned`
(`copy_repro_curated` copie désormais aussi `uvr.toml`). Écarts au plan :
- `[dev-dependencies]` n'est **pas** comparé à DESCRIPTION dans le sens uvr → DESCRIPTION (devtools/usethis
  sont de l'outillage, un projet neuf avertirait à tort) ; seul `Suggests` → `[dev-dependencies]` est vérifié.
- Le code TypR n'est scanné que pour `library(x)`/`require(x)`, pas `x::f` (`::` sert aussi aux modules TypR,
  ambigu). Scan textuel de `TypR/**/*.ty`, commentaires `#` retirés.
- Un paquet déjà déclaré dans l'autre section uvr/DESCRIPTION n'est pas signalé.
- Les clés `uvr.toml` d'une dépendance git sont le nom du dépôt, pas forcément du paquet (limite connue).

(Plan initial :)

- Lire `DESCRIPTION` (`Imports`, `Depends` sauf `R`, `LinkingTo` ; `Suggests` ↔ `[dev-dependencies]`)
  et `uvr.toml` (`[dependencies]`, `[dev-dependencies]`).
- Signaler en avertissement (un warning, pas une erreur, au moins au début) :
  - un paquet dans DESCRIPTION absent de `uvr.toml` → « lancez `typr use x` » ;
  - un paquet dans `uvr.toml` absent de DESCRIPTION → « ajoutez-le à DESCRIPTION ou `uvr remove x` » ;
  - un `library(x)` / `x::f` dans le code TypR, avec `x` déclaré nulle part.
- Ignorer les paquets de base (`base`, `stats`, `utils`, `methods`, `graphics`, `grDevices`,
  `tools`, …).
- Ne rien faire si le projet n'a pas de `uvr.toml`.
- C'est une fonction pure (deux fichiers en entrée, une liste d'écarts en sortie), testée
  unitairement. Elle doit tourner sans R.
- Faire un cas `cases/` pour un écart volontaire (DESCRIPTION modifiée à la main).

## Étape 5 : templates de `typr new` (`crates/typr-cli/configs/`) — FAITE le 2026-10-19

Fait : `.gitignore` (`.uvr/`) — le fichier contenait par erreur un bloc `r#"…"#` de motifs Rbuildignore,
remplacé par `.typr_cache/`, `.uvr/`, `.Rproj.user/` ; `.Rbuildignore` (`uvr.toml`, `uvr.lock`, `.uvr`,
`.r-version`, `.Rprofile`) ; `instructions.md`, `package_structure.md`, `README.md` (uvr sync / typr use / uvr run).
Vérifié avec `typr new`.

(Plan initial :)

- `.gitignore` : ajouter `.uvr/`.
- `.Rbuildignore` : ajouter `^uvr\.toml$`, `^uvr\.lock$`, `^\.uvr$`, `^\.r-version$` (sinon
  `R CMD check` signale des fichiers non standard), plus `^\.Rprofile$` si `uvr init` en écrit un.
- `instructions.md`, `package_structure.md`, `README.md` : expliquer `uvr sync` (après un clone),
  `typr use`, `uvr run`.

## Étape 6 : outillage, cas, doc — FAITE le 2026-10-19

Fait : `cases.rs`/`cases/README.md` (mentions `.uvr/` au lieu de `renv/` ; la copie `copy_repro_curated`
est une liste blanche, rien à exclure en code) ; test d'intégration `crates/typr-cli/tests/new_project.rs`
(`typr new --no-uvr` : `.Rbuildignore`/`.gitignore` ciblent uvr, aucune trace de renv) à la place d'un cas
`cases/`, car un cas rejoue `check|build|run` sur une repro et ne peut pas lancer `typr new` ;
`install.sh`/`.ps1` inchangés (uvr non installé automatiquement) ; README du compilateur et de `editors/rstudio`
(paquet `uvr-r` optionnel) ; doc : `typr-for-dummies.md` l.378, nouvelle page `howto/manage-r-dependencies.md`
(+ `sidebars.ts`). Pas fait : `npm run build` du site (non lancé), `ai_context/CLAUDE.md` (fichier introuvable
à `../ai_context/`), CI (rien à changer). `cargo test --workspace` vert ; `typr case run` : 4 REGRESS
(0004, 0005, 0006, 0008, diff golden de `R/main.R`) non liés à cette étape, à examiner séparément.

(Plan initial :)

- `cases.rs:540` + `cases/README.md:116,160` : exclure `.uvr/` à la place de `renv/`.
- Faire un cas pour `typr new` : `uvr.toml` présent, `.Rbuildignore` contient `\.uvr`, pas de `renv`.
- `install/install.sh` / `.ps1` : ne **pas** installer uvr automatiquement. `typr init` et
  `typr new` affichent la commande d'installation si uvr manque.
- CI : les tests unitaires portent sur des argv et n'ont pas besoin de uvr. On n'installe uvr dans
  le job `cases` que si un cas l'exécute réellement.
- Doc :
  - `typr.github.io/docs/tutorials/typr-for-dummies.md:108,378`
  - une page how-to « Gérer les dépendances R avec uvr » (`docs/howto/`, avec `description:` en
    front matter, ajoutée à `sidebars.ts`)
  - `README.md` du compilateur
  - `editors/rstudio` : mentionner le paquet compagnon `uvr-r` (pas sur CRAN) sans le rendre
    obligatoire
- `typR/ai_context/CLAUDE.md` : mettre à jour si la section commandes/projets parle de renv.

## Validation de bout en bout — FAITE le 2026-10-19

`typr new demo` (uvr init + dev-deps), `typr use dplyr` (DESCRIPTION `Imports` + `uvr.toml [dependencies]`),
`typr check`, `typr build` (NAMESPACE mis à jour) : OK. `jsonlite` ajouté à la main dans DESCRIPTION → `typr check`
réussit et avertit « `jsonlite` is in DESCRIPTION but not in uvr.toml [dependencies]: run `typr use jsonlite` ».
Observations non liées à uvr : `typr test` sur un projet neuf échoue (« No test files found », aucun test dans
`tests/testthat`) ; le paquet registre `dplyr` signale `tests/smoke.ty` non chargeable (`db_drop_table` non défini).

## Restes de la migration

- (fait) 4 REGRESS (0004, 0005, 0006, 0008) : goldens de `R/main.R` antérieurs au formatage styler, écart de mise en forme seulement ; re-figés avec `typr case freeze`, `typr case run` sans REGRESS.
- Lancer `npm run build` dans `typr.github.io` (page `howto/manage-r-dependencies.md`, liens).
- `ai_context/CLAUDE.md` introuvable à `../ai_context/` : mettre à jour la section renv si elle existe.

## Ordre conseillé et validation

0 → 1 → 3 → 2 → 4 → 5 → 6. L'étape 3 vient tôt parce qu'elle est indépendante et qu'elle retire
un effet de bord qui fausserait les essais. À chaque étape : `cargo test --workspace` (avec
`RUST_MIN_STACK=8388608` si nécessaire) et `typr case run`. À la fin, faire un test de bout en bout
manuel : `typr new demo && cd demo && typr use dplyr && typr check && typr build && typr test`,
puis modifier DESCRIPTION à la main et vérifier l'avertissement de `typr check`.
