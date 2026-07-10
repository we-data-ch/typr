# cases/ — catalogue de bugs reproductibles TypR

Chaque sous-dossier est un **cas** auto-portant : un bug rencontré dans un vrai projet TypR,
réduit à une repro rejouable et vérifiable automatiquement. Outil : la sous-commande `typr case`
(implémentée dans `crates/typr-cli/src/cases.rs`).

## Anatomie d'un cas

```
cases/0001-pub-typed-fn-in-module/
  case.toml      # title, cmd (build|check|debug|run), layer, status, created, origin
  repro/         # un VRAI projet TypR (TypR/main.ty, …) rejoué tel quel par `typr <cmd>`
  expect.toml    # ORACLE : règles must_contain / must_not_contain sur le R généré (ou, pour
                 # layer=r-run, sur le stdout+stderr d'une exécution R réelle via file="@run")
  expect.md      # ce qui DEVRAIT se passer (prose), + localisation dans le code
  observed.txt   # l'erreur / le R fautif au moment du report
  golden/        # (cas fixed) copie des R/*.R ciblés → diff de non-régression (jamais "@run")
```

`case.toml` :

| champ    | valeurs                              | rôle                                  |
|----------|--------------------------------------|---------------------------------------|
| `cmd`    | `build` \| `check` \| `debug` \| `run` | comment rejouer le repro (`run` = build + exécution R réelle) |
| `layer`  | `parse` \| `type` \| `transpile` \| `r-run` | à quel étage le bug se manifeste |
| `status` | `open` \| `fixed` \| `wontfix`       | tri du catalogue + sémantique du run  |

## L'oracle : on ne décrit jamais le R attendu en entier

`expect.toml` liste seulement ce qui doit / ne doit pas apparaître dans le R généré — typiquement
une transcription directe des anomalies que tu noterais en prose :

```toml
[[rule]]
file = "R/leaf.R"
must_not_contain = "`g`.default"     # l'anomalie ne doit plus apparaître
[[rule]]
file = "R/leaf.R"
must_contain = "#' @method g"        # le comportement correct est présent
```

C'est robuste au formatage et auto-documenté. `golden/` (capturé, jamais écrit à la main) est un
filet de sécurité optionnel : sur les cas `fixed`, le run diffe les fichiers ciblés.

Phase 1 = oracle par règles, **sans exécuter R** : `typr build` écrit `R/*.R` *avant* l'étape
`document()`, donc le grep fonctionne même si `document()` échoue (R/devtools absent).

**Phase 2 (`layer = "r-run"`) exécute R pour de vrai.** Associe `cmd = "run"` (au lieu de
`build`) : le sandbox lance `typr run`, qui build **et** exécute le point d'entrée du projet via
`Rscript`. Les règles avec `file = "@run"` grep alors le stdout+stderr capturés de cette
exécution réelle, plutôt qu'un fichier généré :

```toml
[[rule]]
file = "@run"
must_not_contain = "no applicable method"   # mauvais dispatch S3 détecté seulement à l'exécution
[[rule]]
file = "@run"
must_contain = "42"                          # le résultat correct est bien produit
```

C'est le seul moyen d'attraper les bugs invisibles dans le texte R généré mais qui explosent à
l'exécution (mauvais dispatch S3, mauvais ordre d'arguments, …). Échoue "ouvert" : si `Rscript`
n'est pas sur le PATH, `typr run` échoue et toute règle `@run` échoue avec — pas de cas
particulier à gérer. `@run` n'est **jamais** diffé en golden (le texte capturé inclut les
timings de la barre de progression CLI, qui varient à chaque run) — reste du grep pur, golden/
continue de ne cibler que des fichiers `R/*.R` déterministes.

## Capturer un cas depuis un projet en cours de dev (`snapshot`)

Quand tu rencontres un bug en développant un package TypR ailleurs, **depuis la racine de ce
projet** :

1. Annote le code fautif avec un commentaire-marqueur `#@case` (commentaire TypR valide, donc `#`) :

   ```typr
   #@case pub-fn-in-module: g.default émis au lieu de g dans un module
   @pub let g <- fn(): int { 1 };
   ```

   Le marqueur donne le **slug** (avant `:`), la **description** (après) et la **localisation**
   (fichier:ligne). Optionnel : un fichier `.case.md` à la racine pour une description plus longue.

2. `typr case snapshot [slug]` → produit un bundle auto-portant `<slug>.case/` **dans le projet** :
   - `repro/` = copie curatée (`TypR/` + `DESCRIPTION`/`NAMESPACE`, sans `renv/`, `.git/`, etc.) ;
   - `case.toml` / `expect.md` (depuis `.case.md` + marqueurs) / `expect.toml` (squelette) ;
   - `observed.txt` = build capturé du snapshot ;
   - crée un `.case.md` squelette s'il n'existe pas encore.

3. **Déplace** `<slug>.case/` toi-même dans `cases/` (renommé `NNNN-<slug>`), édite `expect.toml`
   (anomalies → règles), puis enchaîne sur la boucle ci-dessous.

## La boucle

```
bug rencontré dans un projet
  → (dans le projet) typr case snapshot <slug>   # bundle <slug>.case/ ; OU : typr case add --from <projet>
  → déplacer le bundle dans cases/NNNN-<slug>/
  → éditer case.toml / expect.toml / expect.md
  → typr case run <slug>                      # OPEN = reproduit · READY = semble corrigé
  → (corriger typr-core)                       ↺
  → typr case freeze <id>                     # fige golden, status → fixed
  → typr case run --status fixed              # garde-fou : exit≠0 si régression (CI)
```

### Verdicts de `run`

| verdict   | signification                                              |
|-----------|------------------------------------------------------------|
| `OPEN`    | cas ouvert, les règles échouent → le bug se reproduit      |
| `READY`   | cas ouvert mais les règles passent → prêt à `freeze`       |
| `PASS`    | cas `fixed`, règles + golden OK                            |
| `REGRESS` | cas `fixed` cassé → **régression** (run sort en code 1)    |
| `SKIP`    | cas `wontfix`                                              |

## Commandes

```
typr case list [--status open]
typr case run [filtre] [--status s] [--keep]
typr case add <slug> [--from <dir>] [--cmd build] [--layer transpile]
typr case snapshot [slug]                     # depuis un projet : crée <slug>.case/ in situ
typr case freeze <id>
typr case show <id>
```

Pour un cas `r-run` créé depuis un vrai projet : `typr case add <slug> --from <dir> --cmd run
--layer r-run` (`add` curate la copie comme `snapshot` — `TypR/` + `DESCRIPTION`/`NAMESPACE`
seulement, sans `renv/`/`.git/`).

`run` rejoue chaque cas en réinvoquant ce même binaire `typr` (via `current_exe`) sur une copie
temporaire de `repro/` — un rebuild est donc pris en compte automatiquement (pas de `--build`).
`--keep` conserve les sandbox temporaires (chemin affiché) pour inspection. À lancer depuis la
racine du projet (là où vit `cases/`).
