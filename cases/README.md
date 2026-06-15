# cases/ — catalogue de bugs reproductibles TypR

Chaque sous-dossier est un **cas** auto-portant : un bug rencontré dans un vrai projet TypR,
réduit à une repro rejouable et vérifiable automatiquement. Outil : la sous-commande `typr case`
(implémentée dans `crates/typr-cli/src/cases.rs`).

## Anatomie d'un cas

```
cases/0001-pub-typed-fn-in-module/
  case.toml      # title, cmd (build|check|debug), layer, status, created, origin
  repro/         # un VRAI projet TypR (TypR/main.ty, …) rejoué tel quel par `typr <cmd>`
  expect.toml    # ORACLE : règles must_contain / must_not_contain sur le R généré
  expect.md      # ce qui DEVRAIT se passer (prose), + localisation dans le code
  observed.txt   # l'erreur / le R fautif au moment du report
  golden/        # (cas fixed) copie des R/*.R ciblés → diff de non-régression
```

`case.toml` :

| champ    | valeurs                              | rôle                                  |
|----------|--------------------------------------|---------------------------------------|
| `cmd`    | `build` \| `check` \| `debug`        | comment rejouer le repro              |
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
Phase 2 (à venir) : oracle comportemental via `typr document` / `Rscript` pour les cas `layer=r-run`.

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
typr case add <slug> [--from <dir>] [--cmd build]
typr case snapshot [slug]                     # depuis un projet : crée <slug>.case/ in situ
typr case freeze <id>
typr case show <id>
```

`run` rejoue chaque cas en réinvoquant ce même binaire `typr` (via `current_exe`) sur une copie
temporaire de `repro/` — un rebuild est donc pris en compte automatiquement (pas de `--build`).
`--keep` conserve les sandbox temporaires (chemin affiché) pour inspection. À lancer depuis la
racine du projet (là où vit `cases/`).
