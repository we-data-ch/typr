# Diagnostic GitHub — release v0.6.2

État relevé le 2026-10-30 avec `gh` sur `we-data-ch/typr`.

## 1. État général du dépôt

| Contrôle | Résultat |
| --- | --- |
| `gh auth status` | Connecté en tant que `fabriceHategekimana` (scope `repo`) |
| Dépôt | `we-data-ch/typr`, public, branche par défaut `main`, issues activées |
| Branches | `main` et `develop` alignées (`main..develop = 0`, `develop..main = 0`) |
| PR #38 (`release v0.6.2`) | Fusionnée |
| Tag `v0.6.2` | Publié, non draft, non prerelease, avec tous les assets |
| CI sur `main` et `develop` | Verte (run 41042989189 et 41043011280) |
| Release workflow (run 41043007309) | **Échec** — 3 jobs |

Parties réussies de la release : `Publish to crates.io`, `Grammar → playground + docs`,
`VS Code extension`, `Vim / Neovim plugin`, build matrix complète, `Create Release`,
`RStudio runner package`, `Trigger docs rebuild`, `Docker image`.

## 2. Échec du job Homebrew tap

Étape : `Homebrew accepte la formule` (`.github/workflows/release.yml:724`).

```
Trusted tap: we-data-ch/typr
##[error]Invalid tap name: '/opt/homebrew/Library/Taps/we-data-ch/homebrew-typr'
##[error]Process completed with exit code 1.
```

Cause : `brew readall` attend un **nom de tap**, pas un chemin. Le `brew trust`
précédent a bien fonctionné ; c'est l'appel suivant qui est mal formé. Le job
équivalent de la CI (`.github/workflows/ci.yml:336`) utilise déjà la bonne forme.

Correctif :

```diff
-          brew readall "$TAP_DIR"
-          brew style "$TAP_DIR/Formula/typr.rb"
-          brew audit --strict --formula="$TAP_DIR/Formula/typr.rb"
+          brew readall we-data-ch/typr
+          brew style we-data-ch/typr/typr
+          brew audit --strict --formula we-data-ch/typr/typr
```

## 3. Échec du job Scoop bucket

Étape : `Scoop installe le binaire publié et le lance` (`.github/workflows/release.yml:968`).

```
Resolve-Path: Cannot find path 'D:\a\typr\typr\manifests\scoop\typr.json' because it does not exist.
```

Cause : `packaging/render.sh --out manifests` écrit le manifeste dans
`manifests/scoop/bucket/typr.json` (et l'historique dans
`manifests/scoop/bin/typr/<version>.json`). Le chemin utilisé par l'étape
d'installation omet le sous-dossier `bucket`.

Correctif :

```diff
-          $manifest = (Resolve-Path "manifests\scoop\typr.json").Path
+          $manifest = (Resolve-Path "manifests\scoop\bucket\typr.json").Path
```

## 4. Échec du job WinGet manifest

Étape : `wingetcreate` (`.github/workflows/release.yml:882`).

```
The forked repository could not be synced with the upstream commits. Sync your fork manually and try again.
```

Cause : le fork `fabriceHategekimana/winget-pkgs` (utilisé par
`WINGET_CREATE_TOKEN`) est désynchronisé de `microsoft/winget-pkgs`.
Ce n'est pas un bug du workflow.

Ce n'est pas un échec isolé : WinGet dépend d'un fork personnel à garder
synchronisé, d'un jeton `public_repo`, et d'une PR que les mainteneurs de
`microsoft/winget-pkgs` doivent accepter. Chaque release peut casser pour une
raison extérieure au dépôt. **Décision : on retire WinGet** au lieu de le réparer.

### Cible de remplacement : archive de la GitHub Release + `install.ps1`

Le canal Windows de base existe déjà et ne dépend d'aucun tiers :

- le job `build` publie `typr-<tag>-{x86_64,aarch64}-pc-windows-msvc.zip` et `checksums.txt` dans la
  GitHub Release ;
- `install/install.ps1` télécharge l'archive, vérifie son SHA-256 et pose
  `typr.exe` dans `%LOCALAPPDATA%\Programs\typr`, sans droits administrateur ;
- il est déjà testé en CI (`ci.yml`, suite `install/tests/run-tests.ps1` et
  `-DryRun` sur Windows) et publié sur le site de doc :

```powershell
irm https://we-data-ch.github.io/typr.github.io/install/install.ps1 | iex
```

Scoop reste disponible pour qui veut un gestionnaire de paquets : c'est un
dépôt ordinaire sous notre contrôle, sans fork ni revue extérieure.

### Fichiers à modifier

| Fichier | Changement |
| --- | --- |
| `.github/workflows/release.yml:779-906` | supprimer le job `winget` (aucun autre job n'en dépend) |
| `.github/workflows/ci.yml:237-300` | job `packaging-win` : ne valider que Scoop, retirer les schémas winget-pkgs |
| `packaging/winget/` | supprimer les trois gabarits `.yaml.in` |
| `packaging/render.sh` | ne rendre que `scoop/` |
| `packaging/tests/run-tests.sh`, `validate-schemas.py` | retirer les cas WinGet |
| `packaging/README.md`, `RELEASING.md:109,158,192-202` | retirer WinGet de la documentation |
| `../../typr.github.io/docs/reference/installation.md` | retirer `winget install we-data-ch.TypR` (autre dépôt) |
| secret `WINGET_CREATE_GITHUB_TOKEN` | supprimer du dépôt (`gh secret delete WINGET_CREATE_GITHUB_TOKEN`) |

Le fork `fabriceHategekimana/winget-pkgs` n'est plus utile et peut être
supprimé. Si une PR WinGet d'une release précédente est encore ouverte sur
`microsoft/winget-pkgs`, la fermer.

## 5. Reprise sans re-taguer

Le tag `v0.6.2` est déjà publié : la release ne doit pas être recréée. Le workflow
expose `workflow_dispatch` précisément pour relancer les canaux de distribution.

1. Appliquer les correctifs 2 et 3, et le retrait de WinGet (4), sur `develop`.
2. PR `develop` → `main`, fusionner.
3. Relancer la release avec le workflow de `main` :

```sh
gh workflow run Release -f tag=v0.6.2
```

Les étapes déjà réussies sont idempotentes (crates.io vérifie la présence de la
version, la release et ses assets existent déjà). Le binaire Windows de
`v0.6.2` est déjà dans la release : `install.ps1` peut l'installer dès
maintenant, sans attendre la relance.

## 6. Remarque annexe — message trompeur de `publish.nu`

`.github` n'est pas en cause ici, mais `publish.nu:258-261` compare les SHA par
égalité :

```nu
let local = (git rev-parse HEAD ...)
let remote = (git rev-parse origin/develop ...)
if $local != $remote { error make { msg: "... divergent — `git pull` d'abord" } }
```

Un simple commit local non poussé (donc en avance, pas divergent) déclenche ce
message et invite à tort à faire `git pull`. Il devrait distinguer :

- en retard : `git rev-list --count HEAD..origin/develop` > 0 → demander `pull` ;
- en avance : `git rev-list --count origin/develop..HEAD` > 0 → demander `push` ;
- divergence réelle : les deux > 0 → demander une résolution.
