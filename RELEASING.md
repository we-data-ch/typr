# Publier une version de TypR

Un seul geste déclenche toute la distribution : **pousser un tag `vX.Y.Z`**.
Tout le reste est fait par `.github/workflows/release.yml`.

## Source de vérité

`[workspace.package] version` dans `Cargo.toml`. Rien d'autre ne porte de version
de référence — l'extension VS Code et le runner RStudio en sont dérivés par
`nu publish.nu sync`, et le playground lit celle que la CI écrit à côté du WASM.

La CI **refuse** de publier si le tag, `Cargo.toml`, `editors/vscode/package.json`
et `editors/rstudio/DESCRIPTION` ne disent pas tous la même chose (job `verify`),
et le même contrôle tourne sur chaque PR (job `coherence` de `ci.yml`).

## Procédure

```bash
git switch develop
nu publish.nu ship patch      # ou minor / major
```

C'est tout. `ship` enchaîne la montée de version, la pull request, l'attente de
la CI, la fusion, le tag et la resynchronisation de `develop`.

Il s'interrompt **une seule fois**, quand les checks sont verts et juste avant
de fusionner :

```
Prêt à fusionner la PR #22 et à poser v0.5.10.
Le tag déclenche la publication sur crates.io, où un numéro ne peut plus être repris.
Continuer ? [o/N]
```

C'est le dernier moment réversible. Répondre autre chose que `o` laisse la PR
ouverte et n'a rien publié ; `nu publish.nu ship --resume` reprend plus tard.

Même chose si la CI échoue : rien n'est fusionné ni tagué, la PR reste ouverte,
on corrige sur `develop` et on relance avec `--resume`.

Ajouter `--yes` supprime la confirmation, pour un usage non interactif. À éviter
autrement : c'est la seule barrière avant l'irréversible.

### Ce que ship fait, dans l'ordre

1. vérifie que `gh` est authentifié, qu'on est sur `develop`, que l'arbre est
   propre et que `develop` est à jour ;
2. fusionne `main` dans `develop` — sans ça la PR embarquerait une régression ;
3. `bump`, qui propage la version à l'extension VS Code et au runner RStudio ;
4. `cargo check --workspace`, qui met `Cargo.lock` à jour. **En cas d'échec la
   version est restaurée** : un numéro qui ne compile pas ne reste pas dans
   l'arbre de travail ;
5. commit, push, ouverture de la PR ;
6. attente des checks obligatoires ;
7. *confirmation* ;
8. fusion, puis `release` — qui refait ses quatre gardes, volontairement en
   double : c'est la dernière barrière ;
9. `develop` remis au niveau de `main`.

### À la main

L'enchaînement reste décomposable, ce qui est utile pour reprendre une release
partiellement passée :

```bash
git switch develop && git pull
nu publish.nu bump patch
cargo check --workspace
git commit -am "release v0.5.10" && git push origin develop
gh pr create --base main --head develop --fill
gh pr merge --merge --delete-branch=false

git switch main && git pull
nu publish.nu release --dry-run
nu publish.nu release

git switch develop && git merge main && git push origin develop
```

`release` ne pousse **que le tag** : `main` est protégée, son contenu n'y arrive
que par pull request. S'il détecte que `main` locale diverge de `origin/main`,
il s'arrête plutôt que de taguer dans le vide.

Un numéro de version déjà publié ne se réutilise pas. `release` refuse un tag
existant en local comme sur le distant, et `cargo publish` refuserait de toute
façon d'écraser une version présente sur crates.io.

`nu publish.nu check` compare ensuite ce qui est réellement publié sur chaque canal.

## Ce que la CI fait à partir du tag

```
tag vX.Y.Z
   │
   ├─ verify ......... refuse si tag ≠ Cargo.toml ≠ éditeurs
   │
   ├─ build .......... 6 cibles (Linux/Windows/macOS × x86_64/aarch64)
   │   └─ release .... GitHub Release + checksums SHA-256
   │        ├─ docker ..... image depuis le binaire Linux, tags X.Y.Z + latest
   │        ├─ rstudio .... tarball R avec les binaires frais embarqués
   │        └─ docs ....... repository_dispatch → reconstruction du site
   │
   ├─ crates-io ...... typr-core → typr-lsp → typr-cli → typr
   ├─ wasm ........... WASM + version.json → dépôt du playground
   └─ vscode ......... .vsix → Marketplace + attaché à la release
```

Les branches sont indépendantes : si le Marketplace échoue, les binaires et
crates.io sont quand même publiés. Relancer une branche seule se fait via
`workflow_dispatch` en fournissant le tag, sans re-taguer.

## L'ordre de crates.io n'est pas négociable

`typr-cli` déclare `typr-lsp = { path = ..., version = "0.5" }`. Une dépendance
`path` **portant un numéro de version** exige que le crate existe sur le registre
au moment du `cargo publish`. `typr-lsp` doit donc être publié avant `typr-cli`,
lui-même avant `typr`.

C'est précisément ce qui avait bloqué la chaîne : `typr-lsp` n'ayant jamais été
publié, `typr-cli` et `typr` sont restés en 0.5.5 pendant que `typr-core` passait
en 0.5.7 et que les releases GitHub annonçaient 0.5.7.

## Secrets requis

| Secret | Utilisé par | Où l'obtenir |
|---|---|---|
| `CARGO_REGISTRY_TOKEN` | `crates-io` | crates.io → Account Settings → API Tokens |
| `DOCKERHUB_USERNAME` | `docker` | nom de compte Docker Hub |
| `DOCKERHUB_TOKEN` | `docker` | Docker Hub → Account Settings → Personal access tokens |
| `VSCE_PAT` | `vscode` | Azure DevOps → PAT, scope *Marketplace: Manage* |
| `PLAYGROUND_DEPLOY_TOKEN` | `wasm` | PAT GitHub, scope `repo` sur le dépôt du playground |
| `DOCS_DISPATCH_TOKEN` | `docs` | PAT GitHub, scope `repo` sur `typr.github.io` |

## Aucun binaire dans le dépôt

Les binaires du runner RStudio sont assemblés par la CI au moment de construire
le tarball, jamais commités. L'ancien dépôt `typr_runner` pesait 140 Mo de `.git`
pour 100 Ko de source, à cause de 33 commits « [auto] updated binaries ».
Le job `coherence` de `ci.yml` échoue si un binaire réapparaît dans l'index.

## Développement local

```bash
nu deploy.nu               # binaire de debug → ~/sh/typr
./build-wasm.sh            # WASM → playground voisin, avec version.json
nu docker/deploy.nu        # image locale depuis le binaire release
```
