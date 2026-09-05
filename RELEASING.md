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
git switch main && git pull

nu publish.nu bump patch      # ou minor / major — synchronise les éditeurs
cargo check --workspace       # met Cargo.lock à jour

git add -A
git commit -m "release v$(nu publish.nu version | head -1)"

nu publish.nu release --dry-run   # vérifie branche, propreté, tag
nu publish.nu release             # tag + push → la CI prend le relais
```

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
