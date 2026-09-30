# Plan — installation de TypR en une commande

Objectif : qu'un utilisateur choisisse **un seul canal** parmi quatre, sans
compter sur Cargo, Docker ou un téléchargement manuel depuis la page des
releases.

Ce plan part de l'existant : les 6 binaires, `checksums.txt` et la CI de release
sont déjà en place. Il ne s'agit donc que d'ajouter la couche de distribution
par-dessus, pas de refaire ce qui marche.

---

## 1. État actuel

`.github/workflows/release.yml` produit, à partir d'un unique tag `vX.Y.Z` :

| Cible | Artefact |
|---|---|
| `x86_64-unknown-linux-gnu` | `typr-vX.Y.Z-x86_64-unknown-linux-gnu.tar.gz` |
| `aarch64-unknown-linux-gnu` | `typr-vX.Y.Z-aarch64-unknown-linux-gnu.tar.gz` |
| `x86_64-pc-windows-msvc` | `typr-vX.Y.Z-x86_64-pc-windows-msvc.zip` |
| `aarch64-pc-windows-msvc` | `typr-vX.Y.Z-aarch64-pc-windows-msvc.zip` |
| `x86_64-apple-darwin` | `typr-vX.Y.Z-x86_64-apple-darwin.tar.gz` |
| `aarch64-apple-darwin` | `typr-vX.Y.Z-aarch64-apple-darwin.tar.gz` |

Plus `checksums.txt` au format `sha256sum` (`release.yml:157`), et une version
stable (`X.Y.Z`) et une version instable (`X.Y.Z-alpha.N`) qui est marquée
`prerelease` (`release.yml:169`).

Ce qui manque est uniquement l'agent qui transforme ces fichiers en une
commande. Le reste du plan est de la distribution, pas de la compilation.

---

## 2. Décisions

### Ce qu'on fait

| Canal | Commande | OS |
|---|---|---|
| Script Unix | `curl -fsSL …/install.sh \| sh` | macOS, Linux |
| Script PowerShell | `irm …/install.ps1 \| iex` | Windows |
| Homebrew | `brew install we-data-ch/typr/typr` | macOS, Linux |
| WinGet | `winget install we-data-ch.TypR` | Windows |
| Scoop | `scoop install typr` | Windows |

### Ce qu'on ne fait pas, et pourquoi

- **`.msi` / `.pkg` (installeur graphique)** — `.pkg` macOS non notarisé produit
  « TypR est endommagé » pour tout le monde ; la notarisation exige un compte
  Apple Developer payant. Un binaire posé par `curl | sh` **n'a pas** l'attribut
  `com.apple.quarantine` (seuls les navigateurs le posent) et passe sans
  friction. Un `.msi` non signé se heurte au même mur avec SmartScreen. Le seul
  cas qui le justifierait est un déploiement d'entreprise par GPO — à rouvrir
  si ce cas se présente.
- **Code signing / notarization des binaires** — même raison, hors de portée
  d'un projet open source sans certificat payant.
- **Wrapper npm** (`npm i -g typr`) — la technique esbuild/biome fonctionne
  partout, mais impose Node à l'utilisateur. L'audience TypR (R, data) n'a pas
  Node acquis. À réexaminer plus tard, pas comme canal principal.

### Deux décisions à confirmer

Les questions n'ont pas encore reçu de réponse ; le plan part sur les valeurs
par défaut ci-dessous.

1. **Emplacement** — `~/.local/bin` (défaut retenu). Zéro `sudo`, aucun
   privilège. L'installateur affiche un avertissement si le dossier n'est pas
   dans le `PATH` au lieu d'échouer. Homebrew reste le canal « propre » pour qui
   veut une installation système.
   *Alternative :* `/usr/local/bin` avec `sudo`, plus proche des conventions
   mais qui casse les installations en CI et dans les conteneurs.

2. **Hébergement des scripts** — GitHub Pages sur le dépôt de documentation
   existant (`we-data-ch.github.io/typr.github.io/install/install.sh`), ce qui ne
   coûte aucune infrastructure. Un vrai domaine (`typr.sh`) est plus joli mais
   demande DNS et redirection ; à faire plus tard si le besoin se fait sentir.

---

## 3. Points de vigilance avant de commencer

### 3.1 Le glibc des binaires Linux peut être trop récent

C'est le risque principal, et il est invisible jusqu'au premier utilisateur.

Les binaires GNU sont compilés sur `ubuntu-latest` (actuellement Ubuntu 24.04,
glibc 2.39). Un binaire Rust y lié exigera une glibc **aussi récente** chez
l'utilisateur. Sur une Ubuntu 22.04 (glibc 2.35) ou une Debian 12 (glibc 2.36),
le démarrage échoue sur `GLIBC_2.38 not found` — ce qui donne exactement
l'impression d'un binaire cassé.

Vérification à faire immédiatement sur une release existante :

```bash
objdump -T target/release/typr | grep -o 'GLIBC_[0-9.]*' | sort -Vu | tail -3
```

Si le maximum dépasse glibc 2.35, deux options :

- **recommandée** — compiler `x86_64-unknown-linux-gnu` sur `ubuntu-22.04` dans
  la matrice de `release.yml:77`. Ça abaisse le plancher sans changer
  grand-chose, au prix d'un toolchain plus vieux.
- **alternative** — ajouter des cibles `*-unknown-linux-musl`, statiquement
  liées, qui fonctionnent partout y compris sur Alpine. Résout le problème de
  glibc **et** le cas Alpine, pour deux cibles de plus dans la matrice.

### 3.2 Résoudre « latest » sans dépendance

Les scripts ne doivent pas supposer `jq` ni `python` sur la machine.

L'astuce retenue : l'API GitHub demande `jq`, mais l'URL
`https://github.com/<repo>/releases/latest` **redirige** vers `/tag/vX.Y.Z`.
Il suffit de lire l'en-tête `Location` :

```bash
curl -fsSI https://github.com/we-data-ch/typr/releases/latest \
  | grep -i '^location:' | sed 's|.*/tag/||' | tr -d '\r\n'
```

Deux conséquences utiles :

- `/releases/latest` **exclut** les prereleases — exactement la sémantique de la
  branche stable, alignée sur `prerelease` dans `release.yml:169`. Le canal
  beta devra, lui, lister `/releases` et filtrer.
- Aucune dépendance à un outil de parsing.

### 3.3 Vérifier le SHA-256

`checksums.txt` est déjà au format attendu par `sha256sum -c`. Attention : il
contient des noms de fichiers nus, donc la commande doit tourner **depuis le
répertoire** contenant le fichier téléchargé.

L'option `TYPR_INSTALL_VERIFY=0` permet de sauter la vérification pour un
dépannage réseau, sans jamais être le mode par défaut.

### 3.4 Récupérer le binaire après un échec

`install.sh` télécharge dans un fichier temporaire, vérifie le SHA, puis déplace.
En cas d'échec le fichier temporaire est nettoyé : on n'installe jamais un binaire
partiellement téléchargé.

### 3.5 Le job `coherence` interdit les binaires commités

Le job de CI échoue si un binaire réapparaît dans l'index (`RELEASING.md:157`).
Les scripts ne doivent installer que dans `$HOME`, jamais dans le dépôt, et le
nouveau dossier `install/` ne doit contenir aucun artefact — uniquement du texte.

---

## 4. Phase 1 — Scripts d'installation

Socle : les trois canaux suivants ne font que résoudre une URL et lire un SHA.
Tout ce qui est commun à Unix et Windows (résolution de version, choix du
canal, message de fin) vit dans les scripts, pas dans la CI.

### Livrables

```
install/install.sh      # POSIX sh, Linux + macOS
install/install.ps1     # PowerShell, Windows
install/README.md       # usage et dépannage, destiné aux contributeurs
```

### `install/install.sh`

- `#!/bin/sh`, `set -eu`, aucune dépendance hors `curl`/`tar`/`sha256sum`
  (macOS fournit `shasum -a 256`, pas `sha256sum` : les deux doivent être
  Tentés).
- Résolution de la cible par `uname -s` et `uname -m`, avec correspondance
  explicite `uname -m` → triple Rust. Sous Rosetta, `uname -m` renvoie `x86_64`
  sur une machine Apple Silicon, et le binaire Intel y fonctionne — aucun cas
  particulier n'est nécessaire.
- Support d'arguments : `--version <tag>` pour épingler, `--channel <stable|beta>`,
  `--dry-run`, `--help`.
- Détection du dossier cible : `$TYPR_INSTALL_DIR`, sinon `$HOME/.local/bin`.
- Si le dossier cible n'est pas dans le `PATH`, afficher la ligne à ajouter et le
  `~/.profile`, sans échouer.
- Message final indiquant `typr --version` et le lien vers la documentation.

### `install/install.ps1`

- Cible `%LOCALAPPDATA%\Programs\typr` — convention utilisateur moderne, aucun
  droit administrateur. Surchargeable par `$env:TYPR_INSTALL_DIR`.
- `irm … | iex` :PowerShell impose de rester sur une seule ligne dans le
  message du README, et d'afficher l'URL avant exécution.
- Extraction via `Expand-Archive`, donc format `.zip` et non `.tar.gz`.
- Ajout au `PATH` utilisateur par registre
  (`[Environment]::SetEnvironmentVariable('Path', …, 'User')`) — qui ne prend
  effet qu'au prochain démarrage du terminal. Le script doit le dire
  explicitement, **et** mettre à jour `$env:Path` de la session courante pour
  que `typr --version` fonctionne tout de suite dans le même terminal.

### Vérification

Tester au minimum, pour chaque OS et chaque architecture disponible :

```bash
# Unix
TYPR_INSTALL_DIR=$(mktemp -d) ./install/install.sh --dry-run
TYPR_INSTALL_DIR=$(mktemp -d) ./install/install.sh
$TYPR_INSTALL_DIR/typr --version
```

- [ ] Linux x86_64, Linux aarch64
- [ ] macOS x86_64, macOS aarch64
- [ ] Windows x86_64 via `irm | iex`
- [ ] `--version <tag-ancienne>` installe bien l'ancienne version
- [ ] SHA volontairement corrompu → le script refuse d'installer et le dit
- [ ] `PATH` non modifiable → message clair, pas d'erreur opaque
- [ ] Idempotence : deux exécutions successives ne cassent rien

---

## 5. Phase 2 — Homebrew

Le canal le plus attendu sur macOS, et il couvre Linux au passage.

### Livrables

Dépôt `we-data-ch/homebrew-typr` :

```
README.md              # généré à partir de la formule
Formula/typr.rb        # la seule source
```

### Contenu de la formule

```ruby
class Typr < Formula
  desc "A typed superset of R — transpiler and type checker"
  homepage "https://we-data-ch.github.io/typr.github.io/"
  url "https://github.com/we-data-ch/typr/releases/download/vX.Y.Z/typr-vX.Y.Z-x86_64-apple-darwin.tar.gz"
  version "X.Y.Z"
  sha256 "<omis ici : rempli par la CI>"

  on_macos do
    if Hardware::CPU.arm?
      url "…/typr-vX.Y.Z-aarch64-apple-darwin.tar.gz"
      sha256 "…"
    end
  end

  def install
    bin.install "typr"
  end

  def caveats
    "typR is pre-1.0: version 0.x releases can change CLI surface."
  end
end
```

- Homebrew gère le `PATH`, les mises à jour (`brew upgrade`) et le
  désinstallateur : le script Unix n'a plus à s'en préoccuper pour ce canal.
- **Le `sha256` doit être rempli automatiquement**, jamais écrit à la main.
  Extrapoler `release.yml` avec un job `brew` qui lit `checksums.txt` — le
  fichier contient déjà la ligne correspondant à l'artefact macOS, il n'y a rien
  à recalculer — et commite la formule mise à jour.
- Avec `livecheck`, Homebrew suit le tag et propose la mise à jour tout seul.

### Vérification

- [ ] `brew install --formula we-data-ch/typr/typr` depuis un Mac clean
- [ ] Fonctionne sur Apple Silicon **et** Intel
- [ ] `brew upgrade typr` passe à la bonne version
- [ ] `brew audit --strict` ne signale rien
- [ ] Le SHA publié correspond à `checksums.txt` de la release

---

## 6. Phase 3 — WinGet et Scoop

Deux manifests JSON, aucun code. Ils puisent dans les `.zip` déjà publiés.

### Livrables

```
packaging/winget/we-data-ch.TypR.yaml         (+ .installer.yaml si signature)
packaging/scoop/typr.json
```

### WinGet

- `PackageIdentifier: we-data-ch.TypR`
- `PackageVersion` sans le `v` initial — WinGet l'exige.
- `InstallerType: zip`, `InstallerSwitches: Silent` et `SilentWithProgress` avec
  `/S` pour `7z`… **à ne pas inventer** : le type `zip` d'WinGest ekstré sans
  copie dans un dossier.
- Le chemin `InstallLocation` doit être `%LOCALAPPDATA%\Programs\typr`, pour
  coïncider avec ce que fait `install.ps1`.
- Le fichier doit être publié dans `microsoft/winget-pkgs` par une pull request
  ouverte automatiquement depuis la CI (action
  `microsoft/winget-create` ou un script équivalent).

### Scoop

Le manifest est plus court que celui de WinGet et ne demande pas de PR
maintenue manuellement. `scoop install typr` après
`scoop bucket add typr https://github.com/we-data-ch/scoop-bucket`.

Dans les deux cas, la version publiée doit venir de la même source que le reste —
`needs.verify.outputs.version` — et non d'un fichier écrit à la main.

### Vérification

- [ ] `winget install we-data-ch.TypR` sur Windows 10 et 11
- [ ] `scoop install typr` puis `scoop update typr`
- [ ] Les deux installers aboutissent au même dossier et au même binaire que
      `install.ps1`
- [ ] Une machine sans rights administrateur réussit l'installation

---

## 7. Phase 4 — Documentation

### README

Remplacer le tableau `Install` (README.md:62-69) :

```markdown
## Install

| Plateforme | Commande |
|---|---|
| **Toutes** | `curl -fsSL https://we-data-ch.github.io/typr.github.io/install/install.sh \| sh` |
| **Windows** | `irm https://we-data-ch.github.io/typr.github.io/install/install.ps1 \| iex` |
| macOS / Linux (Homebrew) | `brew install we-data-ch/typr/typr` |
| Windows (WinGet) | `winget install we-data-ch.TypR` |
| Windows (Scoop) | `scoop install typr` |
| Cargo | `cargo install typr` |
| Docker | `docker run --rm -it fabricehategekimana/typr:latest` |
| RStudio / Positron | `typr.runner_*.tar.gz` — release |
| VS Code / Positron | extension **TypR** dans le Marketplace |
| Vim / Neovim | `typr-vim-*.tar.gz` — release, ou un plugin manager |
```

Faire figurer en tête les deux lignes universelles : ce sont elles que l'on veut
que la personne copie.

### Page d'installation du site

Créer `docs/install.md` (ou l'équivalent dans le dépôt `typr.github.io`) avec,
dans cet ordre : la commande unique, le choix du canal, la vérification du
SHA-256, la mise à jour, le dépannage (PATH non modifié, Gatekeeper,
SmartScreen, architecture non supportée).

Préciser explicitement que l'installation **n'exige pas `sudo`** et
**n'écrit rien hors de `$HOME`** — c'est ce qui lève la principale réticence.

### Vérification

- [ ] Les commandes du README sont copiées et exécutées telles quelles
- [ ] `RELEASING.md` mentionne les nouveaux canaux dans le diagramme
- [ ] `publish.nu check`compare bien les quatre canaux (voir ci-dessous)

---

## 8. Phase 5 — `typr upgrade` (optionnelle)

À ne faire que si les releases sont fréquentes ; sinon le script et Homebrew
suffisent.

Le binaire serait `typr upgrade` : même logique de résolution de version et de
vérification de SHA que les scripts, mais à l'intérieur du programme — donc testée
par `cargo test` plutôt que par un script shell.

Le risque est la duplication entre le code Rust et les scripts. Le traiter en
conséquence : soit le script devient le client d'un sous-commande `typr install`,
soit la logique est factorisée dans une petite bibliothèque partagée.

---

## 9. CI

### Étapes dans `release.yml`

```
tag vX.Y.Z
   │
   ├─ verify … verify … release …
   │                  ├─ brew ……… commit de la formule mise à jour
   │                  ├─ winget … PR vers microsoft/winget-pkgs
   │                  └─ scoop … commit du manifest
   └─ …
```

Ces jobs restent **indépendants** des autres canaux, comme le sont déjà
`vscode` et `vim` : un échec de publication Homebrew ne doit pas empêcher les
binaires d'arriver. C'est la règle que `RELEASING.md:108` énonce déjà.

### Étapes dans `ci.yml`

- Job `install`, sur une PR : `bash -n install/install.sh`, `sh -n`, et
  exécution réelle avec `--dry-run`.
- Analyse avec `shellcheck` pour `install.sh`, `PSScriptAnalyzer` pour
  `install.ps1`.
- Pour WinGet et Scoop, valider le YAML/JSON contre leur schéma.
- Vérifier que `install/` ne contient aucun binaire — le job `coherence`
  l'interdit déjà pour l'ensemble du dépôt.

### `publish.nu`

Étendre `check` pour interroger l'API Homebrew et WinGet et confirmer que la
version publiée est bien celle du tag. Sans cela, un canal en retard devient
invisible : les autres canaux sont mis à jour au tag, lui seul dériverait en
silence.

---

## 10. Ordre d'exécution

| # | Lot | Dépend de | Effort |
|---|---|---|---|
| 0 | Diagnostic glibc + décision cible musl | — | 1 h |
| 1 | `install.sh` + `install.ps1` + hébergement | 0 | 1 j |
| 2 | Tests CI sur les scripts | 1 | 3 h |
| 3 | Homebrew tap + job CI | 1 | 1 j |
| 4 | WinGet + Scoop + PR automatique | 1 | 1 j |
| 5 | README, page d'installation, `RELEASING.md` | 1-4 | 3 h |
| 6 | `publish.nu check` étendu | 1-4 | 2 h |
| 7 | `typr upgrade` (optionnel) | — | plus tard |

Le lot 0 conditionne le lot 1 : publier un script d'installation qui installe un
binaire ne démarre pas sur Ubuntu 22.04 transformerait le canal principal en
source de tickets. C'est un diagnostic d'une heure qui évite ce risque.

Les lots 3 et 4 sont indépendants de 2 et parallélisables.

---

## 11. Risques

| Risque | Impact | Réponse |
|---|---|---|
| glibc des binaires Linux trop récent | Installation qui échoue sur les distributions anciennes | Lot 0, puis rebuild sur `ubuntu-22.04` ou cibles musl |
| SmartScreen bloque `typr.exe` | Avertissement à l'insertion, pas un blocage | Signatures `install.ps1` volontairement absentes : PowerShellSmartScreen marque le fichier téléchargé, un fichier posé par script ne l'est pas |
| Gatekeeper sur macOS | Rare pour un binaire installé par script | Documenter `xattr -d com.apple.quarantine` pour ceux qui téléchargent d'abord par navigateur |
| Un canal dérive en silence | Le README promet une version absente d'un canal | `publish.nu check` étendu (lot 6) |
| Le script d'installation casse les gens | Mauvaise expérience sur le canal principal | `--dry-run`, `--version` épinglé, idempotence, tests sur 6 cibles en CI |

---

## 12. hors périmètre

- Signature et notarification (coût, hors budget d'un projet open source).
- Canal de type `winget` : trop tôt, la distribution de Linux est trop
  fragmentée pour que le gain justifie l'effort.
- Mise à jour automatique silencieuse, qui crée plus de problèmes qu'elle n'en
  résout (mise à jour pendant une session LSP en cours, binaire remplacé sous
  les pieds du processus). `typr upgrade` explicite suffit.
- Paquet `apt` / `dnf` : le script Unix couvre ces distributions sans
  fragmentation supplémentaire.