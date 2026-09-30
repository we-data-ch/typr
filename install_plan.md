# Plan — installation de TypR en une commande

Objectif : qu'un utilisateur choisisse **un seul canal** parmi quatre, sans
compter sur Cargo, Docker ou un téléchargement manuel depuis la page des
releases.

Ce plan part de l'existant : les 6 binaires, `checksums.txt` et la CI de release
sont déjà en place. Il ne s'agit donc que d'ajouter la couche de distribution
par-dessus, pas de refaire ce qui marche.

---

## 0. Avancement

| Lot | État |
|---|---|
| 0a — cibles musl dans la matrice | **fait** |
| 0b — `+crt-static` sur `*-msvc` | **fait** (liaison vérifiée en CI, pas en local) |
| 0c — binaire musl dans Rocky 9 / Debian 12 / Alpine | **fait** |
| 1 — scripts d'installation + hébergement + CI | **fait** (voir ci-dessous) |
| 2 — tests CI sur ces scripts | **couvert par le lot 1** — les jobs `install`, `install-windows` et `install-hosting` sont ceux-là |
| 3 — Homebrew | **fait** (voir ci-dessous) |
| 4 — WinGet et Scoop | **fait** (voir ci-dessous) |
| 5 → 7 | à faire |

### Ce que le lot 0 a changé

`.github/workflows/release.yml` est passé de 6 à 8 cibles.

- `x86_64-unknown-linux-musl` et `aarch64-unknown-linux-musl` sont entrés dans
  la matrice, construits par `cross` comme `aarch64-unknown-linux-gnu` l'était
  déjà. Plus de cible `*-sys` : le binaire est `static-pie linked`, sans
  `PT_INTERP`, et `objdump -T | grep -c GLIBC_` renvoie **0**.
- Les deux cibles GNU sont sorties de `ubuntu-latest` pour `ubuntu-22.04`. Le
  plancher de glibc de la voie de repli est ainsi figé à 2.35 au lieu de monter
  tout seul à ~2.42 en novembre 2026 avec le runner.
- Les deux cibles `*-msvc` reçoivent `-C target-feature=+crt-static` par
  `RUSTFLAGS`. Comme `--target` est toujours passé, cargo n'applique pas la
  variable aux build scripts ; le crate C embarqué (oniguruma, via `syntect`)
  est donc compilé en `/MT` et non en `/MD` — `cc` lit `crt-static` dans
  `CARGO_CFG_TARGET_FEATURE` pour choisir. Sans cela, le runtime du
  VC++ Redistributable revenait par la porte du code C.

### Mesures (30 septembre 2026)

Binaire `x86_64-unknown-linux-musl` construit par `cross`, profil `release` :

| Système | Avant (GNU) | Après (musl) |
|---|---|---|
| Rocky Linux 9 (glibc 2.34) | `GLIBC_2.39 not found` | `typr-cli 0.5.12` |
| Debian 12 (glibc 2.36) | `GLIBC_2.39 not found` | `typr-cli 0.5.12` |
| Ubuntu 22.04 (glibc 2.35) | `GLIBC_2.39 not found` | `typr-cli 0.5.12` |
| Alpine 3.20 (musl) | non exécutable | `typr-cli 0.5.12` |
| Debian 13, Ubuntu 24.04 | `typr-cli 0.5.12` | `typr-cli 0.5.12` |

`typr check` produit des diagnostics identiques à ceux du binaire glibc sur le
même fichier : le comportement n'a pas bougé, seul le lien a changé.

### Deux garde-fous dans la CI

La propriété qui fait marcher ces binaires — la liaison statique — est
invisible dans le code : rien ne la casse quand elle disparaît, et c'est
précisément le mode de défaillance qu'on cherche à supprimer. Deux étapes
l'interdisent désormais.

- `Runtime C statique (Windows)` lit la table d'imports PE avec `dumpbin` et
  échoue si `VCRUNTIME140`, `MSVCP140` ou un `api-ms-win-crt-*` réapparaît.
- `Binaire musl statique et exécutable` échoue si le binaire musl déclare un
  `PT_INTERP` — un binaire musl qui trahirait son interpréteur ne tournerait
  que sur Alpine — puis lance `typr --version` dans `rockylinux:9`,
  `debian:12` et `alpine:3.20`.

La seconde remplace la checklist de la section 4 : le test qui compte n'a pas
lieu d'être fait à la main une fois, mais à chaque release.

### Reste à confirmer en CI

La liaison `+crt-static` sur `x86_64-pc-windows-msvc` reste **non vérifiée
localement** : ni le linker MSVC ni le SDK ne sont disponibles ici. `rustc`
accepte le flag pour les deux cibles `*-msvc` (vérifié jusqu'à l'émission des
métadonnées), et le garde-fou `dumpbin` tranchera à la prochaine release.

### Ce que le lot 1 a livré

Sept fichiers sous `install/`, tous du texte, aucun binaire.

| Fichier | Contenu |
|---|---|
| `install/install.sh` | `sh` POSIX : musl par défaut, GNU sur `--gnu`, SHA-256, stable/beta |
| `install/install.ps1` | PowerShell 5.1 et 7 : `.zip`, SHA-256, PATH par registre |
| `install/README.md` | usage, options, codes de sortie, dépannage |
| `install/tests/run-tests.sh` | 59 assertions contre un serveur de fixtures |
| `install/tests/run-tests.ps1` | 49 assertions, plus PSScriptAnalyzer |
| `install/tests/fixture-server.py` | `/releases/latest`, `releases.atom`, archives, `checksums.txt` |
| `install/tests/Dockerfile` | image PowerShell 7.4 + PSScriptAnalyzer + shellcheck |

Trois jobs de CI : `install` (shellcheck, `dash -n`, les deux suites),
`install-windows` (le script sous Windows PowerShell 5.1 réel), et
`install-hosting` (copie vers `typr.github.io/static/install/`).

**59 + 49 assertions passent** ; shellcheck est propre sur les deux scripts
shell. Le vrai test d'installation a réussi en local :
`TYPR_INSTALL_DIR=$(mktemp -d) ./install/install.sh --gnu --version v0.5.12`
puis `typr-cli 0.5.12`.

Cinq défauts réels sont sortis des tests, tous invisibles à la lecture :

- `Try-Save-File` rendait `$null` — donc falsy — sur un téléchargement réussi,
  parce qu'une fonction PowerShell renvoie ce que son `try` a émis, et un
  succès n'émet rien. Chaque installation Windows échouait.
- `$input` est une variable automatique PowerShell. Y écrire pouvait faire lire
  un `$null` à `CopyTo` et rendre le téléchargement silencieusement infructueux.
- Le fichier n'avait pas de BOM UTF-8. PowerShell 5.1 lit un `.ps1` sans BOM
  en ANSI avec la page de code de la machine : sur un Windows français ou
  allemand, chaque accent de ce fichier devenait illisible. Le BOM est
  maintenant testé explicitement, sinon un reformatage l'effacerait en silence.
- Une ligne `checksums.txt` au SHA malformé était signalée « aucune ligne pour
  cette archive » au lieu de « ligne malformée ». Deux diagnostics très
  différents — release mal publiée contre fichier corrompu — se confondaient.
  `install.sh` et `install.ps1` nomment désormais la même cause.
- Le harnais de test perdait les codes de retour : `& script.ps1` ne propage
  pas le `exit` du script appelé au processus. Le refus d'un tag illisible (2)
  était indiscernable d'un refus d'environnement (1).

Deux points restent **non vérifiés localement**, et la CI les couvre sans
pretendre le contraire : la liaison `+crt-static` (ci-dessus) et le démarrage
d'un binaire Windows réel — le runner n'a pas de VC++ Redistributable, et la
fixture n'est de toute façon pas un exécutable Windows.

### Ce que le lot 3 a livré

La formule, ses deux variantes et son générateur — sous `packaging/homebrew/`,
toujours aucun binaire dans le dépôt.

| Fichier | Contenu |
|---|---|
| `Formula/typr.rb.in` | `desc`, `homepage`, `license`, `livecheck`, `on_macos` |
| `parts/on_linux.rb.in` | le bloc `on_linux` musl, inséré seulement s'il existe |
| `parts/on_linux_absent.rb.in` | le commentaire qui le remplace sinon |
| `README.md.in` | le README du tap ; sa description et son URL sont **relues dans la formule rendue**, pas recopiées |
| `render.sh` | `sh` POSIX : tag + `checksums.txt` → formule + README |
| `tests/run-tests.sh` | 81 assertions, sans réseau ni brew |
| `README.md` | le mode d'emploi du canal |

`render.sh` ne fait qu'une chose, et c'est le cœur du lot : **lire les
empreintes dans le `checksums.txt` de la release**. La formule est donc
impossible à publier avec une empreinte recopiée — la seule façon de l'obtenir
est de modifier le générateur, qui est relu et testé à chaque PR.

Le tag est pris avec ou sans `v` initial ; `--checksums` accepte une URL ou un
fichier local ; **rien n'est écrit** dans `--out` si le rendu échoue en cours de
route, pour qu'une formule tronquée ne puisse pas être commitée par erreur.

Deux jobs de CI. `packaging` tourne sous Linux : shellcheck, `dash -n`,
`bash -n`, la suite. `packaging-audit` tourne sous macOS et fait passer la
formule rendue à `brew style` et `brew audit --strict` — sur une PR, donc, et
non à la release. C'est le seul endroit où le DSL Homebrew de la formule peut
être jugé, aucun runner Linux n'ayant Homebrew.

Le job `brew` de `release.yml` reprend le même chemin sur le `checksums.txt`
réellement publié, **télécharge une archive et compare son empreinte à celle
inscrite dans la formule** avant de commiter vers `we-data-ch/homebrew-typr` —
là où une erreur de sélection se verrait enfin. Sans `HOMEBREW_TAP_TOKEN`, la
validation a lieu quand même et seule la publication est sautée, avec un
avertissement ; un refus de Homebrew arrête le job.

**81 assertions passent.** Cinq mutations du générateur ont été essayées pour
vérifier que la suite a vraiment du mordant — c'est elle qui l'affirme, pas la
relecture du gabarit :

| Mutation | Détectée par |
|---|---|
| les deux `sha256` macOS intervertis | les deux blocs `url`+`sha256`, vérifiés côte à côte |
| garde-fou des marqueurs `@@` supprimé | 18 assertions |
| cible GNU choisie pour Linux | 12 assertions |
| bloc `on_linux` désindentation | l'imbrication `on_linux` / `on_intel` |
| garde-fou du retour à la ligne final supprimé | 2 assertions |

La troisième a été trouvée en écrivant la suite : la version précédente
vérifiait que les quatre empreintes *apparaissaient* dans le fichier, ce que
satisfait aussi deux cibles interverties. La désindentation, elle, venait de la
normalisation du gabarit — invisible pour tout test qui ne compte pas les
espaces, et rattrapée par `brew style` dix minutes plus tard si l'on n'y prenait
garde.

**Ce que le lot n'a pas pu faire, et le dit.** Aucune version publiée ne contient
de binaire musl : `v0.5.12`, la dernière, n'a que six artefacts, tous GNU ou
Apple. Le générateur produit donc, pour elle, une formule **macOS seule**, et le
README du tap l'annonce au lieu de promettre Linux. Aucun repli GNU n'a été
ajouté : ce serait réintroduire, par la porte de Homebrew, le défaut
`GLIBC_2.39 not found` que le lot 0 vient de corriger partout ailleurs. Le bloc
`on_linux` réapparaîtra seul, à la première release qui publiera du musl.

Reste une seule chose à faire, et elle n'est pas dans ce dépôt :
`HOMEBREW_TAP_TOKEN` (PAT GitHub, scope `repo` sur `we-data-ch/homebrew-typr`)
dans les secrets de la CI. Tant qu'il manque, le job `brew` valide la formule et
saute la publication, avec un avertissement.

Le dépôt `we-data-ch/homebrew-typr` a été créé le 30 septembre 2026, vide puis
amorcé avec la formule v0.5.12 rendue depuis le `checksums.txt` de cette release
— `brew install we-data-ch/typr/typr` fonctionne donc dès aujourd'hui sur macOS,
et la prochaine release remplacera ce fichier par le sien, ce qui exercera le
chemin complet `release.yml` → tap.

### Ce que le lot 4 a livré

Les deux canaux sont rendus par **un seul script**, `packaging/render.sh`, au
niveau `packaging/` — et non deux sous `packaging/winget/` et `packaging/scoop/`
comme le plan les posait. La raison est celle qui vaut partout ailleurs dans ce
dépôt : les deux canaux lisent le **même** `checksums.txt` et doivent donc
pouvoir être cassés ensemble. Un générateur par canal laisserait passer le jour
où l'un des deux est régénéré et pas l'autre — et les deux canaux
désinstalleront la même version, avec des empreintes différentes.

```sh
packaging/render.sh --tag v0.5.12 --out /tmp/manifests   # 6 fichiers
```

| Fichier | Contenu |
|---|---|
| `packaging/winget/*.yaml.in` | version + installateurs (`x64`, `arm64`) + locale `en-US` |
| `packaging/scoop/typr.json.in` | manifeste Scoop |
| `packaging/scoop/README.md.in` | README du bucket, relu dans le manifeste |
| `packaging/tests/run-tests.sh` | 122 assertions, dont la validation par les schémas officiels |
| `packaging/tests/validate-schemas.py` | schémas WinGet et Scoop, en cache, `--offline` disponible |

Le job `packaging-win` de `ci.yml` rend depuis une fixture et fait valider par les
schémas officiels. Les jobs `winget` et `scoop` de `release.yml` rendent depuis le
`checksums.txt` de la release, **vérifient les deux archives téléchargées**, puis
publient : PR `winget-pkgs` via `wingetcreate submit` d'un côté, commit dans le
bucket de l'autre. Le job `scoop` va plus loin et **installe** : `scoop install`
sur le manifeste local, puis `typr --version` doit annoncer la version de la
release.

Trois défauts ont été trouvés en faisant, que rien ne montrait à la relecture.
Les trois sont couverts par la suite, et détaillés dans `packaging/README.md` :

- **`ReleaseDate` sans guillemets** — lu par un analyseur YAML 1.1, `2026-01-02`
  est une *date* ; le schéma WinGet exige une *chaîne*. 23 tests cassent si le
  guillemet disparaît.
- **Une clé `_comment_urls` dans le manifeste Scoop** — le schéma de Scoop refuse
  toute clé inconnue et n'accorde que `_comment` ; le bucket n'aurait pas
  installé.
- **`url` et `hash` réordonnés** — les deux tableaux sont alignés par index chez
  Scoop ; c'est un défaut qui ne se voit qu'à l'installation, sur une
  architecture ou pas l'autre.

**Deux affirmations de ce plan étaient fausses.** Elles sont corrigées en §6 :

- `InstallLocation` n'est pas un dossier de destination, c'est un *argument*
  passé à l'installeur — et un zip n'a pas d'installeur. WinGet installe sous
  `%LOCALAPPDATA%\Microsoft\WinGet\Packages\…`. La case « même dossier que
  `install.ps1` » de la vérification est donc **abandonnée**, pas ratée :
  `irm | iex` reste le canal qui promet `%LOCALAPPDATA%`.
- « `.installer.yaml` **si signature** » : ce fichier est obligatoire, signé ou
  non — c'est lui qui décrit les installateurs. `SignatureSha256` n'a pas sa
  place ici : les archives ne sont pas signées, et WinGet n'en exige pas pour
  publier.

Sur ARM64, WinGet associe `arm64` au binaire natif. Scoop, lui, cherche la
chaîne littérale `arm64` dans le manifeste et ne trouve pas `aarch64` : le
binaire x86_64 y tourne par émulation, ce que Windows 11 sait faire. Le plan
annonçait une sélection native par architecture ; le bucket fournit **les deux**
URL et laisse Scoop choisir son miroir, ce qui est déjà le natif quand il le prend.
Ajouter un bloc `architecture.arm64` décoratif aurait coûté une illusion — et un
risque de régression sur les vraies installations ARM64, ce qui vaut mieux éviter.

Reste à faire, et ce n'est pas dans ce dépôt : créer
[`we-data-ch/scoop-bucket`](https://github.com/we-data-ch/scoop-bucket) (le job
échoue bruyamment s'il n'existe pas) et définir les secrets
`WINGET_CREATE_GITHUB_TOKEN` (scope `public_repo`) et `SCOOP_BUCKET_TOKEN`. Tant
qu'ils manquent, les deux jobs rendent, valident, téléchargent et vérifient les
archives, puis sautent la publication avec un avertissement.

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

### 3.1 Le glibc des binaires Linux — défaut confirmé, à corriger avant tout

Le diagnostic a été exécuté sur le binaire **publié** en `v0.5.12`. Le défaut
est réel, et il bloque l'installation sur une large part des machines.

`objdump -T` sur `typr-v0.5.12-x86_64-unknown-linux-gnu` :

```
GLIBC_2.39  pidfd_spawnp     (faible)
GLIBC_2.39  pidfd_getpid     (faible)
```

Exécution réelle dans des conteneurs qui n'ont rien de TypR :

| Système | glibc / libc | Résultat |
|---|---|---|
| Ubuntu 24.04 (le runner) | 2.39 | fonctionne |
| Debian 13 (trixie) | 2.41 | fonctionne — `typr-cli 0.5.12` |
| Ubuntu 22.04 | 2.35 | **`GLIBC_2.39 not found`** |
| Debian 12 (bookworm) | 2.36 | **`GLIBC_2.39 not found`** |
| Rocky Linux 9 | 2.34 | **`GLIBC_2.39 not found`** |
| Alpine 3.20 | musl | non exécutable — aucun build musl |

Deux enseignements.

**Les symboles faibles ne sauvent rien.** `pidfd_spawnp` et `pidfd_getpid`
(venus de `std::process::Command`) sont marqués faibles : on pourrait croire que
le chargeur les résoudra à `NULL` et que le repli prendra le relais. Testé : le
chargeur refuse quand même, parce que c'est la **référence versionnée** qui est
absente, pas seulement le symbole. Il n'y a donc aucun contournement à tenter —
seule une chaîne de compilation avec un plancher plus bas règle le problème.

**Rocky 9 échoue.** C'est le point le plus douloureux : RHEL / Rocky / AlmaLinux
9 est une base très répandue chez les utilisateurs R professionnels, en
particulier en entreprise. Debian 12 et Ubuntu 22.04 ne sont pas rares non plus.

**Et le plancher va monter tout seul.** `ubuntu-latest` passe à **Ubuntu 26.04
en novembre 2026** (annonce de `actions/runner-images#14748`). Sans action, la
glibc requise passera de 2.39 à ~2.42 et le terrain cassera davantage, à chaque
release, sans qu'une ligne de code soit modifiée. C'est un défaut qui s'aggrave
silencieusement — la raison de plus pour traiter le fond, pas seulement le
symptôme.

**Décision recommandée : ajouter les deux cibles musl.** Elles sont liées
stiquement, donc sans contrainte de glibc du tout — elles fonctionnent sur
Rocky 9, Debian 12, Ubuntu 22.04 **et** Alpine. Cela règle le problème de glibc
et le cas Alpine d'un seul geste, pour deux cibles de plus dans la matrice.

`cross` est déjà utilisé par la matrice pour `aarch64-unknown-linux-gnu`
(`release.yml:110`), et son image musl est disponible : rien de nouveau à
installer, seulement deux entrées à ajouter.

| Cible | Rôle | Priorité |
|---|---|---|
| `x86_64-unknown-linux-musl` | remplaçante de la version GNU sur x86_64 | 1 |
| `aarch64-unknown-linux-musl` | remplaçante sur ARM | 1 |

Deux variantes possibles si l'on veut rester sur des binaires glibc dynamiques,
par ordre de préférence :

- `cargo-zigbuild` en visant `glibc = "2.28"` — plancher très bas, couvre
  RHEL 8. Mais ne couvre pas Alpine, et ajoute une dépendance d'outillage.
- compiler la cible GNU sur `ubuntu-22.04` au lieu de `ubuntu-latest` — plancher
  2.35, couvre Debian 12 et Ubuntu 22.04 mais **pas** Rocky 9 (2.34). Un
  correctif partiel.

Dans tous les cas, sortir la cible GNU de `latest` une fois musl disponible :
le script doit viser musl par défaut et ne retomber sur GNU que si l'utilisateur
le demande explicitement.

### 3.2 macOS — aucun problème

`LC_VERSION_MIN_MACOSX` du binaire `v0.5.12-x86_64-apple-darwin` annonce un
minimum de **10.12.0** (Sierra), compilé avec un SDK 26.5. Toutes les versions
de macOS réellement en usage sont couvertes. Rien à faire.

### 3.3 Windows — le runtime MSVC manque sur les machines minimalistes

Les DLL importées par `typr.exe` :

```
kernel32.dll, USER32.dll, advapi32.dll, ntdll.dll, bcryptprimitives.dll
VCRUNTIME140.dll
api-ms-win-crt-runtime / -string / -math / -stdio / -locale / -heap
```

`VCRUNTIME140.dll` et les `api-ms-win-crt-*` proviennent du **VC++ Redistributable
de Microsoft**. Ils sont présents sur un poste de développement ou une machine
qui a déjà installé beaucoup de choses, mais **absents d'un Windows 10/11
frais**. Le scénario est exactement celui qu'on veut éviter : l'utilisateur copie
la commande, l'installation se termine sans erreur, puis `typr --version` échoue
avec une `DLL introuvable` — une faute d'orthographe dans un message de
chargeur.

**Correction : `-C target-feature=+crt-static`** sur les cibles `*-msvc`, ce qui
lie statiquement le runtime C et supprime la dépendance. rustc accepte le flag
pour `x86_64-pc-windows-msvc` ; **la liaison n'a pas pu être vérifiée localement**
(pas de linker MSVC dans cet environnement). À valider en CI en comparant la
liste des DLL importées du binaire produit.

Ce correctif est peu coûteux et supprime une classe entière de tickets, donc il
fait partie du lot 0.

### 3.4 `aarch64-pc-windows-msvc` — à réévaluer

Cette cible ne fonctionne que sur Windows sur ARM (Snapdragon X). Elle occupe un
créneau de la matrice pour une audience quasi inexistante. Ce n'est pas un
problème de fonctionnement, seulement de temps de CI. À garder pour la
complétude, à retirer si la matrice devient coûteuse.

### 3.5 Résoudre « latest » sans dépendance

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

### 3.6 Vérifier le SHA-256 — ne pas utiliser `sha256sum -c` tel quel

Testé sur `v0.5.12` : `checksums.txt` contient bien les 6 lignes attendues, au
format `sha256sum`. Mais `sha256sum -c checksums.txt` **échoue**, parce qu'il
tente de vérifier les 5 archives qui n'ont pas été téléchargées :

```
typr-v0.5.12-x86_64-unknown-linux-gnu.tar.gz: OK
typr-v0.5.12-aarch64-apple-darwin.tar.gz: FAILED open or read
… (4 autres)
```

Le code de sortie est non nul même quand le fichier nous intéresse est valide.
Le script doit donc **extraire la seule ligne qui correspond**, puis la
vérifier :

```bash
grep "  $ARTIFACT\$" checksums.txt | sha256sum -c -
```

Testé : cette variante sort `OK` avec un code de retour nul. Elle reste le seul
endroit où une erreur de formatage passerait inaperçue, donc à couvrir par un
test qui fournit volontairement un SHA falsifié.

`checksums.txt` ne contient que les 6 archives de binaires — ni le `.vsix`, ni
le tarball RStudio, ni le plugin Vim, ni le WASM, qui sont joints plus tard par
d'autres jobs. C'est cohérent avec l'usage du script d'installation, et le plan
n'a pas à en tenir compte autrement.

L'option `TYPR_INSTALL_VERIFY=0` permet de sauter la vérification pour un
dépannage réseau, sans jamais être le mode par défaut.

### 3.7 Récupérer le binaire après un échec

`install.sh` télécharge dans un fichier temporaire, vérifie le SHA, puis déplace.
En cas d'échec le fichier temporaire est nettoyé : on n'installe jamais un binaire
partiellement téléchargé.

### 3.8 Le job `coherence` interdit les binaires commités

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

Le `typr --version` de la dernière ligne est le test qui compte : c'est
précisément l'étape qui échoue aujourd'hui sur d'anciennes distributions.
Ajouter un contrôle explicite dans la CI, sur des images Docker sans rien
d'autre :

```bash
# ce test a échoué sur les 3 premières distributions en septembre 2026
docker run --rm -v "$BIN:/w/typr" rockylinux:9  sh -c '/w/typr --version'
docker run --rm -v "$BIN:/w/typr" debian:12   sh -c '/w/typr --version'
docker run --rm -v "$BIN:/w/typr" ubuntu:22.04 sh -c '/w/typr --version'
docker run --rm -v "$BIN:/w/typr" alpine:3.20 sh -c '/w/typr --version'
```

- [ ] Les 4 conteneurs ci-dessus répondent une version
- [ ] Linux x86_64 et aarch64, macOS x86_64 et aarch64
- [ ] Windows x86_64 via `irm | iex`, sur une image sans VC++ Redistributable
- [ ] `--version <tag-ancienne>` installe bien l'ancienne version
- [ ] SHA volontairement falsifié → le script refuse d'installer et le dit
- [ ] `checksums.txt` absent ou malformé → erreur explicite, pas un `FAILED open or read`
- [ ] `PATH` non modifiable → message clair, pas d'erreur opaque
- [ ] Idempotence : deux exécutions successives ne cassent rien

---

## 5. Phase 2 — Homebrew

Le canal le plus attendu sur macOS, et il couvre Linux au passage.

### Livrables

Dépôt `we-data-ch/homebrew-typr`, créé à blanc puis tenu par la CI :

```
README.md              # généré à partir de la formule
Formula/typr.rb        # généré depuis checksums.txt
LICENSE                # copié depuis ce dépôt
```

Le générateur et ses tests restent ici, pas dans le tap : le tap ne contient que
ce qu'il faut pour installer, et sa seule source est `release.yml`.

### Contenu de la formule

Le plan prévoyait un `url` unique plus un `if Hardware::CPU.arm?`. C'est
insuffisant : avec Linux dans le canal, **les deux** systèmes ont deux
architectures, et un seul `url` au niveau racine ne peut pas désigner le bon
binaire. Le gabarit construit utilise donc les deux variantes du DSL moderne :

```ruby
class Typr < Formula
  desc "A typed superset of R — transpiler and type checker"
  homepage "https://we-data-ch.github.io/typr.github.io/"
  license "Apache-2.0"
  version "X.Y.Z"

  livecheck do
    url :stable
    strategy :github_latest
  end

  on_macos do
    on_intel do
      url "…/typr-vX.Y.Z-x86_64-apple-darwin.tar.gz"
      sha256 "<omis ici : rempli par la CI>"
    end

    on_arm do
      url "…/typr-vX.Y.Z-aarch64-apple-darwin.tar.gz"
      sha256 "<omis ici : rempli par la CI>"
    end
  end

  # on_linux : idem, en musl. Ce bloc n'existe que si la release en publie.

  def install
    bin.install "typr"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/typr --version")
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

- [ ] `brew install --formula we-data-ch/typr/typr` depuis un Mac clean — humain
- [ ] Fonctionne sur Apple Silicon **et** Intel — humain
- [ ] `brew upgrade typr` passe à la bonne version — humain
- [x] `brew audit --strict` ne signale rien — `packaging-audit` et `brew`
- [x] Le SHA publié correspond à `checksums.txt` de la release — job `brew`, qui
      télécharge l'archive et compare
- [ ] `brew install we-data-ch/typr/typr` résout le tap — le dépôt existe
      (30 septembre 2026) ; la ligne n'est cochée qu'après un essai sur un Mac

Ce que la CI ne peut pas faire, et qui reste à faire à la main une fois le tap
créé : ouvrir un Mac vierge et taper `brew install`. `brew audit --strict` juge la
formule, pas l'installation.

---

## 6. Phase 3 — WinGet et Scoop

Deux manifests, aucun binaire. Ils puisent dans les `.zip` déjà publiés.

> **Ce qui suit a été corrigé après implémentation.** Les deux affirmations
> fausses du plan initial sont signalées en place ; voir « Ce que le lot 4 a
> livré » pour le reste.

### Livrables

```
packaging/render.sh                              le générateur des deux canaux
packaging/winget/we-data-ch.TypR.yaml.in         version
packaging/winget/we-data-ch.TypR.installer.yaml.in  installateurs x64 + arm64
packaging/winget/we-data-ch.TypR.locale.en-US.yaml.in  locale
packaging/scoop/typr.json.in                     manifeste Scoop
packaging/scoop/README.md.in                     README du bucket
packaging/tests/                                 suite + validation des schémas
```

Le `.installer.yaml` n'est pas conditionné à une signature : WinGet l'exige pour
décrire les installateurs, signés ou non.

### WinGet

- `PackageIdentifier: we-data-ch.TypR`
- `PackageVersion` sans le `v` initial — WinGet l'exige.
- `InstallerType: zip`, `InstallerSwitches: Silent` et `SilentWithProgress` avec
  `/S` pour `7z`… **à ne pas inventer** : le type `zip` de WinGet extrait sans
  copie dans un dossier.
- ~~Le chemin `InstallLocation` doit être `%LOCALAPPDATA%\Programs\typr`, pour
  coïncider avec ce que fait `install.ps1`.~~ **Faux.** `InstallLocation` est un
  *argument passé à l'installeur*, pas un dossier de destination ; un zip n'a pas
  d'installeur, et le champ n'aurait rien fait. WinGet extrait sous
  `%LOCALAPPDATA%\Microsoft\WinGet\Packages\…` et y met l'alias `typr` sur le
  PATH — ce qui garde l'installation sans droits administrateur, le seul point
  qui comptait dans cette case.
- Le fichier est publié dans `microsoft/winget-pkgs` par une pull request
  ouverte automatiquement depuis la CI. `wingetcreate` n'a pas de commande
  `validate` autonome : c'est `wingetcreate submit` qui valide avec l'outil
  officiel, puis calcule lui-même le chemin de destination et ouvre la PR.

### Scoop

Le manifest est plus court que celui de WinGet et ne demande pas de PR
maintenue manuellement. `scoop install typr` après
`scoop bucket add typr https://github.com/we-data-ch/scoop-bucket`.

`url` et `hash` sont des **tableaux alignés par index** : les deux URL sont
fournies, Scoop les propose en miroirs et vérifie l'empreinte du fichier
réellement téléchargé. Voir la note ARM64 dans « Ce que le lot 4 a livré ».

Dans les deux cas, la version publiée doit venir de la même source que le reste —
`needs.verify.outputs.version` — et non d'un fichier écrit à la main. C'est
fait, et par construction : `render.sh` refuse de produire quoi que ce soit
depuis un `checksums.txt` qui ne contiendrait pas les deux empreintes Windows.

### Vérification

- [x] Les manifestes rendus passent les **schémas officiels** WinGet et Scoop —
      122 assertions, dont six mutations vérifiées détectées à la main.
- [ ] `winget install we-data-ch.TypR` sur Windows 10 et 11 — **impossible en
      local** : c'est fait par le job `winget` sur `windows-latest`, avec
      `wingetcreate submit`.
- [x] L'installation **sans droits administrateur** est acquise : WinGet comme
      Scoop écrivent dans le profil utilisateur. C'est vérifié par construction
      (aucun `InstallerSwitches`, aucun `RequireExplicitUpgrade`, aucun
      installeur MSI), pas par une exécution.
- [ ] `scoop install typr` puis `scoop update typr` — **impossible en local** :
      c'est fait par le job `scoop`, qui installe le manifeste rendu avant de
      commiter.
- [x] Les deux canaux installent **le même binaire** : la même archive
      `typr-vX.Y.Z-x86_64-pc-windows-msvc.zip`, lue dans le même
      `checksums.txt`, et le même `typr.exe` à sa racine. La suite le vérifie.
- ~~Les deux installers aboutissent au même dossier et au même binaire que
  `install.ps1`~~ **Abandonnée**, voir §6 WinGet : le dossier ne peut pas être
  choisi pour un zip portable. Ce qui est vérifié, c'est le binaire — pas le
  chemin.

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
- [ ] `publish.nu check` compare bien les quatre canaux (voir ci-dessous)

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
| 0a | **Cibles musl** dans la matrice de `release.yml` | — | 2 h |
| 0b | **`+crt-static`** sur les cibles `*-msvc` | — | 30 min |
| 0c | Vérifier que le binaire musl démarre dans Rocky 9, Debian 12, Alpine | 0a | 1 h |
| 1 | `install.sh` + `install.ps1` + hébergement | 0 | 1 j |
| 2 | Tests CI sur les scripts | 1 | 3 h |
| 3 | Homebrew tap + job CI | 1 | 1 j |
| 4 | WinGet + Scoop + PR automatique | 1 | 1 j |
| 5 | README, page d'installation, `RELEASING.md` | 1-4 | 3 h |
| 6 | `publish.nu check` étendu | 1-4 | 2 h |
| 7 | `typr upgrade` (optionnel) | — | plus tard |

Le lot 0 est **précisément-identified et non négociable** : le diagnostic a
montré que le binaire actuel ne démarre ni sur Rocky 9, ni sur Debian 12, ni sur
Ubuntu 22.04. Publier un script d'installation qui lance `typr --version` et
échoue dessus convertirait le canal principal en source de tickets — c'est-à-dire
exactement l'expérience que ce plan cherche à supprimer.

Les lots 3 et 4 sont indépendants de 2 et parallélisables.

---

## 11. Risques

| Risque | Impact | Réponse |
|---|---|---|
| glibc des binaires Linux trop récent | **Confirmé** — l'installation échoue sur Rocky 9, Debian 12, Ubuntu 22.04, Alpine | Lot 0a : cibles musl, liées statiquement |
| Le plancher glibc remontera seul en novembre 2026 | Nouvelle rupture, sans changement de code | Lot 0a : musl supprime la dépendance à la glibc du runner |
| `VCRUNTIME140.dll` absent sur Windows minimal | Installation réussie puis `typr --version` échoue | Lot 0b : `+crt-static` |
| SmartScreen bloque `typr.exe` | Avertissement, pas un blocage | Un binaire posé par `install.ps1` ne porte pas de MOTW |
| Gatekeeper sur macOS | Rare — plancher à macOS 10.12, et `curl \| sh` ne pose pas la quarantaine | Documenter `xattr -d com.apple.quarantine` pour qui télécharge d'abord par navigateur |
| Un canal dérive en silence | Le README promet une version absente d'un canal | `publish.nu check` étendu (lot 6) |
| Le script d'installation casse les gens | Mauvaise expérience sur le canal principal | `--dry-run`, `--version` épinglé, idempotence, tests sur 6 cibles en CI |

---

## 12. hors périmètre

- Signature et notarification (coût, hors budget d'un projet open source).
- Canal `apt` / `dnf` : la distribution Linux est trop fragmentée pour que le
  gain justifie l'effort. Le script Unix — et musl — couvrent ces systèmes sans
  coût supplémentaire.
- Mise à jour automatique silencieuse, qui crée plus de problèmes qu'elle n'en
  résout (mise à jour pendant une session LSP en cours, binaire remplacé sous
  les pieds du processus). `typr upgrade` explicite suffit.
- Paquet `apt` / `dnf` : sans objet, la ligne ci-dessus couvre le sujet.