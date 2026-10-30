# Manifeste Scoop — TypR

Le canal `scoop install typr` ne publie aucun binaire. Le dépôt
[`we-data-ch/scoop-bucket`](https://github.com/we-data-ch/scoop-bucket) ne
contient que des URL de release et leurs SHA-256.

Ces valeurs sont **générées**, jamais écrites à la main. Un SHA recopié à la main
finit toujours par désigner un binaire qui n'est plus celui de la release, et
l'erreur n'apparaît qu'à l'installation, chez l'utilisateur. Ici elles sont lues
dans le `checksums.txt` que la release publie, puis vérifiées par une
installation réelle avant d'être publiées.

Sur Windows, le canal de base reste `irm …/install.ps1 | iex` (voir
`install/README.md`) : il télécharge l'archive de la GitHub Release et en vérifie
l'empreinte, sans dépendre d'aucun dépôt tiers. Scoop est l'option pour qui veut
un gestionnaire de paquets.

## Les fichiers d'ici

| Fichier | Rôle |
|---|---|
| `render.sh` | le générateur : tag + `checksums.txt` → les trois fichiers du bucket |
| `scoop/typr.json.in` | le manifeste Scoop, et le même fichier versé dans l'historique du bucket |
| `scoop/README.md.in` | le README du bucket, dont la description et l'URL sont relues dans le manifeste |
| `tests/` | la suite de tests et le validateur de schéma, exécutés par le job `packaging-win` de `ci.yml` |

Le générateur est POSIX `sh` de bout en bout : ni bash, ni ruby, ni python. La
validation par Scoop ne peut pas tourner ici ; elle est faite par la suite de
tests (schéma officiel) et par la CI (installation réelle).

## Rendre le manifeste

```sh
# depuis le `checksums.txt` d'une release publiée
packaging/render.sh --tag v0.5.12 --out /tmp/manifests

# ou depuis une copie locale du fichier
packaging/render.sh --tag v9.9.9 --checksums ./checksums.txt --out /tmp/manifests
```

Trois fichiers sont écrits, rien d'autre :

```
scoop/bucket/typr.json                      version courante du bucket
scoop/bin/typr/<version>.json               historique
scoop/README.md                             README du bucket
```

Sans `--checksums`, l'URL est déduite du tag —
`https://github.com/we-data-ch/typr/releases/download/<tag>/checksums.txt` — ce
qui est exactement ce que fait le job `scoop`.

| Option | Effet |
|---|---|
| `--tag <vX.Y.Z>` | version à publier ; le `v` initial est optionnel, `-alpha.N` et `-beta.N` sont acceptés |
| `--checksums <src>` | `checksums.txt` : URL (défaut) ou fichier local |
| `--out <dossier>` | où écrire `scoop/` (défaut : le répertoire courant) |
| `--help` | l'aide |

## Tester

Sans réseau, sous n'importe quel Unix :

```sh
packaging/tests/run-tests.sh
```

La suite couvre ce qui compte et ne se voit pas à la relecture :

- que le générateur prenne **la** bonne ligne parmi les huit de `checksums.txt` —
  les deux archives Windows ne diffèrent que par un mot, et les six autres
  cibles ne doivent jamais apparaître dans le manifeste ;
- que `url` et `hash` restent alignés par index, comme Scoop les lit, et que
  chaque empreinte soit celle de son archive dans `checksums.txt` ;
- que rien ne soit écrit quand une vérification échoue.

Le schéma officiel est validé en plus, quand le réseau est là :

```sh
packaging/render.sh --tag v9.9.9 \
  --checksums packaging/tests/fixtures/checksums-v9.9.9.txt --out /tmp/manifests
packaging/tests/validate-schemas.py /tmp/manifests
```

Il est téléchargé depuis le dépôt de Scoop, pas recopié ici : une copie locale
validerait moins que ce qui sera exigé dès la version suivante.

## Ce que la CI en fait

Le job `packaging-win` de `ci.yml` rend le manifeste depuis une fixture, sans
réseau ni release, et le fait valider par le schéma officiel. C'est ce qui
arrête une pull request.

Ce que la fixture ne peut pas juger a lieu au moment de la release, dans le job
`scoop` de `release.yml`, sur `windows-latest` :

1. récupère le `checksums.txt` de la release ;
2. rend le manifeste ;
3. installe le binaire **pour de vrai**, depuis le manifeste local :
   `scoop install <manifeste>` télécharge l'archive publiée, en vérifie
   l'empreinte, l'extrait, crée le shim `typr` — puis `typr --version` doit
   annoncer la version de la release ;
4. ne commite vers le bucket qu'après cela, en purgeant l'historique au-delà des
   dix dernières versions.

Sans le secret, les étapes 1 à 3 ont lieu quand même et seule la publication est
sautée, avec un avertissement ; un refus de Scoop arrête le job.

## Deux pièges de validation, et comment ils ont été trouvés

Ce sont des défauts que rien n'affiche à la relecture. Ils sont tous couverts par
la suite de tests.

**Une clé de confort dans le manifeste.** Le schéma de Scoop refuse toute clé
inconnue (`additionalProperties: false`) et n'accorde que `_comment` parmi les
clés préfixées par un tiret bas. Une clé `_comment_urls` expliquant l'ordre des
URLs aurait été rejetée à l'installation : l'explication est dans ce fichier, dans
le gabarit, et dans les tests.

**`url` et `hash` réordonnés.** Les deux tableaux sont alignés par index, et
Scoop vérifie l'empreinte du fichier qu'il a réellement téléchargé. Un `hash`
réordonné seul ne serait pas faux *par construction* — seulement faux à
l'installation, chez l'utilisateur, sur une architecture ou pas l'autre. La suite
compare donc les deux tableaux au `checksums.txt` de la fixture, index par index.

## ARM64

Les deux archives Windows sont publiées et annoncées : `x86_64-pc-windows-msvc` et
`aarch64-pc-windows-msvc`.

Scoop choisit son architecture en cherchant la chaîne littérale `arm64` dans le
manifeste — un nom de fichier `aarch64-…` ne suffit pas. Un manifeste qui ne
contient pas ce mot est donc traité comme x64 sur une machine ARM64, et le
binaire x86_64 y tourne par émulation, ce que Windows 11 sait faire. C'est
acceptable, et c'est pourquoi les deux URL sont fournies : Scoop les propose en
miroirs et vérifie l'empreinte du fichier réellement téléchargé. Ajouter un bloc
`architecture.arm64` pour faire croire à une sélection native ne changerait rien
— `arch_specific` retomberait sur le tableau `url` — et coûterait une illusion.

## Prérequis de publication

- **Le dépôt [`we-data-ch/scoop-bucket`](https://github.com/we-data-ch/scoop-bucket) doit exister.**
  C'est un dépôt ordinaire : il suffit de l'initialiser avec un `README.md` et un
  `LICENSE`. Tant qu'il n'existe pas, le job `scoop` échoue bruyamment.
- **`SCOOP_BUCKET_TOKEN`** — PAT GitHub avec permission d'écriture sur ce dépôt.

## Pourquoi pas WinGet

Un canal WinGet a existé jusqu'à la v0.6.2. Il a été retiré : il passait par une
pull request vers `microsoft/winget-pkgs`, depuis un fork personnel à garder
synchronisé, avec un jeton `public_repo` et une revue par des mainteneurs
extérieurs. Chaque release pouvait échouer pour une raison étrangère au dépôt
(la v0.6.2 a échoué sur un fork désynchronisé). L'archive de la release et
`install.ps1` couvrent le même besoin sans aucun tiers.

## Ce que le générateur refuse

| Cause | Message |
|---|---|
| tag illisible | `tag illisible : …` (code 2) |
| `checksums.txt` absent | `checksums.txt introuvable` |
| ligne absente pour un artefact | `ne contient aucune ligne pour …` |
| deux lignes pour le même artefact | `contient 2 lignes pour …` |
| SHA malformé | `ligne malformée dans checksums.txt` |
| gabarit modifié | `contient encore un marqueur non substitué` |
| rendu sans retour à la ligne final | `ne se termine pas par un retour à la ligne` |
| gabarit amputé d'une cible | `le manifeste Scoop déclare N empreintes, attendu 2` |

Dans tous les cas **rien n'est écrit** dans `--out` : une erreur au milieu du
rendu ne doit pas laisser un manifeste tronqué que la CI commiterait dans le
bucket.
