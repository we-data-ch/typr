# Manifestes WinGet et Scoop — TypR

Les canaux `winget install we-data-ch.TypR` et `scoop install typr` ne publient
aucun binaire. Le dépôt [`we-data-ch/scoop-bucket`](https://github.com/we-data-ch/scoop-bucket)
ne contient que des URL de release et leurs SHA-256 ; la pull request ouverte sur
[`microsoft/winget-pkgs`](https://github.com/microsoft/winget-pkgs) n'en contient
pas davantage.

Ces valeurs sont **générées**, jamais écrites à la main. Un SHA recopié à la main
finit toujours par désigner un binaire qui n'est plus celui de la release, et
l'erreur n'apparaît qu'à l'installation, chez l'utilisateur. Ici elles sont lues
dans le `checksums.txt` que la release publie, puis comparées aux archives
réellement téléchargées avant d'être publiées.

Les deux canaux lisent le **même** fichier, ce qui les empêche de diverger entre
eux : une release ne peut pas être installé par WinGet et refusée par Scoop sous
prétexte qu'une empreinte a été recopiée.

## Les fichiers d'ici

| Fichier | Rôle |
|---|---|
| `render.sh` | le générateur : tag + `checksums.txt` → les six fichiers des deux canaux |
| `winget/*.yaml.in` | les trois manifestes WinGet : version, installation, locale `en-US` |
| `scoop/typr.json.in` | le manifeste Scoop, et le même fichier versé dans l'historique du bucket |
| `scoop/README.md.in` | le README du bucket, dont la description et l'URL sont relues dans le manifeste |
| `tests/` | la suite de tests et le validateur de schémas, exécutés par le job `packaging-win` de `ci.yml` |

Le générateur est commun aux deux canaux — `render.sh`, à la racine de
`packaging/`, rend tout. Il est POSIX `sh` de bout en bout : ni bash, ni ruby, ni
python. La validation par WinGet et par Scoop ne peut pas tourner ici ; elle est
faite par la suite de tests (schémas officiels) et par la CI (installation
réelle).

## Rendre les manifestes

```sh
# depuis le `checksums.txt` d'une release publiée
packaging/render.sh --tag v0.5.12 --out /tmp/manifests

# ou depuis une copie locale du fichier
packaging/render.sh --tag v9.9.9 --checksums ./checksums.txt --out /tmp/manifests
```

Six fichiers sont écrits, rien d'autre :

```
winget/we-data-ch.TypR.yaml                 version     → PR winget-pkgs
winget/we-data-ch.TypR.installer.yaml       installateurs  (x64, arm64)
winget/we-data-ch.TypR.locale.en-US.yaml    locale
scoop/bucket/typr.json                      version courante du bucket
scoop/bin/typr/<version>.json               historique
scoop/README.md                             README du bucket
```

Sans `--checksums`, l'URL est déduite du tag —
`https://github.com/we-data-ch/typr/releases/download/<tag>/checksums.txt` — ce
qui est exactement ce que font les jobs `winget` et `scoop`.

| Option | Effet |
|---|---|
| `--tag <vX.Y.Z>` | version à publier ; le `v` initial est optionnel, `-alpha.N` et `-beta.N` sont acceptés |
| `--checksums <src>` | `checksums.txt` : URL (défaut) ou fichier local |
| `--out <dossier>` | où écrire `winget/` et `scoop/` (défaut : le répertoire courant) |
| `--release-date <AAAA-MM-JJ>` | `ReleaseDate` WinGet ; par défaut la date du jour, en UTC |
| `--help` | l'aide |

## Tester

Sans réseau, sous n'importe quel Unix :

```sh
packaging/tests/run-tests.sh
```

La suite couvre ce qui compte et ne se voit pas à la relecture :

- que le générateur prenne **la** bonne ligne parmi les huit de `checksums.txt` —
  les deux archives Windows ne diffèrent que par un mot, et les six autres
  cibles ne doivent jamais apparaître dans un canal Windows ;
- que l'URL d'une architecture ne soit jamais jumelée à l'empreinte de l'autre ;
  les deux cibles sont vérifiées comme un bloc, architecture comprise ;
- que `url` et `hash` du manifeste Scoop restent alignés par index, comme Scoop
  les lit ;
- que l'alias WinGet (`typr`) et l'exécutable Scoop (`typr.exe`) désignent le
  même programme ;
- que rien ne soit écrit quand une vérification échoue.

Les schémas officiels sont validés en plus, quand le réseau est là :

```sh
packaging/render.sh --tag v9.9.9 \
  --checksums packaging/tests/fixtures/checksums-v9.9.9.txt --out /tmp/manifests
packaging/tests/validate-schemas.py /tmp/manifests
```

Ils sont téléchargés depuis `winget-pkgs` et `Scoop`, pas recopiés dans ce dépôt :
`winget-pkgs` n'accepte que son propre schéma, et une copie locale validerait
moins que ce qui sera exigé dès la version suivante.

## Ce que la CI en fait

Le job `packaging-win` de `ci.yml` rend les manifestes depuis une fixture, sans
réseau ni release, et les fait valider par les schémas officiels. C'est ce qui
arrête une pull request.

Ce que la fixture ne peut pas juger a lieu au moment de la release :

Le job `winget` de `release.yml`, sur `windows-latest` :

1. récupère le `checksums.txt` de la release ;
2. rend les manifestes ;
3. télécharge les deux archives Windows et vérifie que leurs empreintes
   figurent bien dans les manifestes rendus ;
4. passe le dossier rendu à `wingetcreate submit`, qui le valide avec le
   validateur officiel de WinGet avant d'ouvrir la pull request.

Le job `scoop` de `release.yml`, sur `windows-latest` :

1. récupère le `checksums.txt` de la release ;
2. rend les manifestes ;
3. installe le binaire **pour de vrai**, depuis le manifeste local :
   `scoop install <manifeste>` télécharge l'archive publiée, en vérifie
   l'empreinte, l'extrait, crée le shim `typr` — puis `typr --version` doit
   annoncer la version de la release ;
4. ne commite vers le bucket qu'après cela, en purgeant l'historique au-delà des
   dix dernières versions.

Sans les secrets, les étapes 1 à 3 ont lieu quand même et seule la publication est
sautée, avec un avertissement ; un refus de WinGet ou de Scoop arrête le job.

## Deux points où le plan avait tort

Le plan de distribution annonçait `InstallLocation` dans le manifeste WinGet, en
prétendant obtenir `%LOCALAPPDATA%\Programs\typr`. Ce champ n'est pas un dossier
de destination : c'est un *argument passé à l'installeur*, et un zip n'a pas
d'installeur. WinGet installe donc dans son propre dossier de paquet, sous
`%LOCALAPPDATA%\Microsoft\WinGet\Packages\…`, et y met `typr` sur le PATH — sans
droits administrateur, ce qui est le point qui compte. Le chemin exact n'est pas
contractuel et changera ; `irm | iex` reste le canal qui promet `%LOCALAPPDATA%`.

Le plan annonçait aussi un « `.installer.yaml` de signature ». Ce fichier est
nécessaire quel que soit le statut de signature : c'est lui qui décrit les
installateurs. La signature, elle, se déclare par `SignatureSha256`, qui n'a pas
sa place ici : les archives ne sont pas signées, et WinGet n'en exige pas pour
publier.

## Trois pièges de validation, et comment ils ont été trouvés

Ce sont des défauts que rien n'affiche à la relecture. Ils sont tous couverts par
la suite de tests.

**`ReleaseDate` sans guillemets.** Lu par un analyseur YAML 1.1 — PyYAML, et
tout ce qui l'imite — `2026-01-02` est une *date*, pas une chaîne, et le schéma
de `winget-pkgs` exige une chaîne. Le gabarit écrit donc `ReleaseDate: "…"`.
Guillemeté, le fichier est un `string` partout, sans dépendre de la version de la
norme qu'applique l'outil de validation.

**Une clé de confort dans le manifeste Scoop.** Le schéma de Scoop refuse toute
clé inconnue (`additionalProperties: false`) et n'accorde que `_comment` parmi les
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
`aarch64-pc-windows-msvc`. WinGet les associe à `x64` et `arm64` respectivement.

Scoop, lui, choisit son architecture en cherchant la chaîne littérale `arm64`
dans le manifeste — un nom de fichier `aarch64-…` ne suffit pas. Un manifeste qui
ne contient pas ce mot est donc traité comme x64 sur une machine ARM64, et le
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
- **`WINGET_CREATE_GITHUB_TOKEN`** — PAT GitHub avec le scope `public_repo` :
  `winget-pkgs` est public, et il faut pouvoir y ouvrir une pull request.
  `wingetcreate` le lit dans la variable d'environnement
  `WINGET_CREATE_GITHUB_TOKEN`, jamais dans `--token`, qui l'afficherait dans la
  liste des processus du runner.

## La première soumission WinGet

`wingetcreate submit` sert à la première publication d'un paquet ; `update`
exigerait qu'il existe déjà dans `winget-pkgs`. Le chemin de destination
(`manifests/w/we-data-ch/typpr/<version>/`) est calculé par l'outil d'après le
contenu des manifestes : le dossier rendu n'a pas besoin d'être pré-dispositionné.

WinGet ne propose une version prerelease que sur demande explicite :
`winget install --pre`. Le paquet reste donc installable normalement, et le
rendu des manifestes n'a pas à distinguer les deux cas — sauf que le premier
`winget install` que quelqu'un tentera pour un paquet encore inconnu de
`winget-pkgs` risque de proposer `-alpha.1` à la place de la version stable.
Précéder la première publication d'une version stable est donc la prudence, pas
une règle que le générateur puisse appliquer : il rend fidèlement ce que la
release publie.

Ce n'est pas au générateur de trancher non plus sur la date : `ReleaseDate`
décrit la date de la version WinGet, pas celle du commit ni celle de la
publication. Par défaut c'est le jour de la génération, ce qui est le jour où la
pull request est ouverte.

Enfin, `wingetcreate` réécrit les manifestes avec son propre sérialiseur : les
commentaires de nos gabarits ne se retrouvent pas dans la pull request. Ce qui
compte — les champs — est exactement ce que l'outil a validé.

## Ce que le générateur refuse

| Cause | Message |
|---|---|
| tag illisible | `tag illisible : …` (code 2) |
| `--release-date` illisible | `release-date illisible : …` (code 2) |
| `checksums.txt` absent | `checksums.txt introuvable` |
| ligne absente pour un artefact | `ne contient aucune ligne pour …` |
| deux lignes pour le même artefact | `contient 2 lignes pour …` |
| SHA malformé | `ligne malformée dans checksums.txt` |
| gabarit modifié | `contient encore un marqueur non substitué` |
| rendu sans retour à la ligne final | `ne se termine pas par un retour à la ligne` |
| gabarit amputé d'une cible | `le manifeste d'installation déclare N empreintes, attendu 2` |

Dans tous les cas **rien n'est écrit** dans `--out` : une erreur au milieu du
rendu ne doit pas laisser un manifeste tronqué que la CI proposerait à
`winget-pkgs`, ou commiterait dans le bucket.
