# Tap Homebrew — TypR

Le canal `brew install we-data-ch/typr/typr` ne publie aucun binaire. Le dépôt
[`we-data-ch/homebrew-typr`](https://github.com/we-data-ch/homebrew-typr) ne
contient qu'une formule : une URL de release et son SHA-256.

Ces deux valeurs sont **générées**, jamais écrites à la main. Un SHA recopié à la
main finit toujours par désigner un binaire qui n'est plus celui de la release,
et l'erreur n'apparaît qu'à l'installation, chez l'utilisateur. Ici elles sont
lues dans le `checksums.txt` que la release publie, puis comparées à l'archive
réellement téléchargée avant d'être commitées.

## Les fichiers d'ici

| Fichier | Rôle |
|---|---|
| `render.sh` | le générateur : tag + `checksums.txt` → `Formula/typr.rb` et `README.md` |
| `Formula/typr.rb.in` | le gabarit de la formule |
| `parts/on_linux.rb.in` | le bloc `on_linux`, inséré seulement si la release publie un binaire musl |
| `parts/on_linux_absent.rb.in` | le commentaire mis à sa place sinon |
| `README.md.in` | le README du tap, dont la description et l'URL sont relues dans la formule |
| `tests/` | la suite de tests, exécutée par le job `packaging` de `ci.yml` |

## Rendre une formule

```sh
# depuis le `checksums.txt` d'une release publiée
packaging/homebrew/render.sh --tag v0.5.12 --out /tmp/tap

# ou depuis une copie locale du fichier
packaging/homebrew/render.sh --tag v9.9.9 --checksums ./checksums.txt --out /tmp/tap
```

Deux fichiers sont écrits, rien d'autre : `Formula/typr.rb` et `README.md`. Sans
`--checksums`, l'URL est déduite du tag —
`https://github.com/we-data-ch/typr/releases/download/<tag>/checksums.txt` — ce
qui est exactement ce que fait le job `brew`.

| Option | Effet |
|---|---|
| `--tag <vX.Y.Z>` | version à publier ; le `v` initial est optionnel |
| `--checksums <src>` | `checksums.txt` : URL (défaut) ou fichier local |
| `--out <dossier>` | où écrire (défaut : le répertoire courant) |
| `--help` | l'aide |

## Tester

Sans Homebrew ni réseau, sous n'importe quel Unix :

```sh
packaging/homebrew/tests/run-tests.sh
```

La suite couvre ce qui compte et ne se voit pas à la relecture : que le
générateur prenne **la** bonne ligne parmi les huit de `checksums.txt` — les
bâtiments GNU et musl ne diffèrent que par un mot — et qu'il refuse de produire
une formule à moitié rendue.

Sur macOS, ajouter ce que Linux ne peut pas juger :

```sh
packaging/homebrew/render.sh --tag v9.9.9 \
  --checksums packaging/homebrew/tests/fixtures/checksums-v9.9.9.txt --out /tmp/tap

TAP_DIR="$(brew --repository)/Library/Taps/we-data-ch/homebrew-typr"
mkdir -p "$TAP_DIR/Formula"
cp /tmp/tap/Formula/typr.rb "$TAP_DIR/Formula/typr.rb"

brew style "$TAP_DIR/Formula/typr.rb"
brew audit --strict --formula="$TAP_DIR/Formula/typr.rb"

# installation réelle, sur l'archive publiée
brew install we-data-ch/typr/typr
```

## Ce que la CI en fait

Le job `brew` de `release.yml`, sur `macos-latest` :

1. récupère le `checksums.txt` de la release ;
2. rend la formule ;
3. télécharge `typr-<tag>-aarch64-apple-darwin.tar.gz` et vérifie que son
   empreinte figure bien dans la formule rendue ;
4. passe la formule à `brew style` et `brew audit --strict` ;
5. ne commite vers le tap qu'après tout cela.

Sans le secret `HOMEBREW_TAP_TOKEN`, les étapes 1 à 4 ont lieu quand même et
seule la publication est sautée, avec un avertissement ; un refus de Homebrew
arrête le job. Mieux vaut un job rouge qu'un tap que personne ne peut installer.

Le dépôt `we-data-ch/homebrew-typr` existe et contient déjà la formule de
v0.5.12, rendue depuis le `checksums.txt` de cette release. Il reste à définir
le secret `HOMEBREW_TAP_TOKEN` (PAT GitHub, scope `repo` sur ce dépôt) dans les
secrets de la CI ; la prochaine release exercera alors le chemin complet
`release.yml` → tap.

## Pourquoi Linux peut manquer

Le bloc `on_linux` n'est inséré que si la release publie
`typr-<tag>-{x86_64,aarch64}-unknown-linux-musl.tar.gz`. **Aucune version
publiée ne le fait** : les cibles musl sont entrées dans la matrice après
v0.5.12, et le prochain tag les produira. Le rendu produit alors une formule
macOS seule, et le README du tap l'annonce au lieu de promettre Linux.

Le repli n'est pas le binaire GNU. Ce serait réintroduire, par la porte du canal
Homebrew, le défaut que le lot 0 du plan vient de corriger partout ailleurs :
`GLIBC_2.39 not found` sur Rocky 9, Debian 12 et Ubuntu 22.04, après une
installation qui a pourtant réussi. Le bloc `on_linux` réapparaîtra seul à la
première release qui en publiera — sans que quiconque touche à ce fichier.

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

Dans tous les cas **rien n'est écrit** dans `--out` : une erreur au milieu du
rendu ne doit pas laisser une formule tronquée que la CI pousserait dans le tap.