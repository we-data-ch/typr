#!/bin/sh
# TypR — génération du manifeste Scoop à partir d'un tag.
#
#   packaging/render.sh --tag v0.5.12
#
# Produit les trois fichiers du bucket Scoop, tels que le dépôt de destination
# les attend :
#
#   scoop/bucket/typr.json                      version courante du bucket
#   scoop/bin/typr/<version>.json               historique
#   scoop/README.md                             README du bucket
#
# Rien n'est écrit à la main, et surtout pas un SHA-256 : recopié à la main, il
# finit forcément par désigner un binaire qui n'est plus celui de la release. Ici
# les empreintes viennent du fichier que la release a publié, donc aucun
# manifeste ne peut en diverger.
#
# POSIX sh de bout en bout : ni bash, ni ruby, ni python. La validation par
# Scoop ne peut pas tourner ici ; elle est faite par la suite de tests (schéma
# officiel) et par la CI (installation réelle).
#
# Ce que ce fichier répète de `packaging/homebrew/render.sh` — la lecture du
# `checksums.txt`, l'isolement d'une ligne, la vérification 64 hexadécimaux — y est
# recopié volontairement plutôt que factorisé dans une bibliothèque partagée :
# les deux générateurs sont testés isolément, dans des bacs à sable où aucun
# fichier voisin n'est disponible. Un correctif appliqué à une seule des deux
# copies se verrait ici, par la suite de tests.

set -eu

# ---------------------------------------------------------------------------
# Configuration
#
# Les deux variables existent pour être surchargées par les tests, qui servent
# une arborescence de release locale.
# ---------------------------------------------------------------------------
REPO="${TYPR_REPO:-we-data-ch/typr}"
ORIGIN="${TYPR_ORIGIN:-https://github.com}"

HERE=$(cd "$(dirname "$0")" && pwd)

SCOOP_IN="$HERE/scoop/typr.json.in"
SCOOP_README_IN="$HERE/scoop/README.md.in"

# Les deux cibles Windows, dans l'ordre où elles apparaissent dans le manifeste.
# L'ordre est une convention lisible, pas une contrainte : chaque entrée porte
# son URL et son empreinte, donc les deux sont interchangeables sans que rien ne
# puisse devenir faux.
A_WINDOWS_X86="x86_64-pc-windows-msvc"
A_WINDOWS_ARM="aarch64-pc-windows-msvc"

TAG=""
CHECKSUMS=""
OUT="."

TMP=""

# ---------------------------------------------------------------------------
# Sortie
# ---------------------------------------------------------------------------
# Erreur d'environnement ou de vérification : le monde est en cause (1).
die() {
  printf 'typr-render : %s\n' "$1" >&2
  exit 1
}

# Erreur de saisie : la ligne de commande est en cause (2). Même convention que
# install.sh, install.ps1 et homebrew/render.sh, pour qu'un appelant puisse
# réagir pareil.
usage_die() {
  printf 'typr-render : %s\n' "$1" >&2
  printf '\n' >&2
  usage >&2
  exit 2
}

step() { printf '→ %s\n' "$*"; }
note() { printf '  %s\n' "$*"; }
warn() { printf 'typr-render : attention : %s\n' "$*" >&2; }

cleanup() {
  if [ -n "$TMP" ] && [ -d "$TMP" ]; then
    rm -rf "$TMP"
  fi
}

usage() {
  cat <<'EOF'
TypR — générateur du manifeste Scoop

Usage :
  render.sh --tag <vX.Y.Z> [options]

Options :
  --tag <tag>          version à publier (ex. v0.5.12 ; le v initial est optionnel)
  --checksums <src>    checksums.txt de la release : URL (défaut) ou fichier local
  --out <dossier>      où écrire scoop/ (défaut : .)
  --help               cette aide

Écrit :
  scoop/bucket/typr.json, scoop/bin/typr/<version>.json, scoop/README.md

Ces fichiers sont entièrement générés : ne jamais les éditer à la main. Voir
packaging/README.md.
EOF
}

# ---------------------------------------------------------------------------
# Arguments
# ---------------------------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    --tag)
      [ $# -ge 2 ] || usage_die "--tag attend une version (ex. --tag v0.5.12)."
      TAG="$2"
      shift 2
      ;;
    --tag=*)
      TAG="${1#--tag=}"
      shift
      ;;
    --checksums)
      [ $# -ge 2 ] || usage_die "--checksums attend une URL ou un chemin."
      CHECKSUMS="$2"
      shift 2
      ;;
    --checksums=*)
      CHECKSUMS="${1#--checksums=}"
      shift
      ;;
    --out)
      [ $# -ge 2 ] || usage_die "--out attend un dossier."
      OUT="$2"
      shift 2
      ;;
    --out=*)
      OUT="${1#--out=}"
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      usage_die "argument inconnu : $1"
      ;;
  esac
done

[ -n "$TAG" ] || usage_die "--tag est obligatoire."

case "$TAG" in
  v*) ;;
  *) TAG="v$TAG" ;;
esac

# La forme exacte est vérifiée, pas seulement le préfixe `v` : le tag entre dans
# une URL, dans un nom de fichier (`scoop/bin/typr/<version>.json`) et dans une
# substitution sed, donc un `v../../x` n'a rien à faire ici. `-alpha.1` et
# `-beta.2` sont acceptés : release.yml marque ces versions prerelease, et Scoop
# sait les publier.
printf '%s' "$TAG" \
  | grep -Eq '^v[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z]+(\.[0-9A-Za-z]+)*)?$' \
  || usage_die "tag illisible : '$TAG' (attendu : vX.Y.Z, ou vX.Y.Z-alpha.N)."

VERSION="${TAG#v}"

for required in "$SCOOP_IN" "$SCOOP_README_IN"; do
  [ -f "$required" ] || die "gabarit manquant : $required"
done

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

TMP=$(mktemp -d "${TMPDIR:-/tmp}/typr-render.XXXXXX") \
  || die "impossible de créer un dossier temporaire."

# ---------------------------------------------------------------------------
# checksums.txt
# ---------------------------------------------------------------------------
command -v curl >/dev/null 2>&1 \
  || die "curl est requis et n'est pas installé."

CHECKSUMS_FILE="$TMP/checksums.txt"

case "$CHECKSUMS" in
  ''|*://*)
    URL="$CHECKSUMS"
    [ -n "$URL" ] \
      || URL="$ORIGIN/$REPO/releases/download/$TAG/checksums.txt"
    step "Téléchargement de $URL"
    curl -fsSL "$URL" -o "$CHECKSUMS_FILE" \
      || die "checksums.txt introuvable pour $TAG.
  URL : $URL"
    ;;
  *)
    [ -f "$CHECKSUMS" ] || die "checksums.txt introuvable : $CHECKSUMS"
    cp "$CHECKSUMS" "$CHECKSUMS_FILE"
    ;;
esac

# ---------------------------------------------------------------------------
# SHA-256
#
# Une ligne par artefact, au format `sha256sum`. On isole la ligne qui concerne
# l'artefact cherché — `sha256sum -c` ne conviendrait pas, il tenterait de
# vérifier les sept autres archives et sortirait en erreur alors que la nôtre est
# valide. Les points du nom sont échappés : sans cela, le motif matche
# n'importe quel caractère à leur place, et `x86_64-pc-windows-msvc` se ferait
# prendre pour `x86_64-pc-windows-msvc` suivi de n'importe quoi.
# ---------------------------------------------------------------------------
lookup_sha() { # lookup_sha <artefact> — vide si absent, exit 1 si malformé
  artifact="$1"
  pattern=$(printf '%s' "$artifact" | sed 's/[.[\*^$]/\\&/g')
  lines=$(grep -E "[[:space:]]\*?${pattern}\$" "$CHECKSUMS_FILE" || true)
  count=$(printf '%s' "$lines" | grep -c . || true)

  [ "$count" -gt 1 ] && die "checksums.txt contient $count lignes pour $artifact :
$lines
Deux lignes pour le même nom : la release est incohérente, et choisir l'une
des deux reviendrait à publier une empreinte arbitraire."

  [ "$count" -eq 1 ] || return 0

  sha=$(printf '%s\n' "$lines" | cut -d' ' -f1)
  printf '%s\n' "$sha" | grep -Eq '^[0-9a-f]{64}$' \
    || die "ligne malformée dans checksums.txt pour $artifact :
  $lines
Un SHA-256 fait 64 caractères hexadécimaux."

  printf '%s\n' "$sha"
}

require_sha() { # require_sha <artefact> — die si absent
  sha=$(lookup_sha "$1") || exit 1
  [ -n "$sha" ] \
    || die "checksums.txt de $TAG ne contient aucune ligne pour $1.
  Fichier reçu :
$(cat "$CHECKSUMS_FILE")"
  printf '%s' "$sha"
}

# ---------------------------------------------------------------------------
# Rendu
# ---------------------------------------------------------------------------
SHA_WINDOWS_X86=$(require_sha "typr-$TAG-$A_WINDOWS_X86.zip") || exit 1
SHA_WINDOWS_ARM=$(require_sha "typr-$TAG-$A_WINDOWS_ARM.zip") || exit 1

step "TypR $VERSION"
note "tag          : $TAG"
note "sortie       : $OUT"

# Les valeurs substituées sont des URL et des empreintes : aucun caractère `&`
# ni `|`, qui seraient des métacaractères de sed. La seule exception possible
# viendrait du tag, validé plus haut.
BASE_URL="$ORIGIN/$REPO/releases/download/$TAG"
URL_WINDOWS_X86="$BASE_URL/typr-$TAG-$A_WINDOWS_X86.zip"
URL_WINDOWS_ARM="$BASE_URL/typr-$TAG-$A_WINDOWS_ARM.zip"

sed \
  -e "s|@@VERSION@@|$VERSION|g" \
  -e "s|@@URL_X86_64_WINDOWS@@|$URL_WINDOWS_X86|g" \
  -e "s|@@SHA256_X86_64_WINDOWS@@|$SHA_WINDOWS_X86|g" \
  -e "s|@@URL_AARCH64_WINDOWS@@|$URL_WINDOWS_ARM|g" \
  -e "s|@@SHA256_AARCH64_WINDOWS@@|$SHA_WINDOWS_ARM|g" \
  "$SCOOP_IN" > "$TMP/typr.json"

# Un gabarit dont le nom d'un marqueur a changé produirait un manifeste servi
# tel quel, avec `@@…@@` dedans : un bucket cassé.
grep -q '@@' "$TMP/typr.json" \
  && die "typr.json contient encore un marqueur non substitué :
$(grep -n '@@' "$TMP/typr.json")"

# Un gabarit écrit sans retour à la ligne final donne un fichier que les
# validateurs refusent, et le retrait ne se voit que dans le diff du dépôt de
# destination. Aucune de ces anomalies ne se voit dans la sortie de ce script,
# d'où le contrôle.
[ -z "$(tail -c1 "$TMP/typr.json")" ] \
  || die "typr.json ne se termine pas par un retour à la ligne."

# Le compte est une garde, pas une décoration : il vérifie que les deux cibles
# Windows ont chacune produit une URL et une empreinte, sans quoi un gabarit
# amputé d'une moitié partirait sans bruit.
shas=$(grep -cE '^[[:space:]]*"[0-9a-f]{64}",?$' "$TMP/typr.json" || true)
[ "$shas" -eq 2 ] \
  || die "le manifeste Scoop déclare $shas empreintes, attendu 2.
Le gabarit et le rendu ne correspondent plus."

grep -Fq "\"version\": \"$VERSION\"" "$TMP/typr.json" \
  || die "le manifeste Scoop ne déclare pas \"version\": \"$VERSION\"."

# Le README du bucket est dérivé du manifeste, pas d'une deuxième source :
# description et URL y sont relues dans ce qui vient d'être écrit, donc les deux
# fichiers ne peuvent pas diverger.
DESC=$(sed -n 's/^[[:space:]]*"description": "\(.*\)",\{0,1\}$/\1/p' "$TMP/typr.json" | head -1)
HOMEPAGE=$(sed -n 's/^[[:space:]]*"homepage": "\(.*\)",\{0,1\}$/\1/p' "$TMP/typr.json" | head -1)

[ -n "$DESC" ]     || die "aucune description lisible dans le manifeste rendu."
[ -n "$HOMEPAGE" ] || die "aucune homepage lisible dans le manifeste rendu."

sed \
  -e "s|@@DESC@@|$DESC|g" \
  -e "s|@@HOMEPAGE@@|$HOMEPAGE|g" \
  -e "s|@@VERSION@@|$VERSION|g" \
  "$SCOOP_README_IN" > "$TMP/README.md"

# Le README est rendu après le manifeste, parce qu'il en relit la description :
# il a donc besoin de ses propres contrôles.
grep -q '@@' "$TMP/README.md" \
  && die "le README rendu contient encore un marqueur non substitué :
$(grep -n '@@' "$TMP/README.md")"

[ -z "$(tail -c1 "$TMP/README.md")" ] \
  || die "README.md ne se termine pas par un retour à la ligne."

# ---------------------------------------------------------------------------
# Écriture
#
# Rien n'est écrit dans le dossier de sortie tant que le rendu n'est pas
# complet : un échec au milieu ne doit pas laisser un manifeste tronqué que la
# CI pousserait vers le bucket.
# ---------------------------------------------------------------------------
mkdir -p "$OUT/scoop/bucket" "$OUT/scoop/bin/typr" \
  || die "impossible de créer $OUT/scoop."

cp "$TMP/typr.json" "$OUT/scoop/bucket/typr.json"
cp "$TMP/typr.json" "$OUT/scoop/bin/typr/$VERSION.json"
cp "$TMP/README.md" "$OUT/scoop/README.md"

note "sha256  : $SHA_WINDOWS_X86  ($A_WINDOWS_X86)"
note "sha256  : $SHA_WINDOWS_ARM  ($A_WINDOWS_ARM)"

printf '\n'
step "Écrit : $OUT/scoop/ (bucket, historique, README)"
