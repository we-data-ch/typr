#!/bin/sh
# TypR — génération de la formule Homebrew à partir d'un tag.
#
#   packaging/homebrew/render.sh --tag v0.5.12
#
# Produit les deux fichiers du tap `we-data-ch/homebrew-typr` :
#
#   Formula/typr.rb   la formule — version et SHA-256 lus dans checksums.txt
#   README.md         le README du tap, dérivé de la formule
#
# Rien n'est écrit à la main, et surtout pas un SHA-256 : recopié à la main, il
# finit forcément par désigner un binaire qui n'est plus celui de la release. Ici
# les empreintes viennent du fichier que la release a publié, donc la formule ne
# peut pas en diverger.
#
# POSIX sh de bout en bout : ni bash, ni ruby. La validation par Homebrew
# (`brew audit --strict`) ne peut pas tourner ici ; elle est faite par la CI sur
# un runner macOS, avant le commit vers le tap.

set -eu

# ---------------------------------------------------------------------------
# Configuration
#
# Les trois variables existent pour être surchargées par les tests, qui servent
# une arborescence de release locale.
# ---------------------------------------------------------------------------
REPO="${TYPR_REPO:-we-data-ch/typr}"
ORIGIN="${TYPR_ORIGIN:-https://github.com}"

HERE=$(cd "$(dirname "$0")" && pwd)
FORMULA_IN="$HERE/Formula/typr.rb.in"
LINUX_IN="$HERE/parts/on_linux.rb.in"
LINUX_ABSENT_IN="$HERE/parts/on_linux_absent.rb.in"
README_IN="$HERE/README.md.in"

TAG=""
CHECKSUMS=""
OUT="."

TMP=""

# ---------------------------------------------------------------------------
# Sortie
# ---------------------------------------------------------------------------
# Erreur d'environnement ou de vérification : le monde est en cause (1).
die() {
  printf 'typr-brew-render : %s\n' "$1" >&2
  exit 1
}

# Erreur de saisie : la ligne de commande est en cause (2). Même convention que
# install.sh et install.ps1, pour qu'un appelant puisse réagir pareil.
usage_die() {
  printf 'typr-brew-render : %s\n' "$1" >&2
  printf '\n' >&2
  usage >&2
  exit 2
}

step() { printf '→ %s\n' "$*"; }
note() { printf '  %s\n' "$*"; }
warn() { printf 'typr-brew-render : attention : %s\n' "$*" >&2; }

cleanup() {
  if [ -n "$TMP" ] && [ -d "$TMP" ]; then
    rm -rf "$TMP"
  fi
}

usage() {
  cat <<'EOF'
TypR — générateur de la formule Homebrew

Usage :
  render.sh --tag <vX.Y.Z> [options]

Options :
  --tag <tag>          version à publier (ex. v0.5.12 ; le v initial est optionnel)
  --checksums <src>    checksums.txt de la release : URL (défaut) ou fichier local
  --out <dossier>      où écrire Formula/typr.rb et README.md (défaut : .)
  --help               cette aide

Écrit Formula/typr.rb et README.md dans le dossier du tap, et rien d'autre.
Les deux fichiers sont entièrement générés : ne jamais les éditer à la main.
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
# une URL et dans une substitution sed, donc un `v../../x` n'a rien à faire
# ici. `-alpha.1` et `-beta.2` sont acceptés — release.yml marque ces versions
# prerelease, et Homebrew les lit sans problème.
printf '%s' "$TAG" \
  | grep -Eq '^v[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z]+(\.[0-9A-Za-z]+)*)?$' \
  || usage_die "tag illisible : '$TAG' (attendu : vX.Y.Z, ou vX.Y.Z-alpha.N)."

VERSION="${TAG#v}"

for required in "$FORMULA_IN" "$LINUX_IN" "$LINUX_ABSENT_IN" "$README_IN"; do
  [ -f "$required" ] || die "gabarit manquant : $required"
done

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

TMP=$(mktemp -d "${TMPDIR:-/tmp}/typr-brew-render.XXXXXX") \
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
# n'importe quel caractère à leur place, et le binaire GNU se ferait prendre pour
# le binaire musl.
# ---------------------------------------------------------------------------
lookup_sha() { # lookup_sha <artefact> — vide si absent, exit 1 si malformé
  artifact="$1"
  pattern=$(printf '%s' "$artifact" | sed 's/[.[\*^$]/\\&/g')
  lines=$(grep -E "[[:space:]]\*?${pattern}\$" "$CHECKSUMS_FILE" || true)
  count=$(printf '%s' "$lines" | grep -c . || true)

  [ "$count" -gt 1 ] && die "checksums.txt contient $count lignes pour $artifact :
$lines
Deux lignes pour le même nom : la release est incohérente, et choisir l'une
des deux reviendrait à publier un SHA arbitraire."

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
A_APPLE_ARM="aarch64-apple-darwin"
A_APPLE_X86="x86_64-apple-darwin"
A_LINUX_ARM="aarch64-unknown-linux-musl"
A_LINUX_X86="x86_64-unknown-linux-musl"

SHA_APPLE_ARM=$(require_sha "typr-$TAG-$A_APPLE_ARM.tar.gz") || exit 1
SHA_APPLE_X86=$(require_sha "typr-$TAG-$A_APPLE_X86.tar.gz") || exit 1

# Linux est facultatif, contrairement à macOS. Les binaires musl sont entrés
# dans la matrice après v0.5.12 : aucune version publiée n'en contient encore, et
# exiger les quatre artefacts rendrait le tap impossible à amorcer. Le GNU n'est
# pas repris à la place — ce serait réintroduire le défaut `GLIBC_2.39 not
# found` que le lot 0 du plan vient de corriger sur le canal principal. Le bloc
# `on_linux` réapparaîtra seul dès la première release qui en publiera.
SHA_LINUX_ARM=$(lookup_sha "typr-$TAG-$A_LINUX_ARM.tar.gz") || exit 1
SHA_LINUX_X86=$(lookup_sha "typr-$TAG-$A_LINUX_X86.tar.gz") || exit 1

if [ -n "$SHA_LINUX_ARM" ] && [ -n "$SHA_LINUX_X86" ]; then
  LINUX_PART="$LINUX_IN"
  EXPECTED_SHAS=4
else
  LINUX_PART="$LINUX_ABSENT_IN"
  EXPECTED_SHAS=2
  for missing in "$A_LINUX_ARM" "$A_LINUX_X86"; do
    grep -q "$TAG-$missing" "$CHECKSUMS_FILE" \
      || warn "$TAG ne publie pas de binaire $missing : le bloc on_linux est omis de la formule."
  done
fi

step "TypR $VERSION"
note "tag      : $TAG"
note "sortie   : $OUT"

# Les valeurs substituées sont des URL et des empreintes : aucun caractère `&`
# ni `|`, qui seraient des métacaractères de sed. La seule exception possible
# viendrait du tag, validé plus haut.
BASE_URL="$ORIGIN/$REPO/releases/download/$TAG"

sed \
  -e "s|@@VERSION@@|$VERSION|g" \
  -e "s|@@URL_AARCH64_APPLE@@|$BASE_URL/typr-$TAG-$A_APPLE_ARM.tar.gz|g" \
  -e "s|@@SHA256_AARCH64_APPLE@@|$SHA_APPLE_ARM|g" \
  -e "s|@@URL_X86_64_APPLE@@|$BASE_URL/typr-$TAG-$A_APPLE_X86.tar.gz|g" \
  -e "s|@@SHA256_X86_64_APPLE@@|$SHA_APPLE_X86|g" \
  "$FORMULA_IN" > "$TMP/formula-1.rb"

# `r file` insère le fichier après la ligne trouvée, `d` supprime cette ligne —
# la paire remplace donc le marqueur par son contenu, en portable (BSD sed
# compris, contrairement à un `s` multi-lignes).
sed -e "/^@@LINUX@@\$/{
  r $LINUX_PART
  d
}" "$TMP/formula-1.rb" > "$TMP/formula-2.rb"

if [ "$LINUX_PART" = "$LINUX_IN" ]; then
  sed \
    -e "s|@@URL_X86_64_LINUX@@|$BASE_URL/typr-$TAG-$A_LINUX_X86.tar.gz|g" \
    -e "s|@@SHA256_X86_64_LINUX@@|$SHA_LINUX_X86|g" \
    -e "s|@@URL_AARCH64_LINUX@@|$BASE_URL/typr-$TAG-$A_LINUX_ARM.tar.gz|g" \
    -e "s|@@SHA256_AARCH64_LINUX@@|$SHA_LINUX_ARM|g" \
    "$TMP/formula-2.rb" > "$TMP/typr.rb"
else
  cp "$TMP/formula-2.rb" "$TMP/typr.rb"
fi

# Un gabarit dont le nom d'un marqueur a changé produirait une formule servie
# telle quelle, avec `@@…@@` dedans — le défaut le plus discret de cette
# famille d'outils, parce qu'il ne se voit qu'à l'installation.
grep -q '@@' "$TMP/typr.rb" \
  && die "la formule rendue contient encore un marqueur non substitué :
$(grep -n '@@' "$TMP/typr.rb")"

# Un gabarit écrit sans retour à la ligne final donne une formule que `brew style`
# refuse (Style/FinalNewline), et le retrait se voit seulement à l'installation.
# Aucune des deux anomalies ne se voit dans le diff du tap, d'où le contrôle.
[ -z "$(tail -c1 "$TMP/typr.rb")" ] \
  || die "la formule rendue ne se termine pas par un retour à la ligne."

shas=$(grep -cE '^[[:space:]]+sha256 "[0-9a-f]{64}"$' "$TMP/typr.rb" || true)
[ "$shas" -eq "$EXPECTED_SHAS" ] \
  || die "la formule rendue déclare $shas empreintes, attendu $EXPECTED_SHAS.
Le gabarit et le rendu ne correspondent plus."

# Pas de `version` explicite : `brew audit` la juge redondante, Homebrew la
# déduit de l'URL. On vérifie donc que l'URL porte bien la version attendue.
grep -Fq "/download/v$VERSION/" "$TMP/typr.rb" \
  || die "la formule rendue ne pointe pas vers la release v$VERSION."

# Le README du tap est dérivé de la formule, pas d'une deuxième source : desc et
# homepage sont relus dans ce qui vient d'être écrit, donc les deux fichiers ne
# peuvent pas diverger.
DESC=$(sed -n 's/^[[:space:]]*desc "\(.*\)"$/\1/p' "$TMP/typr.rb" | head -1)
HOMEPAGE=$(sed -n 's/^[[:space:]]*homepage "\(.*\)"$/\1/p' "$TMP/typr.rb" | head -1)

[ -n "$DESC" ]     || die "aucun desc lisible dans la formule rendue."
[ -n "$HOMEPAGE" ] || die "aucun homepage lisible dans la formule rendue."

if [ "$LINUX_PART" = "$LINUX_IN" ]; then
  LINUX_NOTE='- **Linux** : couvert — binaire musl lié statiquement, x86_64 et aarch64, aucune contrainte de glibc.'
else
  LINUX_NOTE="- **Linux** : pas encore pour cette version — aucun binaire musl n'est publié. macOS est couvert. Le bloc Linux réapparaîtra à la première release qui en publiera."
fi

sed \
  -e "s|@@DESC@@|$DESC|g" \
  -e "s|@@HOMEPAGE@@|$HOMEPAGE|g" \
  -e "s|@@VERSION@@|$VERSION|g" \
  -e "s|@@LINUX@@|$LINUX_NOTE|g" \
  "$README_IN" > "$TMP/README.md"

grep -q '@@' "$TMP/README.md" \
  && die "le README rendu contient encore un marqueur non substitué :
$(grep -n '@@' "$TMP/README.md")"

# Rien n'est écrit dans le dossier de sortie tant que le rendu n'est pas
# complet : un échec au milieu ne doit pas laisser une formule tronquée que la
# CI pousserait dans le tap.
mkdir -p "$OUT/Formula" || die "impossible de créer $OUT/Formula."

cp "$TMP/typr.rb"   "$OUT/Formula/typr.rb"
cp "$TMP/README.md" "$OUT/README.md"

note "sha256  : $SHA_APPLE_X86  (x86_64-apple-darwin)"
note "sha256  : $SHA_APPLE_ARM  (aarch64-apple-darwin)"
# Les deux lignes Linux sont conditionnelles : leur absence est normale pour
# toutes les versions publiées à ce jour (cf. plus haut). Un `if` explicite
# plutôt qu'un `&&` — le statut de retour de la liste entière est alors celui de
# `note`, ce qui évite de dépendre du traitement de `set -e` par l'interpréteur.
if [ -n "$SHA_LINUX_X86" ]; then
  note "sha256  : $SHA_LINUX_X86  (x86_64-unknown-linux-musl)"
fi
if [ -n "$SHA_LINUX_ARM" ]; then
  note "sha256  : $SHA_LINUX_ARM  (aarch64-unknown-linux-musl)"
fi

printf '\n'
step "Écrit : $OUT/Formula/typr.rb et $OUT/README.md"