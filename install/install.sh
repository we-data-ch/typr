#!/bin/sh
# TypR — installation en une commande (Linux, macOS).
#
#   curl -fsSL https://we-data-ch.github.io/typr.github.io/install/install.sh | sh
#
# POSIX sh de bout en bout : ni bash, ni zsh, ni jq, ni python. Un script
# d'installation qui exige un interpréteur que la machine n'a pas transforme le
# canal principal en ticket.
#
# Sur Linux le script vise les binaires musl, liés statiquement : aucune
# contrainte de glibc, donc le binaire démarre sur Rocky 9 (2.34), Debian 12
# (2.36), Ubuntu 22.04 (2.35) *et* Alpine. Les binaires GNU ne sont choisis que
# sur demande explicite (--gnu), jamais par défaut — c'était le défaut mesuré sur
# v0.5.12 (`GLIBC_2.39 not found` sur les trois premières distributions).

set -eu

# ---------------------------------------------------------------------------
# Configuration
#
# Les trois variables ci-dessous existent pour être surchargées par les tests
# CI, qui servent une arborescence de release locale : sans cela, vérifier le
# refus d'un SHA falsifié exigerait de falsifier une vraie release.
# ---------------------------------------------------------------------------
REPO="${TYPR_INSTALL_REPO:-we-data-ch/typr}"
ORIGIN="${TYPR_INSTALL_ORIGIN:-https://github.com}"
DOCS="${TYPR_INSTALL_DOCS:-https://we-data-ch.github.io/typr.github.io}"

DOWNLOAD_BASE="$ORIGIN/$REPO/releases/download"
LATEST_URL="$ORIGIN/$REPO/releases/latest"
RELEASES_FEED="$ORIGIN/$REPO/releases.atom"

CHANNEL="stable"
VERSION=""
LIBC="musl"
VERIFY="${TYPR_INSTALL_VERIFY:-1}"
DRY_RUN="0"
INSTALL_DIR=""

TMP=""

# ---------------------------------------------------------------------------
# Sortie
# ---------------------------------------------------------------------------
# Erreur d'environnement ou de vérification : le monde est en cause (1).
die() {
  printf 'typr-install : %s\n' "$1" >&2
  exit 1
}

# Erreur de saisie : la ligne de commande est en cause (2). La distinction
# permet à un appelant de réagir différemment — et à un test d'affirmer que le
# refus vient bien du script.
usage_die() {
  printf 'typr-install : %s\n' "$1" >&2
  printf '\n' >&2
  usage >&2
  exit 2
}

step() { printf '→ %s\n' "$*"; }
note() { printf '  %s\n' "$*"; }
warn() { printf 'typr-install : attention : %s\n' "$*" >&2; }

cleanup() {
  if [ -n "$TMP" ] && [ -d "$TMP" ]; then
    rm -rf "$TMP"
  fi
}

usage() {
  cat <<'EOF'
TypR — installeur Unix (Linux, macOS)

Usage :
  install.sh [options]

Options :
  --version <tag>     installe ce tag précis (ex. v0.5.12) au lieu du dernier
  --channel <canal>   stable (défaut) ou beta
  --gnu               Linux : cible glibc dynamique au lieu de musl
  --dry-run           affiche ce qui serait fait, ne télécharge rien
  --help              cette aide

Environnement :
  TYPR_INSTALL_DIR      dossier d'installation (défaut : $HOME/.local/bin)
  TYPR_INSTALL_VERIFY   mettre à 0 pour sauter la vérification SHA-256.
                        Réservé au dépannage réseau ; jamais le mode par défaut.

Rien n'est écrit hors de $HOME et aucun privilège administrateur n'est requis.
EOF
}

# ---------------------------------------------------------------------------
# Arguments
# ---------------------------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    --version)
      [ $# -ge 2 ] || usage_die "--version attend un tag (ex. --version v0.5.12)."
      VERSION="$2"
      shift 2
      ;;
    --version=*)
      VERSION="${1#--version=}"
      shift
      ;;
    --channel)
      [ $# -ge 2 ] || usage_die "--channel attend stable ou beta."
      CHANNEL="$2"
      shift 2
      ;;
    --channel=*)
      CHANNEL="${1#--channel=}"
      shift
      ;;
    --gnu)
      LIBC="gnu"
      shift
      ;;
    --dry-run)
      DRY_RUN="1"
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

case "$CHANNEL" in
  stable|beta) ;;
  *) usage_die "canal inconnu : '$CHANNEL' (stable ou beta)." ;;
esac

case "$VERIFY" in
  0|1) ;;
  *) usage_die "TYPR_INSTALL_VERIFY doit valoir 0 ou 1 (reçu : '$VERIFY')." ;;
esac

command -v curl >/dev/null 2>&1 || die "curl est requis et n'est pas installé."
command -v tar  >/dev/null 2>&1 || die "tar est requis et n'est pas installé."

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# ---------------------------------------------------------------------------
# Cible : uname -s / uname -m vers le triple Rust
# ---------------------------------------------------------------------------
case "$(uname -m)" in
  x86_64|amd64)       ARCH="x86_64" ;;
  arm64|aarch64)      ARCH="aarch64" ;;
  *)
    die "architecture non supportée : $(uname -m).
  TypR publie des binaires x86_64 et aarch64."
    ;;
esac

case "$(uname -s)" in
  Linux)
    if [ "$LIBC" = "gnu" ]; then
      TARGET="$ARCH-unknown-linux-gnu"
    else
      TARGET="$ARCH-unknown-linux-musl"
    fi
    FORMAT="tar.gz"
    ;;
  Darwin)
    TARGET="$ARCH-apple-darwin"
    FORMAT="tar.gz"
    ;;
  *)
    die "système non supporté : $(uname -s).
  Windows : powershell -c \"irm https://$DOCS/install/install.ps1 | iex\""
    ;;
esac

# Sous Rosetta, `uname -m` renvoie x86_64 sur une machine Apple Silicon et le
# binaire Intel y fonctionne : aucun cas particulier n'est nécessaire.

# ---------------------------------------------------------------------------
# Version
# ---------------------------------------------------------------------------
validate_tag() {
  case "$1" in
    v[0-9]*) return 0 ;;
    *) usage_die "tag illisible : '$1' (attendu : vX.Y.Z)." ;;
  esac
}

resolve_version() {
  if [ -n "$VERSION" ]; then
    case "$VERSION" in
      v*) TAG="$VERSION" ;;
      *)  TAG="v$VERSION" ;;
    esac
    validate_tag "$TAG"
    return 0
  fi

  if [ "$CHANNEL" = "stable" ]; then
    # `/releases/latest` redirige vers `/tag/<tag>` et exclut les prereleases —
    # exactement la sémantique de la branche stable. Lire l'en-tête Location
    # évite d'avoir à supposer `jq` ou `python` sur la machine.
    LOC=$(curl -fsSI "$LATEST_URL" \
          | tr -d '\r' \
          | grep -i '^location:' \
          | sed 's|.*/tag/||' \
          | head -1 || true)
    [ -n "$LOC" ] || die "impossible de déterminer la dernière version stable.
  URL interrogée : $LATEST_URL
  Pour épingler une version : --version vX.Y.Z"
    TAG="$LOC"
  else
    # Le canal beta doit lister les releases et filtrer. Le flux Atom en liste
    # les dix dernières, prereleases comprises, et se parse avec grep et sed —
    # même contrainte qu'au-dessus. Les suffixes sont ceux que release.yml
    # marque `prerelease`.
    TAG=$(curl -fsSL "$RELEASES_FEED" \
          | grep -oE 'Repository/[0-9]+/v[^<]+' \
          | sed 's|.*/||' \
          | grep -E -- '-(alpha|beta|rc)[.0-9]' \
          | head -1 || true)
    [ -n "$TAG" ] || die "aucune prerelease trouvée sur $RELEASES_FEED.
  Le flux Atom ne liste que les dix dernières releases : si la dernière
  prerelease est plus ancienne, épinglez-la avec --channel beta --version vX.Y.Z"
  fi

  validate_tag "$TAG"
}

# ---------------------------------------------------------------------------
# SHA-256
# ---------------------------------------------------------------------------
# GNU Coreutils expose `sha256sum` ; macOS livre `shasum -a 256`. Les deux
# doivent être tentés, sinon le canal principal échoue sur un tiers des machines.
sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | cut -d' ' -f1
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | cut -d' ' -f1
  else
    die "ni sha256sum ni shasum n'est disponible : le SHA-256 ne peut pas être
  vérifié. Relancer avec TYPR_INSTALL_VERIFY=0 pour installer sans vérifier."
  fi
}

verify_download() {
  file="$1"
  name="$2"

  step "Vérification du SHA-256"

  if ! curl -fsSL "$DOWNLOAD_BASE/$TAG/checksums.txt" -o "$TMP/checksums.txt"; then
    die "checksums.txt introuvable pour $TAG.
  URL : $DOWNLOAD_BASE/$TAG/checksums.txt"
  fi

  # `sha256sum -c checksums.txt` échouerait ici : le fichier liste les huit
  # archives de la release, et l'outil tenterait de vérifier les sept que
  # nous n'avons pas téléchargées, en sortant un code non nul même quand la
  # nôtre est valide. On isole donc la seule ligne qui la concerne, et on
  # compare les empreintes nous-mêmes — sans dépendre du format de `-c`.
  #
  # Les points du nom de fichier sont échappés : sans cela, le motif matche
  # n'importe quel caractère à leur place.
  PATTERN=$(printf '%s' "$name" | sed 's/[.[\*^$]/\\&/g')
  LINE=$(grep -E "[[:space:]]\*?${PATTERN}\$" "$TMP/checksums.txt" || true)

  [ -n "$LINE" ] || die "checksums.txt de $TAG ne contient aucune ligne pour $name.
  Fichier reçu :
$(cat "$TMP/checksums.txt")"

  EXPECTED=$(printf '%s\n' "$LINE" | cut -d' ' -f1)

  # Un SHA-256 fait 64 caractères hexadécimaux. Sans ce contrôle, une ligne
  # tronquée ou malformée produirait un « SHA-256 falsifié » qui accuse
  # l'archive à tort : le message désignerait le mauvais fichier.
  printf '%s\n' "$EXPECTED" | grep -Eq '^[0-9a-f]{64}$' \
    || die "ligne malformée dans checksums.txt pour $name :
  $LINE
  Un SHA-256 fait 64 caractères hexadécimaux."

  ACTUAL=$(sha256_of "$file")
  [ "$EXPECTED" = "$ACTUAL" ] || die "SHA-256 falsifié pour $name.
  attendu $EXPECTED
  obtenu   $ACTUAL
  L'archive a été supprimée ; rien n'est installé."

  note "SHA-256 correct ($ACTUAL)"
}

# ---------------------------------------------------------------------------
# Installation
# ---------------------------------------------------------------------------
resolve_install_dir() {
  if [ -n "$INSTALL_DIR" ]; then
    return 0
  fi
  if [ -n "${TYPR_INSTALL_DIR:-}" ]; then
    INSTALL_DIR="$TYPR_INSTALL_DIR"
    return 0
  fi
  [ -n "${HOME:-}" ] || die "HOME n'est pas défini et TYPR_INSTALL_DIR ne l'est pas
  non plus : impossible de savoir où installer."
  INSTALL_DIR="$HOME/.local/bin"
}

path_contains() {
  # Les entrées vides de PATH (`:$PATH`) ignorées, et un eventual `/` final
  # toléré : `~/.local/bin/` et `~/.local/bin` désignent le même dossier.
  RESOLVED=$(cd "$1" 2>/dev/null && pwd -P) || return 1
  OLDIFS="$IFS"
  IFS=":"
  for entry in $PATH; do
    [ -n "$entry" ] || continue
    NORMALIZED=$(cd "$entry" 2>/dev/null && pwd -P) || continue
    if [ "$NORMALIZED" = "$RESOLVED" ]; then
      IFS="$OLDIFS"
      return 0
    fi
  done
  IFS="$OLDIFS"
  return 1
}

warn_about_path() {
  warn "$INSTALL_DIR n'est pas dans votre PATH."
  note "Ajoutez ceci à ~/.profile (ou ~/.zshrc) :"
  note "    export PATH=\"$INSTALL_DIR:\$PATH\""
  note "Puis ouvrez un nouveau terminal."
}

check_binary_starts() {
  step "Vérification de l'installation"
  if "$INSTALL_DIR/typr" --version; then
    return 0
  fi

  # Le cas mesuré en septembre 2026 : l'installation se termine sans erreur,
  # puis `typr --version` échoue. Le dire ici vaut mieux qu'un ticket.
  printf '\n' >&2
  warn "le binaire est installé mais ne démarre pas."
  note "Si le message ci-dessus évoque une version de glibc manquante, le"
  note "binaire a été compilé pour une glibc plus récente que celle de cette"
  note "machine. Relancer sans --gnu, ou lire $DOCS/install pour le dépannage."
  exit 1
}

# ---------------------------------------------------------------------------
# Déroulement
# ---------------------------------------------------------------------------
resolve_version
resolve_install_dir

ARTIFACT="typr-$TAG-$TARGET.$FORMAT"
URL="$DOWNLOAD_BASE/$TAG/$ARTIFACT"

step "TypR $TAG"
note "système   : $(uname -s) $(uname -m)"
note "cible     : $TARGET"
note "dossier   : $INSTALL_DIR"

if [ "$VERIFY" = "0" ]; then
  warn "vérification SHA-256 désactivée (TYPR_INSTALL_VERIFY=0)."
fi

if [ "$DRY_RUN" = "1" ]; then
  step "Simulation — rien ne sera téléchargé ni écrit"
  note "artefact  : $ARTIFACT"
  note "url       : $URL"
  note "sha256    : $DOWNLOAD_BASE/$TAG/checksums.txt"
  exit 0
fi

mkdir -p "$INSTALL_DIR" || die "impossible de créer $INSTALL_DIR.
  Vérifier les droits, ou choisir un autre dossier avec TYPR_INSTALL_DIR."

TMP=$(mktemp -d "${TMPDIR:-/tmp}/typr-install.XXXXXX") \
  || die "impossible de créer un dossier temporaire."

step "Téléchargement"
curl -fL --progress-bar "$URL" -o "$TMP/$ARTIFACT" \
  || die "téléchargement échoué : $URL
  Vérifier le réseau ou réessayer avec --version vX.Y.Z."

if [ "$VERIFY" != "0" ]; then
  verify_download "$TMP/$ARTIFACT" "$ARTIFACT"
else
  step "Vérification du SHA-256"
  note "ignorée."
fi

step "Extraction"
mkdir -p "$TMP/extract"
tar -xzf "$TMP/$ARTIFACT" -C "$TMP/extract" \
  || die "archive illisible : $ARTIFACT"
[ -f "$TMP/extract/typr" ] \
  || die "l'archive $ARTIFACT ne contient aucun binaire 'typr'."

# mv -f rend l'opération idempotente : relancer l'installeur remplace le
# binaire au lieu d'échouer dessus.
mv -f "$TMP/extract/typr" "$INSTALL_DIR/typr"
chmod 755 "$INSTALL_DIR/typr"

printf '\n'
step "TypR $TAG installé dans $INSTALL_DIR"

if path_contains "$INSTALL_DIR"; then
  check_binary_starts
else
  warn_about_path
  printf '\n'
  note "Essayez d'abord : $INSTALL_DIR/typr --version"
fi

printf '\n'
note "Documentation : $DOCS"