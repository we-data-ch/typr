#!/usr/bin/env bash
# Tests de install/install.sh, sans réseau.
#
# L'installeur ne parle qu'à une release : URL de redirection `/releases/latest`,
# flux Atom, artefacts et `checksums.txt`. Le serveur de fixtures reproduit ces
# quatre points, ce qui permet de tester le chemin heureux *et* les refus —
# notamment le SHA falsifié, qu'on ne peut pas produire sur une vraie release
# sans la compromettre.
#
#   install/tests/run-tests.sh
#
# Le même script est exécuté par le job `install` de ci.yml.

set -uo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
INSTALL_SH="$HERE/../install.sh"
REPO_PATH="we-data-ch/typr"
STABLE_TAG="v9.9.9"
BETA_TAG="v9.9.9-beta.1"

PASSED=0
FAILED=0
ROOT=""
SERVER_PID=""

# ---------------------------------------------------------------------------

cleanup() {
  [ -n "$SERVER_PID" ] && kill "$SERVER_PID" 2>/dev/null
  [ -n "$ROOT" ] && rm -rf "$ROOT"
  return 0
}
trap cleanup EXIT

ok()   { PASSED=$((PASSED + 1)); printf '  \033[32m✓\033[0m %s\n' "$1"; }
ko()   { FAILED=$((FAILED + 1)); printf '  \033[31m✗\033[0m %s\n' "$1"; [ $# -gt 1 ] && printf '      %s\n' "$2"; return 0; }

check_rc() { # check_rc <attendu> <obtenu> <libellé>
  if [ "$1" -eq "$2" ]; then ok "$3"; else ko "$3" "code de retour $2, attendu $1"; fi
}

check_contains() { # check_contains <sortie> <motif> <libellé>
  case "$1" in
    *"$2"*) ok "$3" ;;
    *)      ko "$3" "« $2 » absent de la sortie : $1" ;;
  esac
}

check_not_contains() {
  case "$1" in
    *"$2"*) ko "$3" "« $2 » présent alors qu'il ne devait pas l'être" ;;
    *)      ok "$3" ;;
  esac
}

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------
# Un « binaire » de trois lignes : l'installeur l'extrait, le rend exécutable et
# l'exécute avec `--version`. Assez pour couvrir le chemin complet sans
# télécharger 11 Mo ni dépendre d'une release existante.
build_binary() {
  cat > "$1" <<'EOF'
#!/bin/sh
[ "${1:-}" = "--version" ] || exit 2
echo "typr-cli 0.0.0-fixture"
EOF
  chmod 755 "$1"
}

# Construit une archive et *ajoute* sa ligne à checksums.txt sans l'écraser :
# une vraie release liste les huit archives, et c'est précisément ce qui fait
# échouer `sha256sum -c` quand on n'en a téléchargé qu'une.
make_release() { # make_release <tag> <cible> [bad]
  local tag="$1" target="$2" bad="${3:-}"
  local dir="$ROOT/$REPO_PATH/releases/download/$tag"
  local name="typr-$tag-$target.tar.gz"
  mkdir -p "$dir" "$ROOT/stage"

  build_binary "$ROOT/stage/typr"
  tar -czf "$dir/$name" -C "$ROOT/stage" typr

  local sha
  sha=$(sha256sum "$dir/$name" | cut -d' ' -f1)
  [ "$bad" = "bad" ] && sha="0000000000000000000000000000000000000000000000000000000000000000"
  printf '%s  %s\n' "$sha" "$name" >> "$dir/checksums.txt"
}

# Réécrit checksums.txt depuis les archives déjà présentes, en falsifiant
# éventuellement l'empreinte d'une seule. Les archives ne bougent pas entre deux
# appels, donc leur SHA est stable.
rewrite_checksums() { # rewrite_checksums [cible-à-falsifier]
  local rel="$ROOT/$REPO_PATH/releases/download/$STABLE_TAG"
  local bad="${1:-}" name sha
  : > "$rel/checksums.txt"
  for name in "$rel"/*.tar.gz; do
    sha=$(sha256sum "$name" | cut -d' ' -f1)
    if [ -n "$bad" ] && [ "${name##*/}" = "typr-$STABLE_TAG-$bad.tar.gz" ]; then
      sha="0000000000000000000000000000000000000000000000000000000000000000"
    fi
    printf '%s  %s\n' "$sha" "${name##*/}" >> "$rel/checksums.txt"
  done
}

make_origin() {
  ROOT=$(mktemp -d)
  local rel="$ROOT/$REPO_PATH/releases"

  mkdir -p "$rel"
  printf '%s\n' "$STABLE_TAG" > "$rel/LATEST"

  for target in \
    x86_64-unknown-linux-musl aarch64-unknown-linux-musl \
    x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu \
    x86_64-apple-darwin aarch64-apple-darwin; do
    make_release "$STABLE_TAG" "$target"
  done

  # Release beta : plus récente que la stable dans le flux, donc celle que
  # `--channel beta` doit choisir.
  make_release "$BETA_TAG" "x86_64-unknown-linux-musl"
  make_release "$BETA_TAG" "x86_64-unknown-linux-gnu"

  cat > "$ROOT/$REPO_PATH/releases.atom" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Release notes from typr</title>
  <id>tag:github.com,2008:https://github.com/$REPO_PATH/releases</id>
  <entry>
    <id>tag:github.com,2008:Repository/1/$STABLE_TAG</id>
    <title>$STABLE_TAG</title>
  </entry>
  <entry>
    <id>tag:github.com,2008:Repository/1/$BETA_TAG</id>
    <title>$BETA_TAG</title>
  </entry>
  <entry>
    <id>tag:github.com,2008:Repository/1/v0.1.0</id>
    <title>v0.1.0</title>
  </entry>
</feed>
EOF

  python3 "$HERE/fixture-server.py" "$ROOT" > "$ROOT/.port" &
  SERVER_PID=$!

  local port=""
  for _ in $(seq 1 50); do
    port=$(grep -oE 'PORT [0-9]+' "$ROOT/.port" 2>/dev/null | cut -d' ' -f2)
    [ -n "$port" ] && break
    sleep 0.1
  done
  [ -n "$port" ] || { echo "le serveur de fixtures n'a pas démarré" >&2; exit 1; }

  export TYPR_INSTALL_ORIGIN="http://127.0.0.1:$port"
}

# ---------------------------------------------------------------------------
# Lancement de l'installeur
# ---------------------------------------------------------------------------
run() { # run <dossier-install> <args...>
  local dir="$1"; shift
  TYPR_INSTALL_DIR="$dir" "$INSTALL_SH" "$@" 2>&1
}

# Un dossier de travail sous $ROOT, donc nettoyé avec le reste.
work() {
  local dir
  dir="$ROOT/work-$(printf '%s' "$1" | tr -c 'a-zA-Z0-9' -)"
  mkdir -p "$dir"
  printf '%s' "$dir"
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------
test_static_analysis() {
  echo "Analyse statique"

  # `ok` et `ko` sortent toujours en 0, donc le `||` ne se déclenche jamais à
  # tort : SC2015 signalerait ici un if-then-else qui n'existe pas.
  # shellcheck disable=SC2015
  sh -n "$INSTALL_SH" 2>&1 && ok "sh -n" || ko "sh -n"
  # shellcheck disable=SC2015
  bash -n "$INSTALL_SH" 2>&1 && ok "bash -n" || ko "bash -n"

  if command -v dash >/dev/null 2>&1; then
    # shellcheck disable=SC2015
    dash -n "$INSTALL_SH" 2>&1 && ok "dash -n (POSIX strict)" || ko "dash -n"
  else
    printf '  – dash absent, POSIX strict non vérifié\n'
  fi

  if command -v shellcheck >/dev/null 2>&1; then
    # Aucune exclusion : install.sh ne se sert ni de `local` (non POSIX) ni
    # d'aucun attribut bash. Le dialecte `sh` est vérifié, pas seulement la
    # syntaxe.
    if out=$(shellcheck -s sh "$INSTALL_SH" 2>&1); then
      ok "shellcheck"
    else
      ko "shellcheck" "$out"
    fi
  else
    printf '  – shellcheck absent, non exécuté\n'
  fi

  if command -v pwsh >/dev/null 2>&1; then
    if out=$(pwsh -NoProfile -Command "
      \$e = \$null
      [System.Management.Automation.Language.Parser]::ParseFile(
        '$HERE/../install.ps1', [ref]\$null, [ref]\$e)
      if (\$e.Count -gt 0) { \$e | ForEach-Object { \$_.Message }; exit 1 }"); then
      ok "install.ps1 — analyse syntaxique PowerShell"
    else
      ko "install.ps1 — analyse syntaxique PowerShell" "$out"
    fi
  else
    printf '  – pwsh absent, install.ps1 non analysé\n'
  fi
}

test_help() {
  echo " --help et arguments"
  local out
  out=$(run "$(work help)" --help)
  check_rc 0 $? "--help sort en 0"
  check_contains "$out" "Usage" "--help affiche l'usage"
  check_contains "$out" "--dry-run" "--help documente --dry-run"

  out=$(run "$(work noarg)" --bogus)
  check_rc 2 $? "un argument inconnu sort en 2"
  check_contains "$out" "argument inconnu" "un argument inconnu est nommé"
}

test_dry_run() {
  echo " --dry-run"
  local dir out
  dir=$(work dryrun)
  out=$(run "$dir" --dry-run)
  check_rc 0 $? "--dry-run sort en 0"
  check_contains "$out" "Simulation" "--dry-run annonce la simulation"
  check_contains "$out" "musl" "--dry-run vise musl par défaut sur Linux"
  check_contains "$out" "$STABLE_TAG" "--dry-run résout la dernière version stable"
  check_not_contains "$out" "Téléchargement" "--dry-run ne télécharge rien"
  if [ -z "$(ls -A "$dir")" ]; then ok "--dry-run n'écrit rien"
  else ko "--dry-run n'écrit rien" "contenu : $(ls -A "$dir")"; fi
}

test_happy_path() {
  echo "Installation réelle"
  local dir out
  dir="$ROOT/inst-default"
  out=$(run "$dir")
  check_rc 0 $? "installation par défaut"
  check_contains "$out" "SHA-256 correct" "le SHA-256 est vérifié"
  check_contains "$out" "$STABLE_TAG" "la version installée est annoncée"
  check_contains "$out" "n'est pas dans votre PATH" "un dossier hors PATH avertit sans échouer"
  if [ -x "$dir/typr" ]; then ok "le binaire est installé et exécutable"
  else ko "le binaire est installé et exécutable" "$dir/typr absent ou non exécutable"; fi

  out=$("$dir/typr" --version 2>&1)
  check_rc 0 $? "typr --version répond après installation"

  # Idempotence : relancer ne doit ni échouer ni laisser de fichier temporaire.
  out=$(run "$dir")
  check_rc 0 $? "seconde installation (idempotence)"
  if [ -x "$dir/typr" ]; then ok "le binaire survit à la réinstallation"
  else ko "le binaire survit à la réinstallation"; fi

  # --gnu : la cible glibc dynamique, que le script ne doit choisir que sur
  # demande explicite.
  dir="$ROOT/inst-gnu"
  out=$(run "$dir" --gnu)
  check_rc 0 $? "installation --gnu"
  check_contains "$out" "linux-gnu" "--gnu cible bien unknown-linux-gnu"
  if [ -x "$dir/typr" ]; then ok "--gnu installe le binaire"
  else ko "--gnu installe le binaire"; fi

  # Dossier d'installation imbriqué absent : il doit être créé.
  dir="$ROOT/inst-nested/bin"
  out=$(run "$dir")
  check_rc 0 $? "dossier imbriqué absent créé"
  if [ -x "$dir/typr" ]; then ok "le dossier imbriqué contient le binaire"
  else ko "le dossier imbriqué contient le binaire"; fi
}

test_pinned_version() {
  echo "Version épinglée"
  local dir out
  dir="$ROOT/inst-pinned"
  out=$(run "$dir" --dry-run --version "$STABLE_TAG")
  check_rc 0 $? "--version sort en 0"
  check_contains "$out" "$STABLE_TAG" "--version est respecté"

  # Sans le « v », l'installeur doit le remettre : c'est la forme que la
  # documentation affiche.
  out=$(run "$(work pinned)" --dry-run --version "${STABLE_TAG#v}")
  check_contains "$out" "$STABLE_TAG" "--version sans le v initial est normalisé"
}

test_beta_channel() {
  echo "Canal beta"
  local dir out
  dir="$ROOT/inst-beta"
  out=$(run "$dir" --dry-run --channel beta)
  check_rc 0 $? "--channel beta résout une prerelease"
  check_contains "$out" "$BETA_TAG" "--channel beta choisit la prerelease, pas la stable"

  out=$(run "$dir" --channel beta)
  check_rc 0 $? "installation depuis le canal beta"
  if [ -x "$dir/typr" ]; then ok "le binaire beta est installé"
  else ko "le binaire beta est installé"; fi
}

test_bad_checksum() {
  echo "SHA-256 falsifié"
  local dir out target
  rewrite_checksums x86_64-unknown-linux-musl

  dir="$ROOT/inst-badsha"
  out=$(run "$dir")
  check_rc 1 $? "le script refuse un SHA falsifié"
  check_contains "$out" "SHA-256 falsifié" "le message nomme la cause"
  check_contains "$out" "rien n'est installé" "le message dit que rien n'est installé"
  if [ ! -e "$dir/typr" ]; then ok "aucun binaire n'est laissé sur le disque"
  else ko "aucun binaire n'est laissé sur le disque" "$dir/typr présent"; fi

  # La variable d'échappement doit exister, sans être le mode par défaut.
  out=$(TYPR_INSTALL_VERIFY=0 run "$dir")
  check_rc 0 $? "TYPR_INSTALL_VERIFY=0 installe sans vérifier"
  check_contains "$out" "désactivée" "TYPR_INSTALL_VERIFY=0 est annoncé"
  rm -rf "$dir"

  rewrite_checksums
}

test_missing_checksums() {
  echo "checksums.txt absent ou malformé"
  local dir out rel
  rel="$ROOT/$REPO_PATH/releases/download"

  # Absent
  rm -f "$rel/$STABLE_TAG/checksums.txt"
  dir="$ROOT/inst-nosum"
  out=$(run "$dir")
  check_rc 1 $? "checksums.txt absent → échec"
  check_contains "$out" "checksums.txt introuvable" "l'absence est nommée explicitement"
  check_not_contains "$out" "FAILED open or read" "le message n'est pas celui de sha256sum -c"
  if [ ! -e "$dir/typr" ]; then ok "rien n'est installé sans checksums.txt"
  else ko "rien n'est installé sans checksums.txt"; fi

  # Présent mais sans la ligne de cet artefact : c'est le cas de `sha256sum -c`,
  # qui tente de vérifier les sept autres archives et sort en erreur alors que
  # la nôtre est valide.
  cat > "$rel/$STABLE_TAG/checksums.txt" <<'EOF'
1111111111111111111111111111111111111111111111111111111111111111  typr-v9.9.9-aarch64-apple-darwin.tar.gz
2222222222222222222222222222222222222222222222222222222222222222  typr-v9.9.9-x86_64-pc-windows-msvc.zip
EOF
  dir="$ROOT/inst-nosum2"
  out=$(run "$dir")
  check_rc 1 $? "checksums.txt sans la ligne attendue → échec"
  check_contains "$out" "ne contient aucune ligne pour" "la ligne manquante est nommée"
  if [ ! -e "$dir/typr" ]; then ok "rien n'est installé sans ligne correspondante"
  else ko "rien n'est installé sans ligne correspondante"; fi

  # Malformé : la ligne existe mais le SHA est illisible.
  printf 'pas-un-sha  typr-v9.9.9-x86_64-unknown-linux-musl.tar.gz\n' \
    > "$rel/$STABLE_TAG/checksums.txt"
  dir="$ROOT/inst-badline"
  out=$(run "$dir")
  check_rc 1 $? "checksums.txt malformé → échec"
  check_contains "$out" "malform" "la malformation est nommée"
  if [ ! -e "$dir/typr" ]; then ok "rien n'est installé sur une ligne malformée"
  else ko "rien n'est installé sur une ligne malformée"; fi

  # Restitué pour les tests suivants.
  rewrite_checksums
}

test_missing_release() {
  echo "Release absente"
  local out
  out=$(run "$(work ghost)" --version v0.0.1-does-not-exist)
  check_rc 1 $? "un tag inexistant échoue"
  check_contains "$out" "téléchargement échoué" "l'echec de téléchargement est explicite"
}

test_path_present() {
  echo "Dossier déjà dans le PATH"
  local dir out
  dir="$ROOT/inst-onpath"
  out=$(PATH="$dir:$PATH" run "$dir")
  check_rc 0 $? "installation avec le dossier dans le PATH"
  check_not_contains "$out" "n'est pas dans votre PATH" "aucun avertissement PATH inutile"
  check_contains "$out" "Vérification de l'installation" "le binaire est exécuté dans la foulée"
}

# §3.8 du plan : le job `coherence` interdit les binaires commités. Le script
# ne doit écrire que dans TYPR_INSTALL_DIR — jamais dans l'arbre du dépôt, et
# jamais ailleurs que dans le dossier qu'on lui a donné.
test_contained_writes() {
  echo "Écritures contenues"
  local dir out stray
  dir="$(work contained)"

  # HOME redirigé, TYPR_INSTALL_DIR absent : le dossier par défaut doit être
  # `$HOME/.local/bin`, et rien d'autre ne doit être écrit.
  out=$(env -u TYPR_INSTALL_DIR HOME="$ROOT/fakehome" PATH="$dir:$PATH" "$INSTALL_SH" 2>&1)
  check_rc 0 $? "installation avec HOME redirigé et TYPR_INSTALL_DIR absent"
  if [ -x "$ROOT/fakehome/.local/bin/typr" ]; then
    ok "le dossier par défaut est \$HOME/.local/bin"
  else
    ko "le dossier par défaut est \$HOME/.local/bin" "$out"
  fi

  stray=$(find "$ROOT/fakehome" -type f ! -path "$ROOT/fakehome/.local/bin/typr" 2>/dev/null)
  if [ -z "$stray" ]; then ok "aucun fichier écrit hors du dossier d'installation"
  else ko "aucun fichier écrit hors du dossier d'installation" "$stray"; fi

  # Le dossier temporaire doit être nettoyé : un binaire partiellement
  # téléchargé qui traînerait là n'est pas installé, mais traîne.
  local leftovers
  leftovers=$(find "${TMPDIR:-/tmp}" -maxdepth 1 -name 'typr-install.*' 2>/dev/null)
  if [ -z "$leftovers" ]; then ok "aucun dossier temporaire laissé derrière"
  else ko "aucun dossier temporaire laissé derrière" "$leftovers"; fi
}

# ---------------------------------------------------------------------------

main() {
  printf 'install.sh — tests\n\n'
  make_origin

  test_static_analysis
  test_help
  test_dry_run
  test_happy_path
  test_pinned_version
  test_beta_channel
  test_bad_checksum
  test_missing_checksums
  test_missing_release
  test_path_present
  test_contained_writes

  printf '\n%d réussis, %d échoués\n' "$PASSED" "$FAILED"
  [ "$FAILED" -eq 0 ]
}

main "$@"