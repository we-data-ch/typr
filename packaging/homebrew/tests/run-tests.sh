#!/usr/bin/env bash
# Tests de packaging/homebrew/render.sh, sans réseau.
#
# Le générateur ne lit qu'une chose : le `checksums.txt` d'une release. Toute sa
# valeur tient donc à deux propriétés qu'aucune relecture ne montre —
#
#   1. il prend **la** bonne ligne (le fichier liste huit archives, dont les
#      deux GNU que le bloc `on_linux` ne doit jamais pointer) ;
#   2. il refuse de produire une formule à moitié rendue.
#
# C'est ce que ces tests couvrent, plus la dérive entre les gabarits et le
# générateur — le défaut qui n'apparaît qu'à la prochaine release, chez
# l'utilisateur.
#
#   packaging/homebrew/tests/run-tests.sh
#
# Le même script est exécuté par le job `packaging` de ci.yml.

set -uo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT_DIR=$(cd "$HERE/.." && pwd)
RENDER="$ROOT_DIR/render.sh"
FIXTURE="$HERE/fixtures/checksums-v9.9.9.txt"

# La release sans binaire musl, c'est-à-dire l'état réel de toutes les versions
# publiées jusqu'ici : les cibles `*-unknown-linux-musl` sont entrées dans la
# matrice après v0.5.12.
LEGACY_TAG="v0.5.12"
LEGACY_FIXTURE="$HERE/fixtures/checksums-v0.5.12-legacy.txt"

TAG="v9.9.9"

PASSED=0
FAILED=0
ROOT=""
SERVER_PID=""

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
  # Un motif vide matcherait n'importe quoi : l'assertion passerait sans rien
  # vérifier, ce qui est pire que ne pas tester. Il est donc refusé.
  if [ -z "$2" ]; then ko "$3" "motif de recherche vide — l'assertion ne vérifierait rien"; return 0; fi
  case "$1" in
    *"$2"*) ok "$3" ;;
    *)      ko "$3" "« $2 » absent de la sortie : $1" ;;
  esac
}

check_not_contains() {
  if [ -z "$2" ]; then ko "$3" "motif de recherche vide — l'assertion ne vérifierait rien"; return 0; fi
  case "$1" in
    *"$2"*) ko "$3" "« $2 » présent alors qu'il ne devait pas l'être" ;;
    *)      ok "$3" ;;
  esac
}

check_file() { # check_file <chemin> <libellé>
  if [ -f "$1" ]; then ok "$2"; else ko "$2" "$1 absent"; fi
}

check_no_file() {
  if [ -e "$1" ]; then ko "$2" "$1 présent"; else ok "$2"; fi
}

# L'empreinte de l'artefact dans la fixture — la valeur attendue, lue depuis la
# source du générateur et non recopiée ici. Le tag fait partie du motif : c'est
# lui qui distingue deux releases, et deux releases ont des empreintes
# différentes.
fixture_sha() { # fixture_sha <tag> <cible> [fichier]
  grep -E "[[:space:]]\*?typr-$1-$2(\.tar\.gz|\.zip)\$" "${3:-$FIXTURE}" | head -1 | cut -d' ' -f1
}

work() { # work <nom> — dossier de travail sous $ROOT, donc nettoyé avec lui
  local dir="$ROOT/$1"
  rm -rf "$dir"
  mkdir -p "$dir"
  printf '%s' "$dir"
}

render() { # render <sortie> <args...>
  local out="$1"; shift
  "$RENDER" --tag "$TAG" --checksums "$FIXTURE" --out "$out" "$@" 2>&1
}

# Le bloc attendu d'une cible : son URL **et** son sha, côte à côte. Vérifier
# séparément que les quatre empreintes apparaissent dans le fichier ne prouverait
# rien — deux cibles interverties donneraient exactement le même texte.
expect_block() { # expect_block <cible>
  printf '      url "https://github.com/we-data-ch/typr/releases/download/%s/typr-%s-%s.tar.gz"\n      sha256 "%s"' \
    "$TAG" "$TAG" "$1" "$(fixture_sha "$TAG" "$1")"
}

# ---------------------------------------------------------------------------
# Analyse statique
# ---------------------------------------------------------------------------
test_static_analysis() {
  echo "Analyse statique"

  # `ok` et `ko` sortent toujours en 0, donc le `||` ne se déclenche jamais à
  # tort : SC2015 signalerait ici un if-then-else qui n'existe pas.
  # shellcheck disable=SC2015
  sh -n "$RENDER" 2>&1 && ok "sh -n" || ko "sh -n"
  # shellcheck disable=SC2015
  bash -n "$RENDER" 2>&1 && ok "bash -n" || ko "bash -n"

  if command -v dash >/dev/null 2>&1; then
    # shellcheck disable=SC2015
    dash -n "$RENDER" 2>&1 && ok "dash -n (POSIX strict)" || ko "dash -n"
  else
    printf '  – dash absent, POSIX strict non vérifié\n'
  fi

  if command -v shellcheck >/dev/null 2>&1; then
    # Aucune exclusion : render.sh ne se sert ni de `local` (non POSIX) ni
    # d'aucun attribut bash.
    if out=$(shellcheck -s sh "$RENDER" 2>&1); then
      ok "shellcheck"
    else
      ko "shellcheck" "$out"
    fi
  else
    printf '  – shellcheck absent, non exécuté\n'
  fi
}

# ---------------------------------------------------------------------------
# Dérive gabarit ↔ générateur
#
# Un gabarit dont un marqueur a été renommé (`@@VERSION@@` → `@@VERSION_X@@`)
# produit une formule qui part en production avec `@@VERSION_X@@` dedans : rien
# ne le signale tant qu'un utilisateur n'installe pas. Le générateur le refuse,
# mais seulement s'il connaît encore le nom du marqueur — ce qui est précisément
# ce qui a pu changer. D'où ce test, qui compare les deux listes.
# ---------------------------------------------------------------------------
test_marker_drift() {
  echo "Dérive gabarit / générateur"

  local in_tpl in_gen only_tpl only_gen
  in_tpl=$(grep -oh '@@[A-Z0-9_]*@@' \
    "$ROOT_DIR/Formula/typr.rb.in" \
    "$ROOT_DIR/parts/on_linux.rb.in" \
    "$ROOT_DIR/parts/on_linux_absent.rb.in" \
    "$ROOT_DIR/README.md.in" | sort -u)
  in_gen=$(grep -oh '@@[A-Z0-9_]*@@' "$RENDER" | sort -u)

  only_tpl=$(comm -23 <(printf '%s\n' "$in_tpl") <(printf '%s\n' "$in_gen") | tr '\n' ' ')
  [ -z "$only_tpl" ] \
    && ok "aucun marqueur de gabarit inconnu du générateur" \
    || ko "aucun marqueur de gabarit inconnu du générateur" "$only_tpl"

  only_gen=$(comm -13 <(printf '%s\n' "$in_tpl") <(printf '%s\n' "$in_gen") | tr '\n' ' ')
  [ -z "$only_gen" ] \
    && ok "aucun marqueur du générateur absent des gabarits" \
    || ko "aucun marqueur du générateur absent des gabarits" "$only_gen"

  # Un gabarit sans retour à la ligne final donne une formule que `brew style`
  # refuse, et le retrait ne se voit qu'en installant.
  local tpl
  for tpl in "$ROOT_DIR/Formula/typr.rb.in" \
             "$ROOT_DIR/parts/on_linux.rb.in" \
             "$ROOT_DIR/parts/on_linux_absent.rb.in" \
             "$ROOT_DIR/README.md.in"; do
    if [ -z "$(tail -c1 "$tpl")" ]; then
      ok "$(basename "$tpl") se termine par un retour à la ligne"
    else
      ko "$(basename "$tpl") se termine par un retour à la ligne" "$tpl"
    fi
  done
}

# ---------------------------------------------------------------------------
# Ligne de commande
# ---------------------------------------------------------------------------
test_usage() {
  echo " --help et erreurs de saisie"
  local out
  out=$("$RENDER" --help 2>&1)
  check_rc 0 $? "--help sort en 0"
  check_contains "$out" "Usage" "--help affiche l'usage"

  out=$("$RENDER" --bogus 2>&1)
  check_rc 2 $? "un argument inconnu sort en 2"
  check_contains "$out" "argument inconnu" "l'argument est nommé"

  out=$("$RENDER" --out /tmp 2>&1)
  check_rc 2 $? "--tag absent sort en 2"
  check_contains "$out" "--tag est obligatoire" "l'absence de --tag est nommée"

  out=$("$RENDER" --tag 2>&1)
  check_rc 2 $? "--tag sans valeur sort en 2"

  # Le tag entre dans une URL et dans une substitution sed : une forme trop
  # libre passerait par la porte de l'injection.
  for tag in 'pas-un-tag' 'v1.2' 'v1.2.3/../../etc' 'vX.Y.Z' 'v1.2.3 ../../x'; do
    out=$("$RENDER" --tag "$tag" --checksums "$FIXTURE" --out "$(work bad-tag)" 2>&1)
    check_rc 2 $? "tag refusé : $tag"
  done
}

# ---------------------------------------------------------------------------
# Chemin heureux
# ---------------------------------------------------------------------------
test_render() {
  echo "Rendu complet"
  local out dir formula readme
  dir=$(work render)
  out=$(render "$dir")
  check_rc 0 $? "rendu"
  check_contains "$out" "TypR 9.9.9" "la version est annoncée"

  formula="$dir/Formula/typr.rb"
  readme="$dir/README.md"
  check_file "$formula" "Formula/typr.rb est écrit"
  check_file "$readme" "README.md est écrit"
  [ -f "$formula" ] || return 0

  check_contains "$(cat "$formula")" "version \"${TAG#v}\"" "la version est déclarée"
  check_contains "$(cat "$formula")" "class Typr < Formula" "la classe est Typr"

  local target
  for target in x86_64-apple-darwin aarch64-apple-darwin \
                x86_64-unknown-linux-musl aarch64-unknown-linux-musl; do
    check_contains "$(cat "$formula")" "$(expect_block "$target")" \
      "url et sha256 corrects pour $target"
  done

  # La vraie tentation du contrôleur d'"exactitude" : `typr-v9.9.9-x86_64-
  # unknown-linux-musl.tar.gz` et `…-linux-gnu.tar.gz` ne diffèrent que par un
  # mot, et un `.` non échappé dans le motif fait matcher n'importe quoi à sa
  # place. Le GNU ne doit donc apparaître nulle part.
  check_not_contains "$(cat "$formula")" "$(fixture_sha "$TAG" x86_64-unknown-linux-gnu)" \
    "le binaire GNU n'est pas retenu à la place du musl"
  check_not_contains "$(cat "$formula")" "unknown-linux-gnu" \
    "aucune cible GNU dans la formule"

  local shas
  shas=$(grep -cE '^[[:space:]]+sha256 "[0-9a-f]{64}"$' "$formula")
  if [ "$shas" -eq 4 ]; then ok "quatre empreintes déclarées"; else ko "quatre empreintes déclarées" "$shas trouvées"; fi

  # Le bloc `on_linux` est inséré depuis une autre fichier : son indentation
  # peut être aplatie sans que rien d'autre ne bouge, et `brew style` ne le
  # signalerait qu'à la release. On vérifie donc l'imbrication.
  check_contains "$(cat "$formula")" "  on_linux do
    on_intel do" "le bloc on_linux est imbriqué"

  # Invariant plus large, valable pour tout le corps de la classe : un
  # commentaire calé à la colonne 0 y serait encore du Ruby valide, donc
  # inoffensif — mais seulement parce qu'il est commentaire. Le jour où la même
  # transformation aplatit un `on_linux do`, le fichier resterait parsable et le
  # défaut passerait le test de syntaxe.
  local col0
  col0=$(sed -n '/^class Typr < Formula/,$p' "$formula" \
    | grep '^[^[:space:]]' \
    | grep -cvE '^(class Typr < Formula|end)$' || true)
  if [ "$col0" -eq 0 ]; then
    ok "aucune ligne du corps de la classe sans indentation"
  else
    ko "aucune ligne du corps de la classe sans indentation" \
       "$col0 lignes calées à la colonne 0 : $(sed -n '/^class Typr < Formula/,$p' "$formula" | grep '^[^[:space:]]' | head -2)"
  fi

  # Un marqueur non substitué partirait en production tel quel.
  check_not_contains "$(cat "$formula")" "@@" "aucun marqueur résiduel dans la formule"
  check_not_contains "$(cat "$readme")" "@@" "aucun marqueur résiduel dans le README"

  # Le README est dérivé de la formule, pas d'une deuxième source.
  check_contains "$(cat "$readme")" "Typed superset of R — transpiler and type checker" \
    "le README reprend le desc de la formule"
  check_contains "$(cat "$readme")" "https://we-data-ch.github.io/typr.github.io/" \
    "le README reprend le homepage de la formule"
  check_contains "$(cat "$readme")" "brew install we-data-ch/typr/typr" \
    "le README donne la commande d'installation"
  check_contains "$(cat "$readme")" "couvert" "le README annonce la couverture Linux"

  # Rien d'autre dans le dossier du tap : le job `install-hosting` refuse
  # lui-même de laisser un reste, et un vieux fichier servirait encore à son
  # adresse.
  local extra
  extra=$(cd "$dir" && find . -type f ! -path './Formula/typr.rb' ! -path './README.md')
  if [ -z "$extra" ]; then ok "aucun fichier surnumeraire dans le tap"; else ko "aucun fichier surnumeraire dans le tap" "$extra"; fi
}

# ---------------------------------------------------------------------------
# Sorties d'outils
# ---------------------------------------------------------------------------
test_pin_and_prerelease() {
  echo "Version épinglée et prerelease"
  local out dir

  # Sans le « v », comme install.sh qui le remet.
  dir=$(work no-v)
  out=$("$RENDER" --tag "9.9.9" --checksums "$FIXTURE" --out "$dir" 2>&1)
  check_rc 0 $? "un tag sans le v initial est accepté"
  check_contains "$(cat "$dir/Formula/typr.rb")" 'version "9.9.9"' "la version est rendue sans le v"

  # release.yml marque `-alpha.N` et `-beta.N` prerelease : la formule doit les
  # accepter, sinon le canal Homebrew s'arrêterait à la première d'entre elles.
  dir=$(work prerelease)
  out=$("$RENDER" --tag v9.9.9-alpha.1 --checksums "$FIXTURE" --out "$dir" 2>&1)
  check_rc 1 $? "une prerelease sans artefacts propres est refusée"
  check_contains "$out" "ne contient aucune ligne pour" "l'absence est nommée, pas devinée"

  # Dossier de sortie imbriqué absent : il doit être créé.
  dir="$ROOT/nested/deep/tap"
  rm -rf "$ROOT/nested"
  out=$("$RENDER" --tag "$TAG" --checksums "$FIXTURE" --out "$dir" 2>&1)
  check_rc 0 $? "dossier de sortie imbriqué créé"
  check_file "$dir/Formula/typr.rb" "la formule est écrite dans le dossier imbriqué"
}

# ---------------------------------------------------------------------------
# Refus
# ---------------------------------------------------------------------------
test_missing_line() {
  echo "Ligne absente, doublon, malformation"
  local out dir rel
  dir=$(work missing)
  out=$(render "$dir")
  check_rc 0 $? "rendu de référence"

  # Sans la ligne d'un artefact macOS : rien ne peut être rendu. v0.5.12 est la
  # seule version publiée, mais c'est aussi le cas banal d'une release dont le
  # job de build a échoué sur une cible.
  rel=$(work sums-missing)
  grep -v 'aarch64-apple-darwin' "$FIXTURE" > "$rel/checksums.txt"
  out=$("$RENDER" --tag "$TAG" --checksums "$rel/checksums.txt" --out "$(work out-missing)" 2>&1)
  check_rc 1 $? "checksums.txt sans la ligne attendue → échec"
  check_contains "$out" "aarch64-apple-darwin" "l'artefact manquant est nommé"
  check_no_file "$(work out-missing)/Formula/typr.rb" "aucune formule n'est écrite"

  # Deux lignes pour le même nom : choisir l'une des deux reviendrait à publier
  # une empreinte arbitraire.
  rel=$(work sums-dup)
  { cat "$FIXTURE"; printf '%s  typr-%s-x86_64-apple-darwin.tar.gz\n' \
      "$(fixture_sha "$TAG" aarch64-apple-darwin)" "$TAG"; } > "$rel/checksums.txt"
  out=$("$RENDER" --tag "$TAG" --checksums "$rel/checksums.txt" --out "$(work out-dup)" 2>&1)
  check_rc 1 $? "deux lignes pour le même artefact → échec"
  check_contains "$out" "2 lignes" "le nombre de lignes est nommé"
  check_no_file "$(work out-dup)/Formula/typr.rb" "aucune formule n'est écrite sur un doublon"

  # Malformée : la ligne existe, le SHA est illisible. Un fichier tronqué ne doit
  # pas produire une formule que Homebrew refuserait ensuite — il le dirait par
  # le nom du fichier, sans jamais nommer la release comme cause.
  for bad in 'pas-un-sha' \
             "$(fixture_sha "$TAG" x86_64-apple-darwin | cut -c1-63)" \
             "$(fixture_sha "$TAG" x86_64-apple-darwin | cut -c1-63)z"; do
    rel=$(work sums-bad)
    awk -v bad="$bad" -v t="typr-$TAG-x86_64-apple-darwin.tar.gz" \
      '{ if ($NF == t) print bad "  " $NF; else print }' "$FIXTURE" > "$rel/checksums.txt"
    out=$("$RENDER" --tag "$TAG" --checksums "$rel/checksums.txt" --out "$(work out-bad)" 2>&1)
    check_rc 1 $? "SHA malformé refusé : $bad"
    check_contains "$out" "malform" "la malformation est nommée"
  done

  # checksums.txt absent du tout.
  out=$("$RENDER" --tag "$TAG" --checksums "$ROOT/inexistant.txt" --out "$(work out-ghost)" 2>&1)
  check_rc 1 $? "checksums.txt absent → échec"
  check_contains "$out" "introuvable" "l'absence est nommée"
}

# ---------------------------------------------------------------------------
# Release sans binaire musl
#
# État réel de toutes les versions publiées : la formule doit rester valide en
# macOS seul plutôt que de refuser de se générer — et ne surtout pas pointer le
# binaire GNU à la place, ce qui réintroduirait le défaut `GLIBC_2.39 not found`.
# ---------------------------------------------------------------------------
test_without_musl() {
  echo "Release sans binaire musl"
  local out dir
  dir=$(work legacy)
  out=$("$RENDER" --tag "$LEGACY_TAG" --checksums "$LEGACY_FIXTURE" --out "$dir" 2>&1)
  check_rc 0 $? "le rendu aboutit malgré l'absence de musl"
  check_contains "$out" "on_linux est omis" "l'omission est annoncée"

  formula="$dir/Formula/typr.rb"
  check_file "$formula" "la formule est écrite"
  [ -f "$formula" ] || return 0

  check_not_contains "$(cat "$formula")" "on_linux do" "aucun bloc on_linux"
  # Le commentaire qui explique l'absence nomme les cibles attendues — c'est
  # voulu. Ce qui ne doit pas exister, c'est une *référence* à un artefact
  # Linux : le mot « musl » dans un commentaire ne téléchargerait rien.
  check_not_contains "$(cat "$formula")" "download/$LEGACY_TAG/typr-$LEGACY_TAG-x86_64-unknown-linux" \
    "aucun artefact Linux référencé"
  check_not_contains "$(cat "$formula")" "$(fixture_sha "$LEGACY_TAG" x86_64-unknown-linux-gnu "$LEGACY_FIXTURE")" \
    "le binaire GNU n'est pas utilisé comme repli"
  check_contains "$(cat "$formula")" "ne publie pas encore de binaire musl" \
    "la formule explique pourquoi Linux manque"
  check_contains "$(cat "$dir/README.md")" "pas encore" "le README ne promet pas Linux"

  local shas
  shas=$(grep -cE '^[[:space:]]+sha256 "[0-9a-f]{64}"$' "$formula")
  if [ "$shas" -eq 2 ]; then ok "deux empreintes déclarées (macOS)"; else ko "deux empreintes déclarées (macOS)" "$shas trouvées"; fi
}

# ---------------------------------------------------------------------------
# Gabarit modifié : le générateur doit refuser plutôt que publier tel quel
# ---------------------------------------------------------------------------
test_template_drift() {
  echo "Gabarit modifié"
  local out dir sandbox
  sandbox=$(work sandbox)
  cp "$RENDER" "$sandbox/render.sh"
  mkdir -p "$sandbox/Formula" "$sandbox/parts"
  cp "$ROOT_DIR/Formula/typr.rb.in" "$sandbox/Formula/"
  cp "$ROOT_DIR/parts/on_linux.rb.in" "$ROOT_DIR/parts/on_linux_absent.rb.in" "$sandbox/parts/"
  cp "$ROOT_DIR/README.md.in" "$sandbox/"

  # Le marqueur est renommé côté gabarit, pas côté générateur : exactement le
  # réagencement de nom qu'un commit peut introduire.
  sed -i 's/@@VERSION@@/@@TYPR_VERSION@@/' "$sandbox/Formula/typr.rb.in"
  out=$("$sandbox/render.sh" --tag "$TAG" --checksums "$FIXTURE" --out "$(work out-drift)" 2>&1)
  check_rc 1 $? "un marqueur renommé est refusé"
  check_contains "$out" "marqueur" "le marqueur non substitué est nommé"
  check_no_file "$(work out-drift)/Formula/typr.rb" "aucune formule n'est écrite"

  # Retrait du retour à la ligne final : invisible dans une diff, fatal pour
  # `brew style`.
  cp "$ROOT_DIR/Formula/typr.rb.in" "$sandbox/Formula/"
  printf '%s' "$(cat "$ROOT_DIR/Formula/typr.rb.in")" > "$sandbox/Formula/typr.rb.in"
  out=$("$sandbox/render.sh" --tag "$TAG" --checksums "$FIXTURE" --out "$(work out-nonl)" 2>&1)
  check_rc 1 $? "une formule sans retour à la ligne final est refusée"
  check_contains "$out" "retour à la ligne" "la raison est nommée"
}

# ---------------------------------------------------------------------------
# Lecture par URL — le mode réel, celui du job `brew`
#
# Le serveur de fixtures est celui de install/tests : il sert une arborescence
# réelle, donc un vrai chemin `/releases/download/<tag>/checksums.txt`. Réécrire
# un deuxième serveur pour le même besoin serait du poids inutile.
# ---------------------------------------------------------------------------
test_url_mode() {
  echo "Lecture par URL"
  if ! command -v python3 >/dev/null 2>&1; then
    printf '  – python3 absent, mode URL non exercé\n'
    return 0
  fi

  local server_root port out dir
  server_root=$(work origin)
  mkdir -p "$server_root/we-data-ch/typr/releases/download/$TAG"
  cp "$FIXTURE" "$server_root/we-data-ch/typr/releases/download/$TAG/checksums.txt"

  python3 "$ROOT_DIR/../../install/tests/fixture-server.py" "$server_root" \
    > "$server_root/.port" &
  SERVER_PID=$!

  port=""
  for _ in $(seq 1 50); do
    port=$(grep -oE 'PORT [0-9]+' "$server_root/.port" 2>/dev/null | cut -d' ' -f2)
    [ -n "$port" ] && break
    sleep 0.1
  done
  if [ -z "$port" ]; then
    ko "le serveur de fixtures n'a pas démarré"
    return 0
  fi

  # L'URL par défaut dérive du tag : c'est elle que le job `brew` utilise, sans
  # jamais l'écrire en dur.
  dir=$(work url)
  out=$(TYPR_ORIGIN="http://127.0.0.1:$port" "$RENDER" --tag "$TAG" --out "$dir" 2>&1)
  check_rc 0 $? "l'URL de checksums.txt est déduite du tag"
  check_contains "$(cat "$dir/Formula/typr.rb" 2>/dev/null)" "$(fixture_sha "$TAG" x86_64-apple-darwin)" \
    "la formule rendue par URL a le bon sha256"

  # 404 : le message doit nommer l'URL, pas laisser croire à un bug de rendu.
  out=$(TYPR_ORIGIN="http://127.0.0.1:$port" "$RENDER" --tag v1.2.3 --out "$(work url-404)" 2>&1)
  check_rc 1 $? "une release absente échoue"
  check_contains "$out" "introuvable" "l'absence est nommée"
}

# ---------------------------------------------------------------------------

main() {
  printf 'render.sh — tests\n\n'
  ROOT=$(mktemp -d)

  test_static_analysis
  test_marker_drift
  test_usage
  test_render
  test_pin_and_prerelease
  test_missing_line
  test_without_musl
  test_template_drift
  test_url_mode

  printf '\n%d réussis, %d échoués\n' "$PASSED" "$FAILED"
  [ "$FAILED" -eq 0 ]
}

main "$@"