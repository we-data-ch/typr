#!/usr/bin/env bash
# Tests de packaging/render.sh (manifestes WinGet et Scoop), sans réseau.
#
# Le générateur ne lit qu'une chose : le `checksums.txt` d'une release. Toute sa
# valeur tient donc à trois propriétés qu'aucune relecture ne montre —
#
#   1. il prend **la** bonne ligne (le fichier liste huit archives, dont six
#      Linux/macOS et les deux GNU qu'un canal Windows n'a jamais le droit de
#      pointer) ;
#   2. il necroise jamais une URL avec l'empreinte d'une autre cible — les deux
#      cibles Windows ne diffèrent que par un mot du nom ;
#   3. il refuse de produire un manifeste à moitié rendu.
#
# C'est ce que ces tests couvrent, plus la dérive entre les gabarits et le
# générateur — le défaut qui n'apparaît qu'à la prochaine release, chez
# l'utilisateur.
#
#   packaging/tests/run-tests.sh
#
# Le même script est exécuté par le job `packaging` de ci.yml.

set -uo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
PACKAGING_DIR=$(cd "$HERE/.." && pwd)
RENDER="$PACKAGING_DIR/render.sh"
FIXTURE="$HERE/fixtures/checksums-v9.9.9.txt"
VALIDATE="$HERE/validate-schemas.py"

TAG="v9.9.9"
VERSION="9.9.9"
RELEASE_DATE="2026-01-02"

# Les deux cibles Windows, dans l'ordre où le générateur les déclare.
WIN_X86="x86_64-pc-windows-msvc"
WIN_ARM="aarch64-pc-windows-msvc"

WINGET_VERSION_FILE="we-data-ch.TypR.yaml"
WINGET_INSTALLER_FILE="we-data-ch.TypR.installer.yaml"
WINGET_LOCALE_FILE="we-data-ch.TypR.locale.en-US.yaml"

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

# Un manifeste WinGet porte des commentaires qui expliquent le rendu — dont
# certains nomment précisément ce qui ne doit pas apparaître (pourquoi pas
# d'`InstallLocation`, par exemple). Les contrôles de contenu portent donc sur
# les *champs*, pas sur le texte : un commentaire ne télécharge rien, et une
# assertion qui le prendrait en compte finirait un jour par interdire sa propre
# documentation.
fields() { # fields <fichier yaml>
  grep -v '^[[:space:]]*#' "$1" || true
}

# L'empreinte de l'artefact dans la fixture — la valeur attendue, lue depuis la
# fixture et non recopiée ici. Le tag fait partie du motif : c'est lui qui
# distingue deux releases, et deux releases ont des empreintes différentes.
fixture_sha() { # fixture_sha <cible> [fichier]
  grep -E "[[:space:]]\*?typr-$TAG-$1\.zip\$" "${2:-$FIXTURE}" | head -1 | cut -d' ' -f1
}

work() { # work <nom> — dossier de travail sous $ROOT, donc nettoyé avec lui
  local dir="$ROOT/$1"
  rm -rf "$dir"
  mkdir -p "$dir"
  printf '%s' "$dir"
}

render() { # render <sortie> [args...]
  local out="$1"; shift
  "$RENDER" --tag "$TAG" --checksums "$FIXTURE" --release-date "$RELEASE_DATE" --out "$out" 2>&1
}

# Le bloc attendu d'une cible WinGet : son architecture, son URL **et** son
# empreinte, côte à côte. Vérifier séparément que les quatre valeurs
# apparaissent dans le fichier ne prouverait rien — deux cibles interverties
# donneraient exactement le même texte. C'est l'erreur que le canal Homebrew a
# déjà laissée passer, une fois.
expect_winget_block() { # expect_winget_block <architecture> <cible>
  printf -- '- Architecture: %s\n  NestedInstallerFiles:\n  - RelativeFilePath: typr.exe\n    PortableCommandAlias: typr\n  InstallerUrl: https://github.com/we-data-ch/typr/releases/download/%s/typr-%s-%s.zip\n  InstallerSha256: %s' \
    "$1" "$TAG" "$TAG" "$2" "$(fixture_sha "$2")"
}

# Le manifeste Scoop, lu comme Scoop le lit : `url` et `hash` sont alignés par
# index, `bin` désigne l'exécutable, et l'alias WinGet doit désigner le même
# programme. C'est fait en Python parce que le point à vérifier — l'alignement
# index par index — n'est pas exprimable autrement.
check_scoop() { # check_scoop <manifeste.json> <manifeste installer.yaml> <libellé>
  # La lecture du manifeste Scoop est faite en Python : l'alignement index par
  # index de `url` et `hash` ne s'exprime pas autrement. Chaque problème y est
  # listé, ce que le shell ne sait pas faire.
  local details
  if details=$(python3 - "$1" "$2" "$FIXTURE" "$TAG" 2>&1 <<'PY'
import json, os, re, sys

manifest, installer, fixture, tag = sys.argv[1:5]
problems = []

with open(manifest, encoding="utf-8") as handle:
    doc = json.load(handle)

with open(fixture, encoding="utf-8") as handle:
    wanted = {}
    for line in handle:
        line = line.strip()
        if line and not line.startswith("#"):
            sha, _, name = line.partition("  ")
            wanted[name.strip()] = sha

if doc.get("version") != tag.lstrip("v"):
    problems.append(f"version : {doc.get('version')!r}, attendu {tag.lstrip('v')!r}")

urls, hashes = doc.get("url"), doc.get("hash")
if not isinstance(urls, list) or not isinstance(hashes, list):
    problems.append("url et hash doivent être des listes alignées")
else:
    if len(urls) != len(hashes):
        problems.append(f"{len(urls)} URL pour {len(hashes)} empreintes")
    for url, sha in zip(urls, hashes):
        name = url.rsplit("/", 1)[-1]
        if not re.fullmatch(r"[0-9a-f]{64}", sha):
            problems.append(f"empreinte mal formée pour {name} : {sha}")
        if wanted.get(name) != sha:
            problems.append(
                f"{name} → {sha}, alors que la release publie {wanted.get(name)}"
            )
        if f"typr-{tag}-" not in name:
            problems.append(f"{name} ne vient pas de la release {tag}")

bin_name = doc.get("bin")
if bin_name != "typr.exe":
    problems.append(f"bin : {bin_name!r}, attendu 'typr.exe'")

# Même programme dans les deux canaux : l'alias WinGet est le nom de l'exécutable
# Scoop, sans son extension.
alias = None
with open(installer, encoding="utf-8") as handle:
    for line in handle:
        if "PortableCommandAlias:" in line:
            alias = line.split(":", 1)[1].strip()
if isinstance(bin_name, str) and alias != os.path.splitext(bin_name)[0]:
    problems.append(f"alias WinGet {alias!r} ≠ exécutable Scoop {bin_name!r}")

for problem in problems:
    print(f"  {problem}")
sys.exit(1 if problems else 0)
PY
  ); then
    ok "$3"
  else
    ko "$3" "$details"
  fi
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
  # shellcheck disable=SC2015
  bash -n "$HERE/run-tests.sh" 2>&1 && ok "bash -n (suite)" || ko "bash -n (suite)"
  # Le cache d'octets est redirigé vers le répertoire de travail de la suite :
  # `py_compile` écrit d'ordinaire `__pycache__/` à côté du fichier, et
  # `packaging/tests/` n'a rien à faire committer.
  # shellcheck disable=SC2015
  PYTHONPYCACHEPREFIX="$(work pycache)" python3 -m py_compile "$VALIDATE" 2>&1 \
    && ok "py_compile" || ko "py_compile"

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
    if out=$(shellcheck "$HERE/run-tests.sh" 2>&1); then
      ok "shellcheck (suite)"
    else
      ko "shellcheck (suite)" "$out"
    fi
  else
    printf '  – shellcheck absent, non exécuté\n'
  fi
}

# ---------------------------------------------------------------------------
# Dérive gabarit ↔ générateur
#
# Un gabarit dont un marqueur a été renommé (`@@VERSION@@` → `@@VERSION_X@@`)
# produit un manifeste qui part en production avec `@@VERSION_X@@` dedans : rien
# ne le signale tant qu'un utilisateur n'installe pas. Le générateur le refuse,
# mais seulement s'il connaît encore le nom du marqueur — ce qui est précisément
# ce qui a pu changer. D'où ce test, qui compare les deux listes.
# ---------------------------------------------------------------------------
test_marker_drift() {
  echo "Dérive gabarit / générateur"

  local in_tpl in_gen only_tpl only_gen
  in_tpl=$(grep -oh '@@[A-Z0-9_]*@@' \
    "$PACKAGING_DIR/winget/$WINGET_VERSION_FILE.in" \
    "$PACKAGING_DIR/winget/$WINGET_INSTALLER_FILE.in" \
    "$PACKAGING_DIR/winget/$WINGET_LOCALE_FILE.in" \
    "$PACKAGING_DIR/scoop/typr.json.in" \
    "$PACKAGING_DIR/scoop/README.md.in" | sort -u)
  in_gen=$(grep -oh '@@[A-Z0-9_]*@@' "$RENDER" | sort -u)

  only_tpl=$(comm -23 <(printf '%s\n' "$in_tpl") <(printf '%s\n' "$in_gen") | tr '\n' ' ')
  if [ -z "$only_tpl" ]; then
    ok "aucun marqueur de gabarit inconnu du générateur"
  else
    ko "aucun marqueur de gabarit inconnu du générateur" "$only_tpl"
  fi

  only_gen=$(comm -13 <(printf '%s\n' "$in_tpl") <(printf '%s\n' "$in_gen") | tr '\n' ' ')
  if [ -z "$only_gen" ]; then
    ok "aucun marqueur du générateur absent des gabarits"
  else
    ko "aucun marqueur du générateur absent des gabarits" "$only_gen"
  fi

  # Un gabarit sans retour à la ligne final donne un fichier que les validateurs
  # de winget-pkgs et de Scoop refusent, et le retrait ne se voit qu'en
  # installant.
  local tpl
  for tpl in "$PACKAGING_DIR/winget/$WINGET_VERSION_FILE.in" \
             "$PACKAGING_DIR/winget/$WINGET_INSTALLER_FILE.in" \
             "$PACKAGING_DIR/winget/$WINGET_LOCALE_FILE.in" \
             "$PACKAGING_DIR/scoop/typr.json.in" \
             "$PACKAGING_DIR/scoop/README.md.in"; do
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
  out=$("$RENDER" --tag "$TAG" --release-date 2>&1)
  check_rc 2 $? "--release-date sans valeur sort en 2"
  out=$("$RENDER" --tag "$TAG" --out 2>&1)
  check_rc 2 $? "--out sans valeur sort en 2"

  # Le tag entre dans une URL, dans un nom de fichier
  # (`scoop/bin/typr/<version>.json`) et dans une substitution sed : une forme
  # trop libre passerait par la porte de l'injection.
  for tag in 'pas-un-tag' 'v1.2' 'v1.2.3/../../etc' 'vX.Y.Z' 'v1.2.3 ../../x' 'v1.2.3;rm'; do
    out=$("$RENDER" --tag "$tag" --checksums "$FIXTURE" --out "$(work bad-tag)" 2>&1)
    check_rc 2 $? "tag refusé : $tag"
  done

  # `ReleaseDate` est lue par WinGet comme une chaîne ; une date illisible ferait
  # échouer la validation du dépôt en ne parlant que d'elle.
  out=$("$RENDER" --tag "$TAG" --checksums "$FIXTURE" \
        --release-date '02/01/2026' --out "$(work bad-date)" 2>&1)
  check_rc 2 $? "release-date illisible refusée"
  check_contains "$out" "AAAA-MM-JJ" "le format attendu est nommé"
}

# ---------------------------------------------------------------------------
# Chemin heureux
# ---------------------------------------------------------------------------
test_render() {
  echo "Rendu complet"
  local out dir installer locale version_manifest
  dir=$(work render)
  out=$(render "$dir")
  check_rc 0 $? "rendu"
  check_contains "$out" "TypR $VERSION" "la version est annoncée"

  version_manifest="$dir/winget/$WINGET_VERSION_FILE"
  installer="$dir/winget/$WINGET_INSTALLER_FILE"
  locale="$dir/winget/$WINGET_LOCALE_FILE"

  check_file "$version_manifest" "le manifeste de version est écrit"
  check_file "$installer" "le manifeste d'installation est écrit"
  check_file "$locale" "la locale est écrite"
  check_file "$dir/scoop/bucket/typr.json" "le manifeste du bucket est écrit"
  check_file "$dir/scoop/bin/typr/$VERSION.json" "le manifeste d'historique est écrit"
  check_file "$dir/scoop/README.md" "le README du bucket est écrit"
  [ -f "$installer" ] || return 0

  # Trois manifestes, une seule version : WinGet refuse une pull request dont les
  # versions ne concordent pas, et il le dit d'une façon illisible
  # (« manifest version mismatch ») quand ça vient d'un copier-coller.
  local f
  for f in "$version_manifest" "$installer" "$locale" "$dir/scoop/bucket/typr.json"; do
    check_contains "$(cat "$f")" "$VERSION" "la version $VERSION est déclarée dans $(basename "$f")"
    check_not_contains "$(cat "$f")" "PackageVersion: v$VERSION" "aucun v initial dans le nom de version WinGet"
    check_not_contains "$(cat "$f")" "\"version\": \"v$VERSION\"" "aucun v initial dans la version Scoop"
  done

  check_contains "$(cat "$version_manifest")" "DefaultLocale: en-US" "la version désigne la locale en-US"
  check_contains "$(cat "$locale")" "PackageLocale: en-US" "la locale se déclare en-US"
  check_contains "$(cat "$version_manifest")" "ManifestType: version" "le manifeste se déclare version"
  check_contains "$(cat "$installer")" "ManifestType: installer" "l'installateur se déclare installer"
  check_contains "$(cat "$locale")" "ManifestType: defaultLocale" "la locale se déclare defaultLocale"

  # Chaque cible porte son URL et son empreinte côte à côte, sous son
  # architecture. Une cible intervertie donnerait le même nombre d'empreintes.
  check_contains "$(cat "$installer")" "$(expect_winget_block x64 "$WIN_X86")" \
    "architecture, URL et SHA corrects pour x64"
  check_contains "$(cat "$installer")" "$(expect_winget_block arm64 "$WIN_ARM")" \
    "architecture, URL et SHA corrects pour arm64"

  local shas
  shas=$(grep -cE '^[[:space:]]*InstallerSha256: [0-9a-f]{64}$' "$installer")
  if [ "$shas" -eq 2 ]; then ok "deux empreintes déclarées"; else ko "deux empreintes déclarées" "$shas trouvées"; fi

  # Les deux archives Windows ne diffèrent que par un mot du nom, et un `.` non
  # échappé dans le motif de recherche ferait matcher n'importe quoi à sa place.
  # Aucune des six archives Linux/macOS ne doit apparaître, et en particulier
  # pas les GNU, que le canal Homebrew a déjà confondus un jour.
  check_not_contains "$(fields "$installer")" "tar.gz" "aucune archive Linux/macOS dans l'installateur WinGet"
  check_not_contains "$(fields "$installer")" "unknown-linux" "aucune cible Linux dans l'installateur WinGet"
  check_not_contains "$(fields "$installer")" "apple-darwin" "aucune cible macOS dans l'installateur WinGet"
  local gnu
  gnu=$(grep -E '[[:space:]]typr-v9\.9\.9-x86_64-unknown-linux-gnu\.tar\.gz$' "$FIXTURE" | cut -d' ' -f1)
  check_not_contains "$(cat "$installer")$(cat "$dir/scoop/bucket/typr.json")" "$gnu" \
    "l'empreinte GNU n'est retenue dans aucun canal"
  check_not_contains "$(cat "$dir/scoop/bucket/typr.json")" "tar.gz" "aucune archive Linux/macOS dans le manifeste Scoop"

  # `url` et `hash` alignés par index, `bin` juste, alias concordant.
  check_scoop "$dir/scoop/bucket/typr.json" "$installer" \
    "le manifeste Scoop est cohérent (url/hash/bin/alias)"

  # L'historique et le bucket sont le même fichier : deux rendus distincts
  # divergeraient au premier changement de gabarit.
  if diff -q "$dir/scoop/bucket/typr.json" "$dir/scoop/bin/typr/$VERSION.json" >/dev/null 2>&1; then
    ok "le manifeste d'historique est identique au manifeste du bucket"
  else
    ko "le manifeste d'historique est identique au manifeste du bucket" \
      "$(diff "$dir/scoop/bucket/typr.json" "$dir/scoop/bin/typr/$VERSION.json" | head -4)"
  fi

  # Rien d'autre dans la sortie : un vieux fichier y servirait encore à son
  # adresse, dans un dépôt où l'historique fait foi.
  local extra expected
  expected=$(printf '%s\n' \
    "scoop/README.md" "scoop/bucket/typr.json" "scoop/bin/typr/$VERSION.json" \
    "winget/$WINGET_VERSION_FILE" "winget/$WINGET_INSTALLER_FILE" \
    "winget/$WINGET_LOCALE_FILE" | sort)
  extra=$(cd "$dir" && find . -type f | sed 's|^\./||' | sort | grep -vxF "$expected" || true)
  if [ -z "$extra" ]; then ok "aucun fichier surnumeraire dans la sortie"; else ko "aucun fichier surnumeraire dans la sortie" "$extra"; fi
}

# ---------------------------------------------------------------------------
# Les pièges WinGet et Scoop, une fois pour toutes
#
# Ce sont des défauts que rien n'affiche à la relecture : ils n'apparaissent que
# dans le dépôt de destination, ou pas du tout.
# ---------------------------------------------------------------------------
test_platform_pitfalls() {
  echo "Pièges WinGet et Scoop"
  local out dir installer manifest
  dir=$(work pitfalls)
  out=$(render "$dir")
  check_rc 0 $? "rendu de référence"

  installer="$dir/winget/$WINGET_INSTALLER_FILE"
  manifest="$dir/scoop/bucket/typr.json"
  [ -f "$installer" ] || return 0

  check_contains "$(cat "$installer")" 'ReleaseDate: "'"$RELEASE_DATE"'"' \
    "ReleaseDate est guillemeté (une date nue devient un objet date en YAML 1.1)"
  check_contains "$(cat "$installer")" "InstallerType: zip" "l'installateur est un zip"
  check_contains "$(cat "$installer")" "NestedInstallerType: portable" "le zip est traité comme portable"
  check_contains "$(cat "$installer")" "RelativeFilePath: typr.exe" "l'exécutable est nommé dans l'archive"

  # `InstallLocation` est un argument d'installeur, pas un dossier de destination
  # : un zip n'en accepte pas, et WinGet installe sans élévation de toute façon.
  check_not_contains "$(fields "$installer")" "InstallerSwitches" \
    "aucun champ InstallerSwitches sur un zip (dont InstallLocation, qui n'est pas un dossier)"
  check_not_contains "$(fields "$installer")" "Scope:" \
    "aucun champ Scope (le zip est déjà en installation utilisateur)"

  # Le schéma de Scoop refuse toute clé inconnue, `_comment` étant la seule
  # tolérée : une clé de confort se paie par un manifeste refusé.
  check_not_contains "$(cat "$manifest")" '"_comment_"' "aucune clé _comment_* dans le manifeste Scoop"
  local unknown
  unknown=$(python3 - "$manifest" <<'PY'
import json, sys
allowed = {"$schema", "_comment", "version", "description", "homepage", "license",
           "url", "hash", "bin", "notes", "architecture", "checkver", "autoupdate",
           "depends", "conflicts", "suggest", "installer", "uninstaller", "env_add_path",
           "env_set", "shortcuts", "psmodule", "pre_install", "post_install", "pre_uninstall",
           "post_uninstall", "persist", "extract_dir", "extract_to", "encoding", "innosetup"}
extra = sorted(set(json.load(open(sys.argv[1], encoding="utf-8"))) - allowed)
print(" ".join(extra))
PY
)
  if [ -z "$unknown" ]; then ok "aucune clé inconnue dans le manifeste Scoop"; else ko "aucune clé inconnue dans le manifeste Scoop" "$unknown"; fi
}

# ---------------------------------------------------------------------------
# Version épinglée et prerelease
# ---------------------------------------------------------------------------
test_pin_and_prerelease() {
  echo "Version épinglée et prerelease"
  local out dir sums

  # Sans le « v », comme install.sh qui le remet.
  dir=$(work no-v)
  out=$("$RENDER" --tag "$VERSION" --checksums "$FIXTURE" --release-date "$RELEASE_DATE" --out "$dir" 2>&1)
  check_rc 0 $? "un tag sans le v initial est accepté"
  check_contains "$(cat "$dir/winget/$WINGET_VERSION_FILE" 2>/dev/null)" "PackageVersion: $VERSION" \
    "la version est rendue sans le v"

  # release.yml marque `-alpha.N` et `-beta.N` prerelease : les deux canaux
  # doivent les accepter, sinon le canal WinGet s'arrêterait à la première
  # d'entre elles. La fixture est réécrite sur un tag de prerelease, parce
  # qu'une release prerelease publie bien ses propres archives.
  sums=$(work sums-prerelease)
  sed 's/v9\.9\.9/v0.5.13-alpha.1/g' "$FIXTURE" > "$sums/checksums.txt"
  dir=$(work prerelease)
  out=$("$RENDER" --tag v0.5.13-alpha.1 --checksums "$sums/checksums.txt" --release-date "$RELEASE_DATE" --out "$dir" 2>&1)
  check_rc 0 $? "une prerelease est rendue"
  check_contains "$(cat "$dir/winget/$WINGET_VERSION_FILE" 2>/dev/null)" "PackageVersion: 0.5.13-alpha.1" \
    "la prerelease est déclarée sans v"
  check_contains "$(cat "$dir/scoop/bucket/typr.json" 2>/dev/null)" '"version": "0.5.13-alpha.1"' \
    "la prerelease est déclarée dans le manifeste Scoop"
  check_file "$dir/scoop/bin/typr/0.5.13-alpha.1.json" "l'historique porte le nom de la prerelease"

  # Sans artefact à son nom, rien ne peut être rendu : une release dont le job de
  # build a échoué sur une cible Windows ne doit pas produire un manifeste qui
  # pointerait une autre archive.
  dir=$(work prerelease-missing)
  out=$("$RENDER" --tag v9.9.9-alpha.1 --checksums "$FIXTURE" --release-date "$RELEASE_DATE" --out "$dir" 2>&1)
  check_rc 1 $? "une prerelease sans artefacts propres est refusée"
  check_contains "$out" "ne contient aucune ligne pour" "l'absence est nommée, pas devinée"
  check_no_file "$dir/winget/$WINGET_VERSION_FILE" "aucun manifeste n'est écrit"

  # Dossier de sortie imbriqué absent : il doit être créé.
  dir="$ROOT/nested/deep/out"
  rm -rf "$ROOT/nested"
  out=$(render "$dir")
  check_rc 0 $? "dossier de sortie imbriqué créé"
  check_file "$dir/scoop/bucket/typr.json" "le manifeste est écrit dans le dossier imbriqué"
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

  # Sans la ligne d'un artefact : rien ne peut être rendu.
  rel=$(work sums-missing)
  grep -v "$WIN_ARM" "$FIXTURE" > "$rel/checksums.txt"
  out=$("$RENDER" --tag "$TAG" --checksums "$rel/checksums.txt" --release-date "$RELEASE_DATE" --out "$(work out-missing)" 2>&1)
  check_rc 1 $? "checksums.txt sans la ligne attendue → échec"
  check_contains "$out" "$WIN_ARM" "l'artefact manquant est nommé"
  check_no_file "$(work out-missing)/winget/$WINGET_INSTALLER_FILE" "aucun manifeste n'est écrit"
  check_no_file "$(work out-missing)/scoop/bucket/typr.json" "aucun manifeste Scoop n'est écrit"

  # Deux lignes pour le même nom : choisir l'une des deux reviendrait à publier
  # une empreinte arbitraire.
  rel=$(work sums-dup)
  { cat "$FIXTURE"; printf '%s  typr-%s-%s.zip\n' "$(fixture_sha "$WIN_X86")" "$TAG" "$WIN_X86"; } > "$rel/checksums.txt"
  out=$("$RENDER" --tag "$TAG" --checksums "$rel/checksums.txt" --release-date "$RELEASE_DATE" --out "$(work out-dup)" 2>&1)
  check_rc 1 $? "deux lignes pour le même artefact → échec"
  check_contains "$out" "2 lignes" "le nombre de lignes est nommé"
  check_no_file "$(work out-dup)/winget/$WINGET_INSTALLER_FILE" "aucun manifeste n'est écrit sur un doublon"

  # Malformée : la ligne existe, le SHA est illisible. Un fichier tronqué ne doit
  # pas produire un manifeste que winget-pkgs refusera ensuite — il le dirait par
  # le nom du champ, sans jamais nommer la release comme cause.
  # (64 zéros ne sont pas un cas de test : c'est un SHA bien formé, et le
  # générateur a raison de l'accepter.)
  for bad in 'pas-un-sha' \
             "$(fixture_sha "$WIN_X86" | cut -c1-63)" \
             "$(fixture_sha "$WIN_X86" | cut -c1-63)z" \
             "$(fixture_sha "$WIN_X86")0"; do
    rel=$(work sums-bad)
    awk -v bad="$bad" -v t="typr-$TAG-$WIN_X86.zip" \
      '{ if ($NF == t) print bad "  " $NF; else print }' "$FIXTURE" > "$rel/checksums.txt"
    out=$("$RENDER" --tag "$TAG" --checksums "$rel/checksums.txt" --release-date "$RELEASE_DATE" --out "$(work out-bad)" 2>&1)
    check_rc 1 $? "SHA malformé refusé : $bad"
    check_contains "$out" "malform" "la malformation est nommée"
  done

  # checksums.txt absent du tout.
  out=$("$RENDER" --tag "$TAG" --checksums "$ROOT/inexistant.txt" --out "$(work out-ghost)" 2>&1)
  check_rc 1 $? "checksums.txt absent → échec"
  check_contains "$out" "introuvable" "l'absence est nommée"
}

# ---------------------------------------------------------------------------
# Gabarit modifié : le générateur doit refuser plutôt que publier tel quel
# ---------------------------------------------------------------------------
test_template_drift() {
  echo "Gabarit modifié"
  local out sandbox
  sandbox=$(work sandbox)
  cp "$RENDER" "$sandbox/render.sh"
  mkdir -p "$sandbox/winget" "$sandbox/scoop"
  cp "$PACKAGING_DIR/winget/"*.in "$sandbox/winget/"
  cp "$PACKAGING_DIR/scoop/"*.in "$sandbox/scoop/"

  # Le marqueur est renommé côté gabarit, pas côté générateur : exactement le
  # réagencement de nom qu'un commit peut introduire.
  sed -i 's/@@VERSION@@/@@TYPR_VERSION@@/' "$sandbox/winget/$WINGET_VERSION_FILE.in"
  out=$("$sandbox/render.sh" --tag "$TAG" --checksums "$FIXTURE" --release-date "$RELEASE_DATE" --out "$(work out-drift)" 2>&1)
  check_rc 1 $? "un marqueur renommé est refusé"
  check_contains "$out" "marqueur" "le marqueur non substitué est nommé"
  check_no_file "$(work out-drift)/winget/$WINGET_VERSION_FILE" "aucun manifeste n'est écrit"

  # Retrait du retour à la ligne final : invisible dans une diff, fatal pour la
  # validation du dépôt de destination.
  cp "$PACKAGING_DIR/winget/$WINGET_VERSION_FILE.in" "$sandbox/winget/"
  printf '%s' "$(cat "$PACKAGING_DIR/winget/$WINGET_VERSION_FILE.in")" > "$sandbox/winget/$WINGET_VERSION_FILE.in"
  out=$("$sandbox/render.sh" --tag "$TAG" --checksums "$FIXTURE" --release-date "$RELEASE_DATE" --out "$(work out-nonl)" 2>&1)
  check_rc 1 $? "un manifeste sans retour à la ligne final est refusé"
  check_contains "$out" "retour à la ligne" "la raison est nommée"

  # Un gabarit amputé d'une cible : le compte d'empreintes le remarque, au lieu
  # de pousser un instalteur qui ne dit plus rien de l'arm64.
  cp "$PACKAGING_DIR/winget/$WINGET_VERSION_FILE.in" "$sandbox/winget/"
  cp "$PACKAGING_DIR/winget/$WINGET_INSTALLER_FILE.in" "$sandbox/winget/"
  sed -i '/^- Architecture: arm64/,/^  InstallerSha256: [0-9a-f]\{64\}$/d' \
    "$sandbox/winget/$WINGET_INSTALLER_FILE.in"
  out=$("$sandbox/render.sh" --tag "$TAG" --checksums "$FIXTURE" --release-date "$RELEASE_DATE" --out "$(work out-one-arch)" 2>&1)
  check_rc 1 $? "un gabarit amputé d'une cible est refusé"
  check_contains "$out" "empreinte" "le compte d'empreintes est nommé"
}

# ---------------------------------------------------------------------------
# Schémas officiels
#
# WinGet refuse en amont toute pull request dont un manifeste ne passe pas son
# JSON Schema. Le vérifier ici coûte deux secondes ; le faire dire par le dépôt
# coûte une release.
# ---------------------------------------------------------------------------
test_schemas() {
  echo "Schémas officiels"

  local out dir
  dir=$(work schemas)
  out=$(render "$dir")
  check_rc 0 $? "rendu de référence"

  out=$(python3 "$VALIDATE" "$dir" 2>&1)
  local rc=$?
  if [ "$rc" -eq 0 ]; then
    ok "les manifestes rendus passent les schémas officiels"
  elif [ "$rc" -eq 2 ]; then
    printf '  – validation sautée : %s\n' "$(printf '%s' "$out" | tail -1)"
  else
    ko "les manifestes rendus passent les schémas officiels" "$out"
  fi

  # Un manifeste cassé doit être rejeté, sinon ce test ne prouve rien.
  local broken
  broken=$(work schemas-broken)
  cp -r "$dir/." "$broken/"
  sed -i 's/^  InstallerSha256: .*/  InstallerSha256: pas-un-sha/' "$broken/winget/$WINGET_INSTALLER_FILE"
  out=$(python3 "$VALIDATE" "$broken" 2>&1)
  check_rc 1 $? "un manifeste invalide est rejeté"
  check_contains "$out" "✗" "le manifeste fautif est nommé"

  # Hors ligne sans cache : ni échec ni faux positif — le contrôle est déclaré
  # non fait, pas simulé.
  out=$(TYPR_SCHEMA_CACHE="$(work no-schemas)" "$VALIDATE" --offline "$dir" 2>&1)
  check_rc 2 $? "hors ligne sans cache, la validation est déclarée sautée"
}

# ---------------------------------------------------------------------------
# Lecture par URL — le mode réel, celui des jobs `winget` et `scoop`
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

  python3 "$PACKAGING_DIR/../install/tests/fixture-server.py" "$server_root" \
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

  # L'URL par défaut dérive du tag : c'est elle que les jobs de release
  # utilisent, sans jamais l'écrire en dur.
  dir=$(work url)
  out=$(TYPR_ORIGIN="http://127.0.0.1:$port" "$RENDER" --tag "$TAG" --release-date "$RELEASE_DATE" --out "$dir" 2>&1)
  check_rc 0 $? "l'URL de checksums.txt est déduite du tag"
  check_contains "$(cat "$dir/winget/$WINGET_INSTALLER_FILE" 2>/dev/null)" "$(fixture_sha "$WIN_X86")" \
    "l'installateur WinGet rendu par URL a le bon sha256"
  check_contains "$(cat "$dir/scoop/bucket/typr.json" 2>/dev/null)" "$(fixture_sha "$WIN_ARM")" \
    "le manifeste Scoop rendu par URL a le bon sha256"

  # 404 : le message doit nommer l'URL, pas laisser croire à un bug de rendu.
  out=$(TYPR_ORIGIN="http://127.0.0.1:$port" "$RENDER" --tag v1.2.3 --release-date "$RELEASE_DATE" --out "$(work url-404)" 2>&1)
  check_rc 1 $? "une release absente échoue"
  check_contains "$out" "introuvable" "l'absence est nommée"
}

# ---------------------------------------------------------------------------

main() {
  printf 'render.sh (WinGet + Scoop) — tests\n\n'
  ROOT=$(mktemp -d)

  test_static_analysis
  test_marker_drift
  test_usage
  test_render
  test_platform_pitfalls
  test_pin_and_prerelease
  test_missing_line
  test_template_drift
  test_schemas
  test_url_mode

  printf '\n%d réussis, %d échoués\n' "$PASSED" "$FAILED"
  [ "$FAILED" -eq 0 ]
}

main "$@"
