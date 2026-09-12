# publish.nu — orchestration des versions de TypR
#
# Source de vérité unique : [workspace.package] version dans Cargo.toml.
# Tout le reste (extension VS Code, runner RStudio) en est dérivé par `sync`.
# La publication elle-même (crates.io, binaires, Docker, WASM, Marketplace)
# est faite par .github/workflows/release.yml, déclenché par le tag.
#
# Usage :
#   nu publish.nu version                  # version locale + versions dérivées
#   nu publish.nu version --set 0.6.0      # fixe une version exacte
#   nu publish.nu bump patch|minor|major   # incrémente et synchronise
#   nu publish.nu sync                     # propage la version aux éditeurs
#   nu publish.nu check                    # compare tous les canaux publiés
#   nu publish.nu release [--dry-run]      # tag + push du tag → la CI prend le relais
#   nu publish.nu ship patch|minor|major   # TOUT : bump, PR, attente CI, fusion, tag
#
# `release` ne pousse QUE le tag : main est protégée, son contenu y arrive par PR.

const CARGO_FILE = "Cargo.toml"
const VSCODE_PKG = "editors/vscode/package.json"
const VSCODE_LOCK = "editors/vscode/package-lock.json"
const RSTUDIO_DESC = "editors/rstudio/DESCRIPTION"
const GH_REPO = "we-data-ch/typr"
const DOCKER_REPO = "fabricehategekimana/typr"
# Ordre de publication imposé par le graphe de dépendances :
# typr-core → typr-lsp → typr-mcp → typr-cli → typr
const CRATES = ["typr-core", "typr-lsp", "typr-mcp", "typr-cli", "typr"]

# --- lecture / écriture de la version ----------------------------------------

def current-version []: nothing -> string {
  open $CARGO_FILE | get workspace.package.version
}

def validate-version [v: string] {
  if not ($v =~ '^\d+\.\d+\.\d+$') {
    error make { msg: $"format de version invalide : ($v) — attendu X.Y.Z" }
  }
}

def write-version [v: string] {
  validate-version $v
  # Ne remplace la ligne `version` qu'à l'intérieur du bloc [workspace.package]
  sed -i $"/^\\[workspace\\.package\\]/,/^\\[/ s/^version = .*/version = \"($v)\"/" $CARGO_FILE
  sync-editors $v
}

# --- propagation aux périphériques -------------------------------------------

def sync-editors [v: string] {
  if ($VSCODE_PKG | path exists) {
    sed -i $"0,/^  \"version\":/ s/^  \"version\": \".*\",/  \"version\": \"($v)\",/" $VSCODE_PKG
    print $"  ($VSCODE_PKG) → ($v)"
  }
  # Le lock porte la version en double (racine + packages.""), toutes deux dans
  # les douze premières lignes ; sans ça il dérive et le .vsix publié annonce
  # une version différente de celle du manifeste.
  if ($VSCODE_LOCK | path exists) {
    sed -i $"1,12 s/^  \"version\": \".*\",/  \"version\": \"($v)\",/" $VSCODE_LOCK
    sed -i $"1,12 s/^      \"version\": \".*\",/      \"version\": \"($v)\",/" $VSCODE_LOCK
    print $"  ($VSCODE_LOCK) → ($v)"
  }
  if ($RSTUDIO_DESC | path exists) {
    sed -i $"s/^Version: .*/Version: ($v)/" $RSTUDIO_DESC
    print $"  ($RSTUDIO_DESC) → ($v)"
  }
}

def derived-versions []: nothing -> table {
  mut rows = []
  if ($VSCODE_PKG | path exists) {
    $rows = ($rows | append { cible: "extension VS Code", version: (open $VSCODE_PKG | get version) })
  }
  if ($RSTUDIO_DESC | path exists) {
    let desc = (open --raw $RSTUDIO_DESC | lines | where { |l| $l starts-with "Version:" } | first)
    $rows = ($rows | append { cible: "runner RStudio", version: ($desc | str replace "Version:" "" | str trim) })
  }
  $rows
}

# --- sous-commandes -----------------------------------------------------------

def "main version" [--set: string] {
  if $set != null {
    write-version $set
    print $"version fixée à ($set)"
    return
  }
  print $"Cargo.toml \(source de vérité\) : (ansi cyan)(current-version)(ansi reset)"
  derived-versions | print
}

def "main bump" [level: string = "patch"] {
  let parts = (current-version | split row "." | each { into int })
  let next = match $level {
    "major" => $"($parts.0 + 1).0.0"
    "minor" => $"($parts.0).($parts.1 + 1).0"
    "patch" => $"($parts.0).($parts.1).($parts.2 + 1)"
    _ => { error make { msg: $"niveau inconnu : ($level) — patch|minor|major" } }
  }
  write-version $next
  print $"version : (current-version) → (ansi green)($next)(ansi reset)"
}

def "main sync" [] {
  let v = (current-version)
  print $"propagation de ($v) :"
  sync-editors $v
}

def "main check" [] {
  let local = (current-version)

  print $"(char nl)=== crates.io ==="
  for name in $CRATES {
    let resp = (try {
      http get --headers [User-Agent typr-publish] $"https://crates.io/api/v1/crates/($name)"
    } catch { null })
    if $resp == null {
      print $"  ($name): (ansi red)absent de crates.io(ansi reset)"
    } else {
      let v = $resp.crate.max_version
      let mark = if $v == $local { $"(ansi green)($v)(ansi reset)" } else { $"(ansi yellow)($v)(ansi reset) ← retard" }
      print $"  ($name): ($mark)"
    }
  }

  print $"(char nl)=== GitHub Releases ==="
  let rel = (try { http get $"https://api.github.com/repos/($GH_REPO)/releases/latest" } catch { null })
  if $rel == null {
    print $"  (ansi red)injoignable(ansi reset)"
  } else {
    print $"  ($rel.tag_name) — ($rel.published_at)"
  }

  print $"(char nl)=== Docker Hub ==="
  let tags = (try {
    http get $"https://hub.docker.com/v2/repositories/($DOCKER_REPO)/tags?page_size=5"
  } catch { null })
  if $tags == null {
    print $"  (ansi red)injoignable(ansi reset)"
  } else {
    for t in $tags.results { print $"  ($t.name) — ($t.last_updated)" }
  }

  print $"(char nl)=== local ==="
  print $"  Cargo.toml : (ansi cyan)($local)(ansi reset)"
  derived-versions | print
}

def "main release" [--dry-run] {
  let v = (current-version)
  let tag = $"v($v)"

  # 1. l'arbre de travail doit être propre — on ne commite jamais à l'aveugle
  let dirty = (git status --porcelain | complete | get stdout | str trim)
  if ($dirty != "") {
    print $"(ansi red)L'arbre de travail n'est pas propre :(ansi reset)"
    print $dirty
    error make { msg: "commite ou remise tes changements avant de publier" }
  }

  # 2. la release part de main, jamais d'une branche de travail
  let branch = (git rev-parse --abbrev-ref HEAD | complete | get stdout | str trim)
  if $branch != "main" {
    error make { msg: $"release depuis ($branch) — bascule sur main d'abord" }
  }

  # 3. main est protégée : ce script ne pousse que le tag, jamais la branche.
  #    Le contenu doit donc déjà être sur origin/main, arrivé par une PR.
  git fetch --quiet origin main
  let local_sha = (git rev-parse HEAD | complete | get stdout | str trim)
  let remote_sha = (git rev-parse origin/main | complete | get stdout | str trim)
  if $local_sha != $remote_sha {
    let ahead = (git rev-list --count origin/main..HEAD | complete | get stdout | str trim)
    let behind = (git rev-list --count HEAD..origin/main | complete | get stdout | str trim)
    if $ahead != "0" {
      print $"(ansi red)main locale en avance de ($ahead) commit\(s\) sur origin/main.(ansi reset)"
      print "main est protégée : ce contenu doit arriver par une pull request."
      print $"  gh pr create --base main --head develop --fill"
      print $"  gh pr merge --merge --delete-branch=false"
      print "  git checkout main; git pull"
    }
    if $behind != "0" {
      print $"(ansi red)main locale en retard de ($behind) commit\(s\).(ansi reset) Fais `git pull` d'abord."
    }
    error make { msg: "main locale et origin/main divergent — on ne tague pas dans le vide" }
  }

  # 4. le tag ne doit exister ni en local ni sur le distant
  let existing = (git tag -l $tag | complete | get stdout | str trim)
  if $existing != "" {
    error make { msg: $"le tag ($tag) existe déjà en local" }
  }
  let remote_tag = (git ls-remote --tags origin $tag | complete | get stdout | str trim)
  if $remote_tag != "" {
    error make { msg: $"le tag ($tag) existe déjà sur ($GH_REPO)" }
  }

  if $dry_run {
    print $"(ansi yellow)[dry-run](ansi reset) taguerait ($tag) sur ($local_sha | str substring 0..7) et pousserait le tag vers ($GH_REPO)"
    print "la CI publierait alors : crates.io, binaires, Docker, WASM→playground, Marketplace"
    return
  }

  git tag -a $tag -m $"release ($tag)"
  git push origin $tag
  print $"(char nl)(ansi green)($tag) poussé.(ansi reset) La CI prend le relais :"
  print $"  https://github.com/($GH_REPO)/actions"
}

# `ship` enchaîne tout ce qu'une release demande, de la version au tag.
#
# Il n'y a qu'un seul arrêt : une confirmation, une fois la CI verte, juste
# avant la fusion et le tag. C'est le dernier moment réversible — après, le
# numéro est publié sur crates.io et ne peut plus être repris, seulement yanké.
#
# En cas d'échec ou de refus, rien n'est perdu : la PR reste ouverte et
# `ship --resume` reprend là où on s'est arrêté.
def "main ship" [
  level: string = "patch"   # patch | minor | major
  --yes                     # ne pas demander confirmation (usage non interactif)
  --resume                  # reprendre après une PR déjà ouverte
] {
  # --- 0. préconditions -------------------------------------------------------
  if (which gh | is-empty) {
    error make { msg: "gh introuvable — nécessaire pour ouvrir et fusionner la PR" }
  }
  if (gh auth status | complete | get exit_code) != 0 {
    error make { msg: "gh n'est pas authentifié — lance `gh auth login`" }
  }

  let branch = (git rev-parse --abbrev-ref HEAD | complete | get stdout | str trim)
  if $branch != "develop" {
    error make { msg: $"ship part de develop, pas de ($branch)" }
  }

  git fetch --quiet origin

  if not $resume {
    let dirty = (git status --porcelain | complete | get stdout | str trim)
    if $dirty != "" {
      print $"(ansi red)L'arbre de travail n'est pas propre :(ansi reset)"
      print $dirty
      error make { msg: "commite ou remise tes changements avant de publier" }
    }

    # develop doit être à jour, sinon on pousserait par-dessus le travail d'un autre
    let local = (git rev-parse HEAD | complete | get stdout | str trim)
    let remote = (git rev-parse origin/develop | complete | get stdout | str trim)
    if $local != $remote {
      error make { msg: "develop locale et origin/develop divergent — `git pull` d'abord" }
    }
  }

  # --- 1. préparer la version -------------------------------------------------
  mut pr = ""

  if $resume {
    let open_pr = (gh pr list --base main --head develop --state open --json number
                   | complete | get stdout | from json)
    if ($open_pr | is-empty) {
      error make { msg: "--resume mais aucune PR develop → main ouverte" }
    }
    $pr = ($open_pr | first | get number | into string)
    print $"reprise sur la PR #($pr)"
  } else {
    # develop doit contenir main, sinon la PR embarquerait une régression
    let merged = (git merge --no-edit origin/main | complete)
    if $merged.exit_code != 0 {
      print $merged.stdout
      error make { msg: "conflit en fusionnant main dans develop — résous-le puis relance" }
    }

    let before = (current-version)
    main bump $level
    let v = (current-version)

    print $"(char nl)vérification de la compilation…"
    let chk = (cargo check --workspace | complete)
    if $chk.exit_code != 0 {
      print $chk.stderr
      # on remet la version d'avant : une version qui ne compile pas ne doit
      # pas rester dans l'arbre de travail
      write-version $before
      error make { msg: $"cargo check a échoué — version restaurée à ($before)" }
    }

    git add -A
    git commit --quiet -m $"release v($v)"
    git push --quiet origin develop
    print $"(ansi green)✓(ansi reset) ($before) → ($v), commité et poussé"

    let url = (gh pr create --base main --head develop
                 --title $"release v($v)"
                 --body $"Bump de version : ($before) → ($v).(char nl)(char nl)Aucun changement de code — ce commit n'existe que pour porter le numéro de version jusqu'à `main`, d'où le tag sera posé."
               | complete | get stdout | str trim)
    $pr = ($url | split row "/" | last)
    print $"(ansi green)✓(ansi reset) PR #($pr) ouverte — ($url)"
  }

  # --- 2. attendre la CI ------------------------------------------------------
  # GitHub met quelques secondes à enregistrer les checks d'une PR neuve ;
  # sans ce délai `gh pr checks` sort sur « no checks reported ».
  if not $resume { sleep 15sec }
  print $"(char nl)attente des checks obligatoires…"
  let checks = (gh pr checks $pr --watch --fail-fast | complete)
  if $checks.exit_code != 0 {
    print $checks.stdout
    print $"(ansi red)La CI a échoué.(ansi reset) La PR #($pr) reste ouverte."
    print $"Corrige, pousse sur develop, puis : nu publish.nu ship --resume"
    error make { msg: "checks en échec — rien n'a été fusionné ni tagué" }
  }
  print $"(ansi green)✓(ansi reset) tous les checks sont verts"

  # --- 3. le seul point d'arrêt ----------------------------------------------
  let v = (current-version)
  if not $yes {
    print $"(char nl)Prêt à fusionner la PR #($pr) et à poser (ansi cyan)v($v)(ansi reset)."
    print "Le tag déclenche la publication sur crates.io, où un numéro ne peut plus être repris."
    let answer = (input "Continuer ? [o/N] ")
    if ($answer | str lowercase | str trim) not-in ["o" "oui" "y" "yes"] {
      print $"(char nl)Abandonné. La PR #($pr) reste ouverte ; reprends avec :"
      print "  nu publish.nu ship --resume"
      return
    }
  }

  # --- 4. fusionner, taguer, resynchroniser -----------------------------------
  gh pr merge $pr --merge --admin --delete-branch=false
  print $"(ansi green)✓(ansi reset) PR #($pr) fusionnée"

  git switch --quiet main
  git pull --quiet --ff-only origin main

  # release refait ses quatre gardes : arbre propre, branche main, main alignée
  # sur origin/main, tag inexistant. C'est volontairement redondant — c'est la
  # dernière barrière avant l'irréversible.
  main release

  git switch --quiet develop
  git merge --quiet --no-edit origin/main
  git push --quiet origin develop
  print $"(ansi green)✓(ansi reset) develop resynchronisé sur main"
}

def main [] {
  print "nu publish.nu <version|bump|sync|check|release|ship> — voir l'en-tête du fichier"
}
