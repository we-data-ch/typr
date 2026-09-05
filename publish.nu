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
#   nu publish.nu release [--dry-run]      # commit + tag + push → la CI prend le relais

const CARGO_FILE = "Cargo.toml"
const VSCODE_PKG = "editors/vscode/package.json"
const RSTUDIO_DESC = "editors/rstudio/DESCRIPTION"
const GH_REPO = "we-data-ch/typr"
const DOCKER_REPO = "fabricehategekimana/typr"
# Ordre de publication imposé par le graphe de dépendances :
# typr-core → typr-lsp → typr-cli → typr
const CRATES = ["typr-core", "typr-lsp", "typr-cli", "typr"]

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

  # 3. le tag ne doit pas déjà exister
  let existing = (git tag -l $tag | complete | get stdout | str trim)
  if $existing != "" {
    error make { msg: $"le tag ($tag) existe déjà" }
  }

  if $dry_run {
    print $"(ansi yellow)[dry-run](ansi reset) taguerait ($tag) sur ($branch) et pousserait vers ($GH_REPO)"
    print "la CI publierait alors : crates.io, binaires, Docker, WASM→playground, Marketplace"
    return
  }

  git tag -a $tag -m $"release ($tag)"
  git push origin main
  git push origin $tag
  print $"(char nl)(ansi green)($tag) poussé.(ansi reset) La CI prend le relais :"
  print $"  https://github.com/($GH_REPO)/actions"
}

def main [] {
  print "nu publish.nu <version|bump|sync|check|release> — voir l'en-tête du fichier"
}
