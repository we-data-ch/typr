# publish.nu — Publish a new version of typr
#
# Usage:
#   nu publish.nu                      # bump + all actions (crates.io + docker + github)
#   nu publish.nu crates-io            # bump + crates.io only
#   nu publish.nu docker               # bump + docker only
#   nu publish.nu github-actions       # bump + github only
#   nu publish.nu crates-io docker     # bump + selected actions
#
#   nu publish.nu version              # show current version
#   nu publish.nu version --down       # decrement patch
#   nu publish.nu version --set 0.5.3  # set exact version
#
#   nu publish.nu check                # check crates.io + GitHub + local
#   nu publish.nu check crates-io      # check crates.io only
#   nu publish.nu check github         # check GitHub only

const DATA_FILE = "configs/src/data.toml"
const CARGO_FILE = "Cargo.toml"
const GH_REPO = "we-data-ch/typr"
const CRATES = ["typr-core", "typr-cli", "typr"]

# --- helpers -----------------------------------------------------------------

def read-release-version []: nothing -> int {
  open $DATA_FILE | get release_version
}

def current-version []: nothing -> string {
  $"0.5.(read-release-version)"
}

def write-release-version [v: int] {
  {release_version: $v} | to toml | save -f $DATA_FILE
}

def patch-cargo-version [version_string: string] {
  sed -i $"/^\\[workspace\\.package\\]/,/^\\[/ s/^version = .*/version = \"($version_string)\"/" $CARGO_FILE
}

def validate-version [v: string] {
  let parts = ($v | split row ".")
  if ($parts | length) != 3 or $parts.0 != "0" or $parts.1 != "5" {
    error make { msg: $"invalid version format: ($v). expected 0.5.X" }
  }
}

# --- version subcommand ------------------------------------------------------

def "main version" [
  --up(-u)
  --down(-d)        # decrement
  --set: string     # set exact version like "0.5.3"
] {
  if $set != null {
    validate-version $set
    let new_patch = ($set | split row "." | last | into int)
    patch-cargo-version $set
    write-release-version $new_patch
    print $"version set to ($set)"
    return
  }

  if $down {
    let new_patch = (read-release-version) - 1
    let new_ver = $"0.5.($new_patch)"
    patch-cargo-version $new_ver
    write-release-version $new_patch
    print $"version decremented to ($new_ver)"
    return
  }

  if $up {
    let new_patch = (read-release-version) + 1
    let new_ver = $"0.5.($new_patch)"
    patch-cargo-version $new_ver
    write-release-version $new_patch
    print $"version incremented to ($new_ver)"
    return
  }

  # no flags → just display
  print $"current version: (current-version)"
}

# --- check subcommand --------------------------------------------------------

def "main check" [
  area?: string  # "crates-io", "github", or empty for both
] {
  let targets = if $area == null { ["crates-io", "github"] } else { [$area] }

  if "crates-io" in $targets {
    print $"(char nl)=== crates.io ==="
    for name in $CRATES {
      let url = $"https://crates.io/api/v1/crates/($name)"
      let resp = (try { http get --headers [User-Agent typr-cli] $url } catch { null })
      if $resp == null {
        print $"  ($name): (ansi red)unreachable(ansi reset)"
      } else {
        let latest = $resp.crate.max_version
        print $"  ($name): (ansi green)($latest)(ansi reset)"
      }
    }
  }

  if "github" in $targets {
    print $"(char nl)=== GitHub Releases ==="
    let url = $"https://api.github.com/repos/($GH_REPO)/releases/latest"
    let resp = (try { http get $url } catch { null })
    if $resp == null {
      print $"  (ansi red)unreachable(ansi reset)"
    } else {
      print $"  latest release: (ansi green)($resp.tag_name)(ansi reset)"
      print $"  published:      ($resp.published_at)"
      print $"  url:            ($resp.html_url)"
    }
  }

  print $"(char nl)local version: (ansi cyan)(current-version)(ansi reset)"
}

# --- publish actions (used by main) ------------------------------------------

def bump-and-return-version []: nothing -> string {
  let new_patch = (read-release-version) + 1
  let version_string = $"0.5.($new_patch)"
  patch-cargo-version $version_string
  write-release-version $new_patch
  print $"(char nl)=== Bumped version to ($version_string) ==="
  $version_string
}

def publish-crates-io [version_string: string] {
  print $"(char nl)=== Publishing to crates.io ==="
  git add .
  git commit -m $"bump version to ($version_string)"

  cargo publish --allow-dirty -p typr-core
  sleep 30sec
  cargo publish --allow-dirty -p typr-cli
  sleep 30sec
  cargo publish --allow-dirty -p typr
}

def publish-docker [] {
  print $"(char nl)=== Building & pushing Docker image ==="
  let original_dir = $env.PWD
  cd ../docker
  nu deploy.nu
  cd $original_dir
}

def publish-github-actions [version_string: string] {
  print $"(char nl)=== Committing, tagging & pushing for GitHub Actions ==="
  cargo fmt --all
  git add .
  git commit -m $"release v($version_string)" --allow-empty

  let tag_name = $"v($version_string)"
  git tag $tag_name
  git push -f origin main
  git push -f origin $tag_name
}

# --- main (no subcommand = do all or selected actions) ------------------------

def main [...actions: string] {
  let version_string = (bump-and-return-version)

  let selected = if ($actions | length) == 0 {
    ["crates-io", "docker", "github-actions"]
  } else {
    $actions
  }

  for action in $selected {
    match $action {
      "crates-io"       => { publish-crates-io $version_string }
      "docker"          => { publish-docker }
      "github-actions"  => { publish-github-actions $version_string }
      _                 => { print $"Unknown action: ($action) — skipping" }
    }
  }

  print $"(char nl)Done."
}
