# publish.nu — Publish a new version of typr
#
# What this script does locally:
#   1. Bump version in Cargo.toml and configs/src/data.toml
#   2. Publish the crate to crates.io
#   3. Build & push the Docker image
#   4. Commit, tag and push — the tag triggers GitHub Actions which:
#        - Builds binaries for all platforms (Linux, Windows, macOS)
#        - Builds the WASM package
#        - Creates the GitHub Release with all artifacts

# --- 1. Bump version ---
let data_file = "configs/src/data.toml"
let new_version = (open $data_file | get release_version | $in + 1)
let version_string = $"0.4.($new_version)"
let updated_line = $"version = \"($version_string)\""
# Update version in [workspace.package] section (matches the line: version = "x.y.z")
sed -i $"/^\\[workspace\\.package\\]/,/^\\[/ s/^version = .*/version = \"($version_string)\"/" Cargo.toml

# Save new version number
open $data_file | update release_version { $in + 1 } | to toml | save -f $data_file

# --- 2. Publish to crates.io ---
git add .
git commit -m $"bump version to ($version_string)"
cargo publish --allow-dirty

# --- 3. Docker ---
cd ../docker
nu deploy.nu
cd ../app

# --- 4. Commit, tag and push to trigger GitHub Actions release ---
git add .
git commit -m $"release v($version_string)" --allow-empty

let tag_name = $"v($version_string)"
git tag $tag_name
git push origin main
git push origin $tag_name

print $"Published v($version_string) — GitHub Actions will build the release artifacts."
