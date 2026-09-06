#!/bin/bash
# Build WASM local + copie vers le playground voisin, pour le développement.
# En release c'est le job `wasm` de .github/workflows/release.yml qui fait foi.
set -e

VERSION=$(sed -n '/^\[workspace\.package\]/,/^\[/p' Cargo.toml | grep '^version' | head -1 | cut -d'"' -f2)
PLAYGROUND=../../typr-playground.github.io/public/wasm

wasm-pack build --release --target web crates/typr-wasm

mkdir -p "$PLAYGROUND"
cp crates/typr-wasm/pkg/*.wasm crates/typr-wasm/pkg/*.js "$PLAYGROUND/"
printf '{"version":"%s"}\n' "$VERSION" > "$PLAYGROUND/version.json"

echo "WASM $VERSION construit et copié vers $PLAYGROUND"
