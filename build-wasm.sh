#!/bin/bash
set -e

wasm-pack build --release --target web crates/typr-wasm

cp crates/typr-wasm/pkg/*.wasm playground/public/wasm/
cp crates/typr-wasm/pkg/*.js playground/public/wasm/

echo "WASM built and copied to playground/public/wasm/"
