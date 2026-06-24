# TypR CLI

Command-line interface, REPL, and LSP server for TypR - a typed superset of R.

This crate provides the CLI layer for TypR, depending on `typr-core` for the core logic. It includes:

- **Command-line interface** with project management commands
- **Interactive REPL** with syntax highlighting
- **Language Server Protocol (LSP) server** for IDE integration
- **Filesystem-based source and output handlers**

## Usage

```bash
# Create a new project
typr new myproject

# Check types
typr check

# Build to R
typr build

# Run
typr run

# Start REPL
typr repl

# Start LSP server
typr lsp
```

## Architecture

This crate follows the same design philosophy as `typr-wasm`:
- **Minimal wrapper** - Only CLI-specific code, no business logic
- **Uses typr-core abstractions** - Implements `SourceProvider` for filesystem
- **Clear dependency separation** - CLI deps (clap, tokio, tower-lsp-server) stay here

## Modules

- `cli` - Main CLI entry point with clap
- `repl` - Interactive REPL with rustyline
- `lsp` - LSP server with tower-lsp-server
- `project` - Project management commands
- `fs_provider` - Filesystem implementations of core traits
- `engine` - Build/compile utilities
- `io` - R execution utilities
- `metaprogramming` - Module expansion

## Editor Integration

`typr lsp` starts a standard LSP server over **stdio** (no socket, no extra flags). Any LSP
client can drive it by spawning `typr lsp` as the server command for files with the `.ty`
extension. Capabilities currently implemented (see `crates/typr-cli/src/lsp.rs`):
`hover`, `completion` (trigger chars `.` `$` `>` `:`), `workspace/symbol`, `documentSymbol`,
`definition`, `signatureHelp` (trigger chars `(` `,`), `rename` (+ `prepareRename`), `references`,
and diagnostics (published on open/change, debounced).

### Neovim (native `vim.lsp`, 0.10+)

No plugin required. In an `ftplugin/ty.lua` (or a `FileType` autocmd) for files matching `*.ty`:

```lua
vim.lsp.start({
  name = "typr",
  cmd = { "typr", "lsp" },
  root_dir = vim.fs.root(0, { "DESCRIPTION", ".git" }) or vim.fn.getcwd(),
})
```

Make sure the file type is detected as `.ty` first (e.g. `vim.filetype.add({ extension = { ty = "ty" } })`
in your config) and that the `typr` binary built from this repo (`cargo build --release`, then
`target/release/typr`) is on `$PATH`.

If you use `nvim-lspconfig`, the equivalent is a custom server entry (there's no upstream
`lspconfig.typr` config) — `vim.lsp.config.typr = { cmd = { "typr", "lsp" }, filetypes = { "ty" } }`
plus `vim.lsp.enable("typr")` (nvim 0.11+ config style), or `require('lspconfig.configs').typr =
{ default_config = { cmd = { "typr", "lsp" }, filetypes = { "ty" }, root_dir = ... } }` on older
versions.

### VSCode

There is no published TypR extension. Any generic "LSP client" extension that lets you configure
an arbitrary `command`/`args` and a language id can attach to `typr lsp` the same way — point it
at the `typr` binary with args `["lsp"]`, associate it with `.ty` files, and it gets the same
capabilities listed above. A dedicated extension (syntax highlighting + bundled `vscode-languageclient`
wrapper around `typr lsp`) is tracked as future work, not shipped in this repo.

### Any other client

The general recipe is the same everywhere: spawn `typr lsp`, speak LSP over its stdin/stdout, and
associate it with `.ty` files. No special initialization options are required.

## License

Apache-2.0
