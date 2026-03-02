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
- **Clear dependency separation** - CLI deps (clap, tokio, tower-lsp) stay here

## Modules

- `cli` - Main CLI entry point with clap
- `repl` - Interactive REPL with rustyline
- `lsp` - LSP server with tower-lsp
- `project` - Project management commands
- `fs_provider` - Filesystem implementations of core traits
- `engine` - Build/compile utilities
- `io` - R execution utilities
- `metaprogramming` - Module expansion

## License

Apache-2.0
