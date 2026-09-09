# TypR for Vim / Neovim

Editor support for [TypR](https://github.com/we-data-ch/typr) — a typed
superset of R with Rust-inspired syntax.

Works in **both Vim and Neovim** from a single plugin, and shares the same
**language server binary** (`typr lsp`) used by every other editor.

## Features

- Syntax highlighting for `.ty` files
- Filetype detection (`*.ty`, legacy `*.typr` / `*.tyr`)
- Indentation
- Buffer options: `#` comments, `# region` / `# endregion` folding
- CLI integration: `:TyprCheck` / `:TyprBuild` / `:TyprRun` / `:TyprTest` /
  `:TyprRepl` + file variants, with dropdown completion of CLI flags
- Language server (hover, go-to-definition, autocompletion):
  - **Neovim**: automatic via the built-in LSP client
  - **Vim**: one-line setup with `coc.nvim` or `vim-lsp`

## Requirements

The `typr` binary must be installed and expose the LSP subcommand:

```bash
cargo install typr
typr lsp --help   # works if the server is available
```

## Installation

The plugin lives in `editors/vim/` inside the TypR monorepo and follows the
standard Vim plugin layout, so it works with every plugin manager.

**lazy.nvim** (Neovim):
```lua
{
  "we-data-ch/typr",
  dir = "editors/vim",
  ft = "typr",
  config = function()
    require("typr").setup()
  end,
}
```

**vim-plug**:
```vim
Plug 'we-data-ch/typr', { 'rtp': 'editors/vim', 'for': 'typr' }
" :PlugInstall
```

**packer.nvim** (Neovim):
```lua
use { "we-data-ch/typr", rtp = "editors/vim", ft = "typr" }
```

**Native packages** (Vim or Neovim):
```bash
ln -s "$(pwd)/editors/vim" ~/.local/share/nvim/site/pack/typr/start/typr
# or for Vim:
ln -s "$(pwd)/editors/vim" ~/.vim/pack/typr/start/typr
```

## Language server

### Neovim (automatic)

On Neovim the plugin starts the language server automatically for `.ty`
files when `typr` is available — no configuration needed. Check with:

```vim
:TyprLspStatus
```

Optionally override the binary path:

```lua
vim.g.typr_path = "/path/to/typr"
require("typr").setup()
```

To keep only syntax/indent support and skip the language server, set the
opt-out before the plugin loads (e.g. in lazy.nvim's `init`):

```lua
vim.g.typr_lsp_enabled = false
```

### Vim (one line)

Vim has no built-in LSP client. Add a snippet to your config:

**coc.nvim** (`:CocConfig`):
```json
{
  "languageserver": {
    "typr": {
      "command": "typr",
      "args": ["lsp"],
      "filetypes": ["typr"]
    }
  }
}
```

**vim-lsp**:
```vim
au User lsp_setup call lsp#register_server({
      \ 'name': 'typr',
      \ 'cmd': {server_info->['typr', 'lsp']},
      \ 'whitelist': ['typr'],
      \ })
autocmd FileType typr setlocal omnifunc=lsp#complete
```

## Commands

Run the `typr` CLI tooling from inside the editor. Output goes to a
terminal window (kept open so diagnostics stay readable); the bang form
runs synchronously inline instead.

| Command | Description |
|---------|-------------|
| `:TyprCheck [args]` | Type-check the project |
| `:TyprBuild [args]` | Build (transpile to R) the project |
| `:TyprRun [args]` | Transpile and run the project |
| `:TyprTest [args]` | Run the project test suite |
| `:TyprRepl [args]` | Start the interactive REPL |
| `:TyprCheckFile [args]` | Type-check the current file |
| `:TyprBuildFile [args]` | Build the current file |
| `:TyprRunFile [args]` | Transpile and run the current file |
| `:TyprLspStatus` | Report typr binary / LSP availability |

Examples:

```vim
:TyprCheck
:TyprBuildFile           " current .ty file, transpile only
:TyprBuildFile --strict  " extra CLI flags (tab-completed)
:TyprRun!
```

If the binary is not on PATH, set `g:typr_path` (in both editors).

## Documentation

`:help typr`

## License

Apache-2.0 (matching the TypR project).
