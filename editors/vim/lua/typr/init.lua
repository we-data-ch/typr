local M = {}

--- Plugin entry point.
--- * Neovim: starts the typR language server automatically on .ty files
---   when the `typr` binary is available.
--- * Vim (non-Lua): no-op; see doc/typr.txt for the one-line client setup.
function M.setup(opts)
  opts = opts or {}
  -- Opt-out: set vim.g.typr_lsp_enabled = false before this plugin loads
  -- (e.g. in lazy.nvim's `init`) to keep only syntax/indent/ft support.
  if vim.g.typr_lsp_enabled == false then
    return
  end
  require("typr.lsp").setup(opts.lsp or {})
end

return M