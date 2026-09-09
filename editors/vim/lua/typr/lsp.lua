local M = {}

local default_capabilities = function()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  return capabilities
end

--- Resolve the typr binary path.
--- Order of precedence:
---   1. vim.g.typr_path (global, e.g. set in init.lua)
---   2. vim.b.typr_path (buffer-local)
---   3. "typr" resolved from PATH
--- @return string|nil
function M.resolve_binary()
  if vim.g.typr_path and vim.g.typr_path ~= "" then
    return vim.g.typr_path
  end
  if vim.b.typr_path and vim.b.typr_path ~= "" then
    return vim.b.typr_path
  end
  if vim.fn.executable("typr") == 1 then
    return "typr"
  end
  return nil
end

--- Check whether the typr binary exposes the `lsp` subcommand.
--- The LSP entry point is `typr lsp` (same as the VS Code extension).
--- Result is cached per binary path on first call.
--- @param binary string
--- @return boolean
local lsp_checked = {}

function M.has_lsp(binary)
  if lsp_checked[binary] ~= nil then
    return lsp_checked[binary]
  end
  local ok, out = pcall(vim.fn.system, { binary, "lsp", "--help" })
  local available = ok and vim.v.shell_error == 0
  lsp_checked[binary] = available
  return available
end

--- Start the typR language server for the current buffer.
--- Skips silently when the binary is absent or lacks the `lsp` subcommand.
--- @param opts table|nil Extra opts passed to vim.lsp.start().
function M.attach(opts)
  opts = opts or {}

  local binary = M.resolve_binary()
  if not binary then
    return nil
  end
  if not M.has_lsp(binary) then
    return nil
  end

  local config = vim.tbl_deep_extend("force", {
    name = "typr_language_server",
    cmd = { binary, "lsp" },
    filetypes = { "typr" },
    root_dir = vim.fs.root(0, { "typr.toml", ".git" }) or vim.fs.dirname(vim.api.nvim_buf_get_name(0)),
    capabilities = default_capabilities(),
    settings = {},
  }, opts)

  return vim.lsp.start(config)
end

--- Lazy wrapper: registers a filetype autocommand that starts the LSP on
--- first open of a typr buffer. Guarded so it only runs once.
function M.setup(opts)
  opts = opts or {}

  if vim.g.typr_lsp_setup_done then
    return
  end
  vim.g.typr_lsp_setup_done = true

  vim.api.nvim_create_autocmd("FileType", {
    pattern = "typr",
    group = vim.api.nvim_create_augroup("typr_lsp", { clear = true }),
    callback = function()
      M.attach(opts)
    end,
  })

  -- Attach immediately if the current buffer is already typr
  if vim.bo.filetype == "typr" then
    M.attach(opts)
  end
end

return M
