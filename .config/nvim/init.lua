-- ===================================================
-- general settings   
-- ===================================================

vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.keymap.set({"n","v"}, "<Space>", "<Nop>", { silent = true })

local opt = vim.opt

vim.g.autoformat = true

opt.clipboard = "unnamedplus" -- Sync with system clipboard
opt.confirm = true -- Confirm to save changes before exiting modified buffer
opt.cursorline = false -- highlighting of the current line
opt.expandtab = true -- Use spaces instead of tabs
opt.ignorecase = true -- case-insensitive search
opt.inccommand = "nosplit" -- preview incremental substitute
opt.laststatus = 3 -- global statusline
opt.mouse = "a" -- Enable mouse mode
opt.number = true -- Print line number
vim.wo.relativenumber = true
opt.scrolloff = 4 -- Lines of context
opt.shiftround = true -- Round indent
opt.shiftwidth = 2 -- Size of an indent
opt.softtabstop = 2
opt.showmode = false -- Dont show mode since we have a statusline
opt.smartcase = true -- Don't ignore case with capitals
opt.smartindent = true -- Insert indents automatically
opt.spelllang = { "en" }
opt.splitbelow = true -- Put new windows below current
opt.splitkeep = "screen"
opt.splitright = true -- Put new windows right of current
opt.timeoutlen = 300
opt.undofile = true
opt.undolevels = 10000
opt.wildmode = "longest:full,full" -- Command-line completion mode
opt.wrap = true -- line wrap
opt.conceallevel = 2 --conceal links
opt.concealcursor = 'nc'
opt.completeopt = {'menu', 'menuone', 'noselect', 'popup', 'fuzzy'} --autocomplete selection

-- activate tree view in file-browser (Ex, Sex, Vex)
vim.g.netrw_liststyle = 3

-- =========================
-- Activate treesitter without nvim-treesitter
-- =========================

--vim.cmd("syntax off")
--
--
--vim.api.nvim_create_autocmd("FileType", {
--  callback = function(ev)
--    local buf = ev.buf
--    local lang = vim.treesitter.language.get_lang(vim.bo[buf].filetype)
--    if not lang then return end
--
--    pcall(vim.treesitter.start, buf, lang)
--  end,
--})



-- =========================
-- Folding (Treesitter)
-- =========================

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldtext = "v:folddashes.substitute(getline(v:foldstart),'/\\\\*\\\\|\\\\*/\\\\|{{{\\\\d\\\\=','','g')"
vim.opt.foldenable = true
vim.opt.foldlevel = 99       -- open all folds by default
vim.opt.foldlevelstart = 99

-- Toggle fold under cursor with Shift+Tab
vim.keymap.set('n', '<S-Tab>', 'za', { noremap = true, silent = true })

vim.keymap.set('n', '<leader>c', function()
  local any_closed = false
  local line_count = vim.api.nvim_buf_line_count(0)

  for lnum = 1, line_count do
    if vim.fn.foldclosed(lnum) ~= -1 then
      any_closed = true
      break
    end
  end

  if any_closed then
    vim.cmd('normal! zR') -- open all folds
  else
    vim.cmd('normal! zM') -- close all folds
  end
end, { noremap = true, silent = true })

-- ===================================================
-- statusline   
-- ===================================================

function _G.statusline()
  local mode_map = {
    n = "NORMAL",
    i = "INSERT",
    v = "VISUAL",
    V = "V-LINE",
    [""] = "V-BLOCK",
    c = "COMMAND",
    R = "REPLACE",
    t = "TERMINAL",
  }

  local mode = mode_map[vim.fn.mode()] or vim.fn.mode()
  local filename = vim.fn.expand("%:t")
  if filename == "" then filename = "[No Name]" end

  local line = vim.fn.line(".")
  local col = vim.fn.col(".")

 -- LSP client name
  local lsp_name = "No LSP"
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients > 0 then
    lsp_name = clients[1].name
  end

   -- LSP diagnostics
  local diagnostics = vim.diagnostic.get(0)
  local errors, warnings = 0, 0

  for _, d in ipairs(diagnostics) do
    if d.severity == vim.diagnostic.severity.ERROR then
      errors = errors + 1
    elseif d.severity == vim.diagnostic.severity.WARN then
      warnings = warnings + 1
    end
  end

  -- only show diagnostics if an LSP is attached
  if #vim.lsp.get_clients({ bufnr = 0 }) > 0 then
    diag_str = string.format(" E:%d W:%d ", errors, warnings)
  else
    diag_str = ""
  end


  return string.format(
    " %s | %s | %s %%=| LSP: %s | %d:%d | %%p%%%% ",
    mode,
    filename,
    diag_str,
    lsp_name,
    line,
    col
  )
end

vim.o.statusline = "%!v:lua.statusline()"

-- ===================================================
-- Import of configs and plugins
-- ===================================================

require("config.autocommands")
require("config.functions")
require("config.keymaps")

require("config.lazy")

--vim.cmd.colorscheme "catppuccin-mocha"
vim.cmd.colorscheme "tokyonight-night"

-- ===================================================
-- LSP
-- ===================================================

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.lsp.config('gopls', {
    cmd = { "gopls" },
    root_markers = { "go.mod", ".git" },
    filetypes = { "go" },
    capabilities = capabilities,
})

vim.lsp.config('marksman', {
    cmd = { 'marksman', 'server' },
    root_markers = { ".marksman.toml", ".git" },
    filetypes = { "markdown", "markdown.mdx" },
})

vim.lsp.enable('marksman')
vim.lsp.enable('gopls')
--vim.lsp.enable('pylsp')

-- ===================================================
-- Completion
-- ===================================================

---- attach LSP to completion function
--vim.api.nvim_create_autocmd("LspAttach", {
--    callback = function(ev)
--        -- LSP completion
--        vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
--
--        -- Enable built-in LSP completion engine
--        vim.lsp.completion.enable(true, ev.data.client_id, ev.buf, {
--            autotrigger = true,
--        })
--    end,
--})
--
--vim.opt.complete = {
--  "f",   -- file paths  
--  ".",   -- buffer words
--  "k",   -- dictionary (optional, safe)
--    }
--
---- Expand or jump forward
--local ls = require("luasnip")
--
--vim.keymap.set({ "i", "s" }, "<Tab>", function()
--  if require("luasnip").jumpable(1) then
--    -- usage of plug is necessary since otherwise no jumping within the snippet with expr = true. Without expr = true, no tab outside
--    return "<Plug>luasnip-jump-next"
--  else
--    return "<Tab>"
--  end
--end, { expr = true, silent = true })
--
--vim.keymap.set({"i"}, "<C-K>", function() ls.expand() end, {silent = true})
