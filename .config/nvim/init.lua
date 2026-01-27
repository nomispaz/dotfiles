vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.keymap.set({"n","v"}, "<Space>", "<Nop>", { silent = true })

local opt = vim.opt

vim.g.autoformat = true

opt.clipboard = "unnamedplus" -- Sync with system clipboard
opt.confirm = true -- Confirm to save changes before exiting modified buffer
opt.cursorline = false -- highlighting of the current line
--opt.expandtab = true -- Use spaces instead of tabs
opt.ignorecase = true -- Ignore case
opt.inccommand = "nosplit" -- preview incremental substitute
opt.laststatus = 3 -- global statusline
opt.mouse = "a" -- Enable mouse mode
opt.number = true -- Print line number
vim.wo.relativenumber = true
opt.scrolloff = 4 -- Lines of context
opt.shiftround = true -- Round indent
--opt.shiftwidth = 4 -- Size of an indent
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
opt.completeopt = {'menu', 'menuone', 'noselect'} --autocomplete selection

-- =========================
-- Folding (Treesitter)
-- =========================
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldtext = "v:folddashes.substitute(getline(v:foldstart),'/\\\\*\\\\|\\\\*/\\\\|{{{\\\\d\\\\=','','g')"
vim.opt.foldenable = true
vim.opt.foldlevel = 99       -- open all folds by default
vim.opt.foldlevelstart = 99

require("config.autocommands")
require("config.functions")
require("config.keymaps")

require("config.lazy")

--vim.cmd.colorscheme "catppuccin-mocha"
vim.cmd.colorscheme "tokyonight-night"

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

