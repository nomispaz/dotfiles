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

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
-- bootstrap lazy.nvim
vim.fn.system({ 
    "git", 
    "clone", 
    "--filter=blob:none", 
    "https://github.com/folke/lazy.nvim.git", 
    "--branch=stable",
    lazypath,
})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
        spec = {
            { import = "plugins" },
        },
        defaults = {
            -- It's recommended to leave version=false for now, since a lot the plugin that support versioning,
            -- have outdated releases, which may break your Neovim install.
            version = false, -- always use the latest git commit
            -- version = "*", -- try installing the latest stable version for plugins that support semver
        }
})

vim.cmd.colorscheme "tokyonight-night"

require("mini.pairs").setup()
require("config.whichkey")
require("persistence").setup({opts})
require("config.orgmode-headlines")
require("config.autocommands")
require("null-ls").setup({
        sources = {
            require("null-ls").builtins.formatting.shfmt,
        },
})
