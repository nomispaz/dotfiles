--load configs
require("config.icons")
require("config.options")
require("config.keymaps")

--load lazy.nvim plugin manager
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

--load colorscheme
vim.cmd.colorscheme "catppuccin-mocha"

require("bufferline").setup({opts})
require("persistence").setup({opts})
require("null-ls").setup({
        sources = {
            require("null-ls").builtins.formatting.shfmt,
        },
    })
require("mini.pairs").setup()
require("config.whichkey")
