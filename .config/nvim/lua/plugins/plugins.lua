return {

-- ===================================================
-- colorschemes
-- ===================================================
    {
      "folke/tokyonight.nvim",
      lazy = false,
      priority = 1000,
      opts = {},
    },
    --{
    --  "catppuccin/nvim",
    --    name = "catppuccin",
    --  lazy = false,
    --  priority = 1000,
    --  opts = {},
    --},

-- ===================================================
-- completion
-- ===================================================
     {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "onsails/lspkind.nvim",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()
      require "config.completion"
    end,
    },

-- ===================================================
-- luasnip
-- ===================================================
    {
        "L3MON4D3/LuaSnip", event = "VeryLazy",
        dependencies = {
        {
            "rafamadriz/friendly-snippets",
            config = function()
              require("luasnip.loaders.from_vscode").lazy_load()
            end,
          },
        },
    },

-- ===================================================
-- telescope
-- ===================================================
    {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" }
    },
    
-- ===================================================
-- treesitter
-- ===================================================
    {
        "nvim-treesitter/nvim-treesitter",
	lazy = false,
	ild = ':TSUpdate',
        config = function()
	    require'nvim-treesitter'.install { 'go', 'bash', 'markdown', 'rust'}
            require("nvim-treesitter").setup({
                -- Install parsers synchronously (only applied to `ensure_installed`)
                sync_install = false,

                -- Automatically install missing parsers when entering buffer
                -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
                auto_install = false,

                highlight = {
                    enable = true,
                    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
                    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
                    -- Using this option may slow down your editor, and you may see some duplicate highlights.
                    -- Instead of true it can also be a list of languages
                    additional_vim_regex_highlighting = false,
                  },

                  incremental_selection = {
                      enable = true,
                  }
            }
            )
        end,
    },
    -- Show context of the current function
    {
        "nvim-treesitter/nvim-treesitter-context",
        enabled = true,
        opts = {
            mode = "cursor",
            max_lines = 4,
            multiline_threshold = 2, -- Maximum number of lines to show for a single context
        },
    },

}
