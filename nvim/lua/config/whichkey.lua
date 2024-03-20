local wk = require("which-key")

local setup = {
    plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        spelling = {
            enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
            suggestions = 20, -- how many suggestions should be shown in the list?
        },
-- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        presets = {
            operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
            motions = true, -- adds help for motions
            text_objects = true, -- help for text objects triggered after entering an operator
            windows = true, -- default bindings on <c-w>
            nav = true, -- misc bindings to work with windows
            z = true, -- bindings for folds, spelling and others prefixed with z
            g = true, -- bindings for prefixed with g
        },
    },

    icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "➜", -- symbol used between a key and it's label
        group = "+", -- symbol prepended to a group
    },
    popup_mappings = {
        scroll_down = "<c-d>", -- binding to scroll down inside the popup
        scroll_up = "<c-u>", -- binding to scroll up inside the popup
    },
    window = {
        border = "rounded", -- none, single, double, shadow
        position = "bottom", -- bottom, top
        margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
        padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
        winblend = 0,
    },
    layout = {
        height = { min = 4, max = 25 }, -- min and max height of the columns
        width = { min = 20, max = 50 }, -- min and max width of the columns
        spacing = 3, -- spacing between columns
        align = "left", -- align columns left, center or right
    },
    ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
    show_help = true, -- show help message on the command line when the popup is visible
    triggers = "auto", -- automatically setup triggers
    -- triggers = {"<leader>"} -- or specify a list manually
    triggers_blacklist = {
        -- list of mode / prefixes that should never be hooked by WhichKey
        -- this is mostly relevant for key maps that start with a native binding
        -- most people should not need to change this
        i = { "j", "k" },
        v = { "j", "k" },
    },
}

local opts = {
    mode = "n", -- NORMAL mode
    prefix = "",
    buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
    silent = true, -- use `silent` when creating keymaps
    noremap = true, -- use `noremap` when creating keymaps
    nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
    ["f"] = {
        name = "Open files", 
        f = { "<cmd>Telescope find_files<cr>", "Find File"},
        r = { "<cmd>Telescope oldfiles<cr>", "Recent Files"},
        n = { "<cmd>ene <BAR> startinsert<cr>", "New File"},
        g = { "<cmd>Telescope live_grep<cr>", "Search files (grep)"},
        p = { "<cmd>Telescope projects<cr>", "Project list"}
    },
    ["t"] = {
        name = "Telescope search functions",
        k = { "<cmd>Telescope keymaps<cr>", "Display keymaps"},
        f = { "<cmd>Telescope current_buffer_fuzzy_find<cr>", "Fuzzy find in buffer"},
        d = { "<cmd>Telescope diagnostics<cr>", "LSP Errors/Warnings"},
    },
    ["p"] = { "<cmd>Lazy<CR>", "Plugin Manager" }, 
--    ["m"] = { "<cmd>MarkdownPreviewToggle<cr>", "Markdown Preview"},
    ["e"] = { "<cmd>Neotree toggle<cr>", "Toggle Neotree"},

    [","] = { "<cmd>WhichKey<CR>", "WhichKey" },
    ["b"] = {
        name = "Tabs",
        ["b"] = { "<cmd>Telescope buffers<cr>", "Switch to buffer" },
        ["k"] = { "<cmd>bd<cr>", "Close  buffer" },
        ["<Left>"] = { "<cmd>:BufferLineCyclePrev<cr>", "Switch to left buffer" },
        ["<Right>"] = { "<cmd>:BufferLineCycleNext<cr>", "Switch to right buffer" },
    },
    ["s"] = {
        name = "Session management",
        ["s"] = { [[<cmd>lua require("persistence").load()<cr>]], "Restore Session for current directory" },
        ["l"] = { [[<cmd>lua require("persistence").load({ last = true })<cr>]], "Restore last session" },
        ["d"] = { [[<cmd>lua require("persistence").stop()<cr>]], "Don't save session on exit" },
    },
    ["o"] = {
        name = "Org-mode",
        ["a"] = { "oa", "Org-Agenda"},
        ["t"] = { "ot", "Add tag"},
        ["i"] = {
            name = "Insert",
            ["s"] = { "ois", "Insert scheduled date"},
            ["t"] = { "oit", "Insert todo item"},
        },
        ["b"] =  { "obt", "Export code blocks"},
    },
   
  -- ["c"] = { name = "+code" },
   --["gh"] = { name = "+hunks" },
    --["q"] = { name = "+quit/session" },
    --["s"] = { name = "+search" },
    --["u"] = { name = "+ui" },          

}

wk.setup(setup)
wk.register(mappings, { prefix = "<leader>" }, opts)
