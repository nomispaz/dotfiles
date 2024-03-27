local wk = require("which-key")

local setup = {
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
        ["b"] = {
            name = "Babel",
            ["t"] = { "obt", "Export code blocks"},
        },
        ["l"] = { 
            name = "Links",
            ["s"] = { "ols", "Store link ID"},
            ["i"] = { "oli", "Insert stored link ID"},
        },
        ["o"] = { "oo", "Jump to link destination"},
    },
}

wk.setup(setup)
wk.register(mappings, { prefix = "<leader>" }, opts)
