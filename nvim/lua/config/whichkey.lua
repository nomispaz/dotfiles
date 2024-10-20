local wk = require("which-key")

local setup = {
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

wk.add({
	
    { "<leader>,", "<cmd>WhichKey<CR>", desc = "WhichKey" },
    { "<leader>b", group = "Tabs" },
    { "<leader>b<Left>", "<cmd>:BufferLineCyclePrev<cr>", desc = "Switch to left buffer" },
    { "<leader>b<Right>", "<cmd>:BufferLineCycleNext<cr>", desc = "Switch to right buffer" },
    { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Switch to buffer" },
    { "<leader>bk", "<cmd>bd<cr>", desc = "Close buffer" },
    { "<leader>f", group = "Open files" },
    { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File" },
    { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Search files (grep)" },
    { "<leader>fn", "<cmd>ene <BAR> startinsert<cr>", desc = "New File" },
    { "<leader>fp", "<cmd>Telescope projects<cr>", desc = "Project list" },
    { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
    { "<leader>o", group = "Org-mode" },
    { "<leader>oa", "oa", desc = "Org-Agenda" },
    { "<leader>ob", group = "Babel" },
    { "<leader>obt", "obt", desc = "Export code blocks" },
    { "<leader>oi", group = "Insert" },
    { "<leader>ois", "ois", desc = "Insert scheduled date" },
    { "<leader>oit", "oit", desc = "Insert todo item" },
    { "<leader>ol", group = "Links" },
    { "<leader>oli", "oli", desc = "Insert stored link ID" },
    { "<leader>ols", "ols", desc = "Store link ID" },
    { "<leader>oo", "oo", desc = "Jump to link destination" },
    { "<leader>ot", "ot", desc = "Add tag" },
    { "<leader>p", "<cmd>Lazy<CR>", desc = "Plugin Manager" },
    { "<leader>s", group = "Session management" },
    { "<leader>sd", '<cmd>lua require("persistence").stop()<cr>', desc = "Don't save session on exit" },
    { "<leader>sl", '<cmd>lua require("persistence").load({ last = true })<cr>', desc = "Restore last session" },
    { "<leader>ss", '<cmd>lua require("persistence").load()<cr>', desc = "Restore Session for current directory" },
    { "<leader>t", group = "Telescope search functions" },
    { "<leader>td", '<cmd>lua require("telescope.builtin").diagnostics({wrap_results=true, line_width="full"})<cr>', desc = "LSP Errors/Warnings" },
    { "<leader>tf", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Fuzzy find in buffer" },
    { "<leader>tr", "<cmd>Telescope lsp_references<cr>", desc = "Show references" },
    { "<leader>tk", "<cmd>Telescope keymaps<cr>", desc = "Display keymaps" },
    { "<leader>g", group = "LSP functions" },
    { "<leader>gd", "<cmd>lua vim.lsp.buf.definition()<cr>", desc = "Go to function definition", nowait = true, remap = false },
    { "<leader>gD", "<cmd>lua vim.lsp.buf.references()<cr>", desc = "Show function references", nowait = true, remap = false },
    { "<leader>grn", "<cmd>lua vim.lsp.buf.rename()<cr>", desc = "Rename all references in buffer", nowait = true, remap = false },
    { "<C-s>", "<cmd>lua vim.lsp.buf.signature_help()<cr>", desc = "Show variable signatures", nowait = true, remap = false , mode = "i"},
    { "<C-d>", "<cmd>lua vim.diagnostic.setloclist()<cr>", desc = "Open buffer in split window with diagnostics", nowait = true, remap = false},
    { "<A-k>", "<cmd>m .-2<cr>", desc = "Move row one row up", nowait = true, remap = false },
    { "<A-j>", "<cmd>m .+1<cr>", desc = "Move row one row down", nowait = true, remap = false },
    { "<leader>db", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", desc = "Dap add breakpoint", nowait = true, remap = false },
    { "<leader>dr", "<cmd>lua require'dap'.continue()<cr>", desc = "Dap resume", nowait = true, remap = false },
    { "<leader>dis", "<cmd>lua vim.diagnostic.config({ virtual_text = true })<cr>", desc = "Enable inline diagnostics", nowait = true, remap = false },
    { "<leader>did", "<cmd>lua vim.diagnostic.config({ virtual_text = false })<cr>", desc = "Disable inline diagnostics", nowait = true, remap = false },




})

wk.setup(setup)
