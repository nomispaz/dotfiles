local keymap = vim.api.nvim_set_keymap

--work with tabs (nvim buffers)
keymap("n", "<C-tab>", "<cmd>Telescope buffers<cr>", { desc = "Switch Tab" })
keymap("n", "<C-w>", "<cmd>bd<cr>", { desc = "Close Tab" })

--session management
-- restore the session for the current directory
keymap("n", "<leader>ls", [[<cmd>lua require("persistence").load()<cr>]], {desc = "Restore Session for current directory"} )
-- restore the last session
keymap("n", "<leader>ll", [[<cmd>lua require("persistence").load({ last = true })<cr>]], { desc = "Restore last session" })
-- stop Persistence => session won't be saved on exit
keymap("n", "<leader>ld", [[<cmd>lua require("persistence").stop()<cr>]], { desc = "Don't save session on exit" })

--open search for files
--keymap("n", "<leader>f", "<cmd>Telescope find_files<cr>", { desc = "Open file search" })

--split windows
--horizontal
keymap("n", "<leader>s", "<cmd>split<cr>", { desc = "Horizontal split" })
--vertical
keymap("n", "<leader>v", "<cmd>vsplit<cr>", { desc = "Vertical split" })

--navigate windows
--up
keymap("n", "<leader><Up>", "<cmd>wincmd k<cr>", { desc = "Move to window above" })
--down
keymap("n", "<leader><Down>", "<cmd>wincmd j<cr>", { desc = "Move to window below" })
--left
keymap("n", "<leader><Left>", "<cmd>wincmd h<cr>", { desc = "Move to left window" })
--right
keymap("n", "<leader><Right>", "<cmd>wincmd l<cr>", { desc = "Move to right window" })

--markdown preview
keymap("n", "<leader>cp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "Markdown Preview" })
