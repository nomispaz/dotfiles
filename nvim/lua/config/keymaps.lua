local keymap = vim.api.nvim_set_keymap

--work with tabs (nvim buffers)
keymap("n", "<C-tab>", "<cmd>Telescope buffers<cr>", { desc = "Switch Tab" })
keymap("n", "<C-w>", "<cmd>bd<cr>", { desc = "Close Tab" })

--Neotree
keymap("n", "<leader>e", "<cmd>Neotree toggle<cr>", { desc = "Toggle Neotree (root dir)" })

--whichkey
keymap("n", "<leader>", "<cmd>WhichKey<cr>", { desc = "Open Whichkey" })

--session management
-- restore the session for the current directory
vim.api.nvim_set_keymap("n", "<leader>ls", [[<cmd>lua require("persistence").load()<cr>]], {desc = "Restore Session for current directory"} )
-- restore the last session
vim.api.nvim_set_keymap("n", "<leader>ll", [[<cmd>lua require("persistence").load({ last = true })<cr>]], { desc = "Restore last session" })
-- stop Persistence => session won't be saved on exit
vim.api.nvim_set_keymap("n", "<leader>ld", [[<cmd>lua require("persistence").stop()<cr>]], { desc = "Don't save session on exit" })

--open search for files
vim.api.nvim_set_keymap("n", "<leader>f", "<cmd>Telescope find_files<cr>", { desc = "Open file search" })
