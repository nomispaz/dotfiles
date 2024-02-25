local keymap = vim.api.nvim_set_keymap

--change s key to i --> enter input mode
keymap( "n", "s", "i", { desc = "Switch to visual mode" })


--work with tabs (nvim buffers)
--keymap("n", "<C-tab>", "<cmd>Telescope buffers<cr>", { desc = "Switch Tab" })
--keymap("n", "<C-w>", "<cmd>bd<cr>", { desc = "Close Tab" })

--Neotree
--keymap("n", "<leader>e", "<cmd>Neotree toggle<cr>", { desc = "Toggle Neotree (root dir)" })

--session management
-- restore the session for the current directory
--keymap("n", "<leader>ls", [[<cmd>lua require("persistence").load()<cr>]], {desc = "Restore Session for current directory"} )
-- restore the last session
--keymap("n", "<leader>ll", [[<cmd>lua require("persistence").load({ last = true })<cr>]], { desc = "Restore last session" })
-- stop Persistence => session won't be saved on exit
--keymap("n", "<leader>ld", [[<cmd>lua require("persistence").stop()<cr>]], { desc = "Don't save session on exit" })

--open search for files
--keymap("n", "<leader>f", "<cmd>Telescope find_files<cr>", { desc = "Open file search" })



--markdown preview
--keymap("n", "<leader>cp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "Markdown Preview" })
