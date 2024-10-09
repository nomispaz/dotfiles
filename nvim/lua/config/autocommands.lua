vim.api.nvim_create_autocmd(
    {"BufEnter"}, 
    { pattern = "*",    
    desc = "Automatically change directory to directory of current file",
    command = "cd %:p:h"
   }
)