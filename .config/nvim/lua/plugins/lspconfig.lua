return {
    "neovim/nvim-lspconfig",
    vim.lsp.enable('marksman'),
    vim.lsp.enable('gopls'),
    vim.lsp.config('pylsp', {
      settings = {
        pylsp = {
          plugins = {
            pyflakes = { 
	      enabled = true,
              maxLineLength = 200},
              black = { enabled = true },
              pylsp_mypy = { enabled = true },
              pycodestyle = {
	        maxLineLength = 200,
              },
          }
        }
      }
    })
}
