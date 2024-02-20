return {
    "neovim/nvim-lspconfig",
    dependencies = {
        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim"
    },
    --opts = {
--         signs = {
  --        text = {
    --        [vim.diagnostic.severity.ERROR] = require("config.icons").icons.diagnostics.Error,
      --      [vim.diagnostic.severity.WARN] = require("config.icons").icons.diagnostics.Warn,
        --    [vim.diagnostic.severity.HINT] = require("config.icons").icons.diagnostics.Hint,
          --  [vim.diagnostic.severity.INFO] = require("config.icons").icons.diagnostics.Info,
      --    },
       -- },
   config = function()
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

    require('mason').setup()
    local mason_lspconfig = require 'mason-lspconfig'
    mason_lspconfig.setup {
        ensure_installed = { "pyright" }
    }
    require("lspconfig").pyright.setup {
        capabilities = capabilities,
    }
  end
}
