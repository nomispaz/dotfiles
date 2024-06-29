return {
    "hrsh7th/nvim-cmp",
    version = false,
    dependencies = {
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "L3MON4D3/LuaSnip",
        "saadparwaiz1/cmp_luasnip",
    },
    
    config = function()

	local cmp = require('cmp')
    local luasnip = require('luasnip')

    cmp.setup({
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end
      },
      completion = {
        --autocomplete = false
		completeopt = 'menu,menuone,noinsert'
      },
       mapping = cmp.mapping.preset.insert ({
           ["<Tab>"] = cmp.mapping(function(fallback)
           if luasnip.expand_or_jumpable() then
             luasnip.expand_or_jump()
          else
             fallback()
           end
         end, { "i", "s" }),
         ["<c-e>"] = cmp.mapping.abort(),
         ["<CR>"] = cmp.mapping.confirm({ select=true }),
        }),
      sources = {
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "buffer" },
        { name = "path" },
        { name = "orgmode" },
      }
    })
  end
}