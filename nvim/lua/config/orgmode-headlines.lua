vim.cmd [[highlight Headline1 guibg=#24283b]]
vim.cmd [[highlight Headline2 guibg=#24283b]]
vim.cmd [[highlight CodeBlock guibg=#394b70]]
vim.cmd [[highlight Dash guibg=#D19A66 gui=bold]]

require("headlines").setup {
    org = {
        headline_highlights = { "Headline1", "Headline2" },
        fat_headlines = false,
       },
}
