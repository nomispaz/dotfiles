
return {
    {
    'nvim-orgmode/orgmode',
    dependencies = {
        { 'nvim-treesitter/nvim-treesitter', lazy = true },
    },
    event = 'VeryLazy',
    config = function()
        
        -- Setup treesitter
        require('nvim-treesitter.configs').setup({
        highlight = {
            enable = true,
        },
        ensure_installed = { 'org' },
        })

        -- Setup orgmode
        require('orgmode').setup({
        org_agenda_files = '/data/orgmode/**/*',
        org_default_notes_file = '/data/orgmode/refile.org',
        org_startup_indented = true,
        org_hide_leading_stars = true,
        org_hide_emphasis_markers = true,
        org_id_link_to_org_use_id = true,
        emacs_config = { executable_path = 'emacs', config_path='$HOME/.config/nvim/init_export.el' }

        })
    end,
    }
}