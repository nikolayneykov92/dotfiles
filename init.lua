local Plug = vim.fn['plug#']
local call = vim.call

call('plug#begin', '~/.config/nvim/plugged')

Plug('nvim-treesitter/nvim-treesitter', { ['do'] = ':TSUpdate' })
Plug('numToStr/Comment.nvim')
Plug('nvim-lua/plenary.nvim')
Plug('nvim-telescope/telescope.nvim')
Plug('kyazdani42/nvim-web-devicons')
Plug('lukas-reineke/indent-blankline.nvim')
-- Plug('neoclide/coc.nvim')
Plug('vim-airline/vim-airline')
Plug('vim-airline/vim-airline-themes')
--     Plug 'kyazdani42/nvim-tree.lua'
Plug('Mofiqul/dracula.nvim')
Plug('folke/tokyonight.nvim', { branch = 'main' })

call('plug#end')


require('telescope').setup {}
require('Comment').setup {}
-- require('nvim-tree').setup{}
--     require("nvim-tree").setup{
--         view = {
--                 adaptive_size = true,
--             }
--     }

require('nvim-treesitter.configs').setup {
  ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "javascript" },
  sync_install = true,
  auto_install = true,
  ignore_install = {},
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

local opt = vim.opt

opt.backup = false
opt.swapfile = false
opt.number = true
opt.scrolloff = 4
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.autoindent = true
opt.showmode = true
opt.showcmd = true
opt.showmatch = true
opt.hlsearch = false
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.expandtab = true
opt.termguicolors = true
opt.clipboard = {"unnamed" ,"unnamedplus"}




-- nnoremap <ESC> :let @/=""<CR>
-- vim.keymap.<C-P> :Telescope find_files<CR>
-- nnoremap <C-F> :Telescope live_grep<CR>
-- nnoremap <C-N> :NvimTreeOpen <CR>

local augroup = vim.api.nvim_create_augroup('cmdline', { clear = true })
local cmd = vim.cmd

cmd([[
    augroup cmdline
    autocmd!
    autocmd CmdlineLeave : echo ''
    autocmd VimLeave * set guicursor=a:ver25
    augroup end
]])

cmd('colorscheme tokyonight-night')
