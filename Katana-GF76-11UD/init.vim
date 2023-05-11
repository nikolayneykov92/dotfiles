call plug#begin('~/plugged')

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'numToStr/Comment.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'lukas-reineke/indent-blankline.nvim'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'Mofiqul/dracula.nvim'

call plug#end()

lua << EOF

    require('telescope').setup{}

    require('Comment').setup{}
    
    require('indent_blankline').setup{}

    require("nvim-tree").setup{
        view = {
                adaptive_size = true,
            }
    }

EOF

set background=dark
set encoding=utf-8
set backspace=indent,eol,start
set nobackup
set noswapfile
set number 
set scrolloff=2
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set autoindent
set showmode
set showcmd
set showmatch
set hlsearch
set incsearch
set ignorecase
set smartcase
set wildmenu wildmode=list:longest,full
set clipboard+=unnamedplus

colorscheme dracula


nnoremap <ESC> :let @/=""<CR>
nnoremap <C-P> :Telescope find_files<CR>
nnoremap <C-F> :Telescope live_grep<CR>
nnoremap <C-N> :NvimTreeOpen <CR>

augroup cmdline
    autocmd!
    autocmd CmdlineLeave : echo ''
    autocmd VimLeave * set guicursor=a:ver25
augroup end
