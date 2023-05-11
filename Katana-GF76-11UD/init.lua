local Plug = vim.fn["plug#"]
local call = vim.call

call("plug#begin", "~/plugged")

Plug("nvim-treesitter/nvim-treesitter", { ["do"] = ":TSUpdate" })
Plug("sbdchd/neoformat")
Plug("numToStr/Comment.nvim")
Plug("nvim-lua/plenary.nvim")
Plug("nvim-telescope/telescope.nvim")
Plug("nvim-tree/nvim-tree.lua")
Plug("kyazdani42/nvim-web-devicons")
Plug("lukas-reineke/indent-blankline.nvim")
Plug("williamboman/mason.nvim")
Plug("williamboman/mason-lspconfig.nvim")
Plug("neovim/nvim-lspconfig")
Plug("nvim-lualine/lualine.nvim")
Plug("onsails/lspkind.nvim")
Plug("hrsh7th/cmp-nvim-lsp")
Plug("hrsh7th/cmp-buffer")
Plug("hrsh7th/cmp-path")
Plug("hrsh7th/cmp-cmdline")
Plug("hrsh7th/nvim-cmp")
Plug("SirVer/ultisnips")
Plug("quangnguyen30192/cmp-nvim-ultisnips")
Plug("kosayoda/nvim-lightbulb")
Plug("lewis6991/gitsigns.nvim")
Plug("svrana/neosolarized.nvim")
Plug("tjdevries/colorbuddy.nvim")

call("plug#end")

vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.number = true
vim.opt.scrolloff = 1
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.autoindent = true
vim.opt.showmode = true
vim.opt.showcmd = true
vim.opt.showmatch = true
vim.opt.incsearch = true
vim.opt.expandtab = true
vim.opt.termguicolors = true
vim.opt.linespace = 0
vim.o.wrap = false
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.o.updatetime = 100
vim.o.hlsearch = true
vim.wo.number = true
vim.o.mouse = "a"
vim.o.clipboard = "unnamedplus"
vim.o.breakindent = true
vim.o.undofile = false
vim.o.ignorecase = true
vim.o.smartcase = true
vim.wo.signcolumn = "yes"
vim.g.neoformat_try_node_exe = 1
vim.o.guifont = "FiraCode Nerd Font:h12"
vim.g.neovide_scale_factor = 1.0
vim.g.neovide_refresh_rate = 144
vim.g.neovide_no_idle = true
vim.g.neovide_confirm_quit = false
vim.g.neovide_remember_window_size = false
vim.g.neovide_cursor_trail_size = 0
vim.g.neovide_padding_top = 2
vim.g.neovide_padding_bottom = 2
vim.g.neovide_padding_right = 0
vim.g.neovide_padding_left = 0
vim.g.neovide_cursor_animate_command_line = false
vim.g.neovide_cursor_animate_in_insert_mode = false

require("lualine").setup({
	options = {
		theme = "solarized_dark",
	},
})

require("telescope").setup({
	extensions = {
		file_browser = {
			hijack_netrw = true,
		},
	},
	defaults = {
		file_ignore_patterns = { "node%_modules/.*" },
	},
})

require("Comment").setup({})

require("nvim-tree").setup({
	reload_on_bufenter = true,
	sync_root_with_cwd = true,
	renderer = {
		root_folder_label = false,
	},
})

require("nvim-treesitter.configs").setup({
	-- Add languages to be installed here that you want installed for treesitter
	ensure_installed = { "c", "cpp", "go", "lua", "python", "rust", "tsx", "typescript", "vimdoc", "vim" },

	-- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
	auto_install = false,

	highlight = { enable = true },
	indent = { enable = true, disable = { "python" } },
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "<c-space>",
			node_incremental = "<c-space>",
			scope_incremental = "<c-s>",
			node_decremental = "<M-space>",
		},
	},
	textobjects = {
		select = {
			enable = true,
			lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				["aa"] = "@parameter.outer",
				["ia"] = "@parameter.inner",
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["ac"] = "@class.outer",
				["ic"] = "@class.inner",
			},
		},
		move = {
			enable = true,
			set_jumps = true, -- whether to set jumps in the jumplist
			goto_next_start = {
				["]m"] = "@function.outer",
				["]]"] = "@class.outer",
			},
			goto_next_end = {
				["]M"] = "@function.outer",
				["]["] = "@class.outer",
			},
			goto_previous_start = {
				["[m"] = "@function.outer",
				["[["] = "@class.outer",
			},
			goto_previous_end = {
				["[M"] = "@function.outer",
				["[]"] = "@class.outer",
			},
		},
		swap = {
			enable = true,
			swap_next = {
				["<leader>a"] = "@parameter.inner",
			},
			swap_previous = {
				["<leader>A"] = "@parameter.inner",
			},
		},
	},
})

local cmp = require("cmp")

cmp.setup({
	snippet = {
		expand = function(args)
			vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
		end,
	},
	window = {
		-- completion = cmp.config.window.bordered(),
		-- documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
		["<C-b>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.abort(),
		["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
	}),
	sources = cmp.config.sources({
		{ name = "nvim_lsp" },
		{ name = "ultisnips" }, -- For ultisnips users.
	}, {
		{ name = "buffer" },
	}),
})

-- Set configuration for specific filetype.
cmp.setup.filetype("gitcommit", {
	sources = cmp.config.sources({
		{ name = "cmp_git" }, -- You can specify the `cmp_git` source if you were installed it.
	}, {
		{ name = "buffer" },
	}),
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ "/", "?" }, {
	mapping = cmp.mapping.preset.cmdline(),
	sources = {
		{ name = "buffer" },
	},
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(":", {
	mapping = cmp.mapping.preset.cmdline(),
	sources = cmp.config.sources({
		{ name = "path" },
	}, {
		{ name = "cmdline" },
	}),
})

require("mason").setup()
require("mason-lspconfig").setup({
	ensure_installed = { "lua_ls", "rust_analyzer", "tsserver", "eslint", "html", "jsonls", "vtsls" },
})
require("lspconfig").tsserver.setup({})

local map = vim.api.nvim_set_keymap

map("n", "<ESC>", ":nohlsearch<CR>", { silent = true })
map("n", "<C-P>", ":Telescope find_files hidden=true<CR>", { noremap = true })
map("n", "<C-F>", ":Telescope live_grep hidden=true<CR>", { noremap = true })
map("n", "<C-N>", ":NvimTreeFocus <CR>", { noremap = true })
map("i", "<C-S>", "<ESC>:wall<CR>", { noremap = true })
map("n", "<C-S>", ":wall<CR>", { noremap = true })

vim.cmd([[
    augroup cmdline
    autocmd!
    autocmd CmdlineLeave : echo ''
    autocmd VimLeave * set guicursor=a:ver25
    augroup end
]])

vim.cmd([[
    autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false, scope="cursor"})
]])

vim.o.background = "dark"

require("neosolarized").setup({
	comment_italics = true,
	background_set = true,
})

vim.cmd([[
    augroup fmt
    autocmd!
    autocmd BufWritePre * Neoformat
    augroup end
]])

vim.diagnostic.config({
	virtual_text = false,
})
