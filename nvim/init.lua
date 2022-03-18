-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_exec(
[[
augroup Packer
autocmd!
autocmd BufWritePost init.lua PackerCompile
augroup end
]],
false
)


local use = require('packer').use
require('packer').startup(function()
	use 'wbthomason/packer.nvim' -- Package manager
	use 'b3nj5m1n/kommentary' -- Comentários
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use {
		'vimwiki/vimwiki',
		config = function()
			vim.g.vimwiki_list = {
				{
					path = '/home/xx/Documents/singularityOffice/wiki',
					syntax = 'markdown',
					ext = '.md',
				}
			}
		end
	}
	use{
		'L3MON4D3/LuaSnip',
		requires='rafamadriz/friendly-snippets'
	}
	use 'mfussenegger/nvim-dap'
	use 'nvim-lua/plenary.nvim'
	use {
		"mattn/emmet-vim",
		setup = function () -- load stuff before the plugin is loaded
			vim.g.user_emmet_leader_key = '<c-m>'
			vim.g.user_emmet_mode = 'n'
			vim.g.user_emmet_settings = {
				indent_blockelement = 1,
			}
		end
	}
	use {
		'neovim/nvim-lspconfig',
		'williamboman/nvim-lsp-installer',
	}
	use {
		'folke/which-key.nvim',
		config = function()
			require("which-key").setup({}) end
	}
	use 'hrsh7th/nvim-cmp'
	use {
		'windwp/nvim-autopairs',
		config = function()
			require('nvim-autopairs').setup{} 
		end
	}
	use {
		"ThePrimeagen/refactoring.nvim",
		requires = {
			{"nvim-lua/plenary.nvim"},
			{
				"nvim-treesitter/nvim-treesitter",
				run = ':TSUpdate'
			}
		},
		config = function()
			require('refactoring').setup({})
		end
	}
	use {
		'nvim-telescope/telescope.nvim',
		requires = { 'nvim-lua/plenary.nvim' }
	}
end)

--Remap space as leader key
vim.g.mapleader = " "
vim.g.completion_matching_strategy_list ='exact,substring,fuzzy'
vim.g.markdown_folding = 1
vim.wo.number = true
vim.o.completeopt = 'menuone,noinsert,noselect'
vim.o.hidden = true
vim.opt.clipboard = vim.opt.clipboard + 'unnamedplus'
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.opt.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.opt.shortmess = vim.opt.shortmess + 'c'
vim.o.updatetime = 250
vim.wo.signcolumn = 'no'
vim.o.showmatch = true
vim.o.backup = true
vim.o.wildmode = 'longest,list,full'
vim.o.wildmenu = true
vim.opt.wildignore = vim.opt.wildignore + '*/tmp/*,*.so,*.swp,*.zip'
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.splitbelow = true
vim.o.splitright = true


local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend('force', options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('n','<C-J>','<C-W><C-J>')
map('n','<C-K>','<C-W><C-K>')
map('n','<C-L>','<C-W><C-L>')
map('n','<C-H>','<C-W><C-H>')
map('n','<leader>ff','<cmd>Telescope find_files<CR>')
map('n','<leader>fg','<cmd>Telescope live_grep<cr>')
map('n','<leader>fb','<cmd>Telescope buffers<cr>')
map('n','<leader>fh','<cmd>Telescope help_tags<cr>')
map('n','<F2>',':e %:p:h<CR>')
map('n','<F3>','<cmd> lua require(\'telescope.builtin\').find_files( { cwd = vim.fn.expand(\'%:p:h\') })<cr>')
map('n','<F4>',':bd <CR>')
map('n','<leader>v', ':vsplit <CR>')
map('n','<leader>s', ':split <CR>')
map('n','<esc>', ':noh<CR>')
require('lsp')
require("luasnip.loaders.from_vscode").lazy_load()


local action_layout = require("telescope.actions.layout")
require('telescope').setup{
	defaults = {
		layout_config = {
			vertical = { width = 0.5 }
		},
		mappings = { 
			i = {
				["?"] = action_layout.toggle_preview,
			},
		},
	},
}
::eof::
