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
	use 'tpope/vim-commentary' -- "gc" to comment visual regions/lines
	use {
		'neovim/nvim-lspconfig', 
		'williamboman/nvim-lsp-installer',
	}
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/vim-vsnip'
	use 'hrsh7th/nvim-cmp' -- for completion whilst using the language server
	use 'saadparwaiz1/cmp_luasnip' -- vim cmp requires luasnip
	use 'nvim-lua/plenary.nvim'
	use 'mattn/emmet-vim' 
	use 'jiangmiao/auto-pairs' 
	use 'junegunn/fzf'
	use 'junegunn/fzf.vim' 
	use {
		"ThePrimeagen/refactoring.nvim",
		requires = {
			{"nvim-lua/plenary.nvim"},
			{"nvim-treesitter/nvim-treesitter"}
		}
	}
	use 'ygm2/rooter.nvim'
	use 'fatih/vim-go'
	use 'vimwiki/vimwiki'
end)

vim.g.mapleader = ","
vim.g.rooter_pattern = {'.git', 'Makefile', '_darcs', '.hg', '.bzr', '.svn', 'node_modules', 'CMakeLists.txt'} 
vim.g.outermost_root = true
vim.g.user_emmet_leader_key= ","
vim.g.user_emmet_mode = 'n'
vim.g.completion_matching_strategy_list ='exact,substring,fuzzy'
vim.g.markdown_folding = 1
vim.wo.number = true
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

vim.api.nvim_set_keymap('n','<C-J>','<C-W><C-J>',{})
vim.api.nvim_set_keymap('n','<C-K>','<C-W><C-K>',{})
vim.api.nvim_set_keymap('n','<C-L>','<C-W><C-L>',{})
vim.api.nvim_set_keymap('n','<C-H>','<C-W><C-H>',{})
vim.api.nvim_set_keymap('n','<C-f>',':Files %:p:h<CR>',{})
vim.api.nvim_set_keymap('n','<C-p>',':Files<CR>',{})
vim.api.nvim_set_keymap('n','<F2>',':e %:p:h<CR>',{})
vim.api.nvim_set_keymap('n','<F3>',':e %:p:h',{})
vim.api.nvim_set_keymap('n','<F4>',':cd %:p:h',{})


require('lsp')

::eof::
