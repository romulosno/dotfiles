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
	use 'neovim/nvim-lspconfig' -- use a language server
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/nvim-cmp' -- for completion whilst using the language server
	use 'github/copilot.vim' -- for AI completion
	use 'saadparwaiz1/cmp_luasnip' -- vim cmp requires luasnip
	use 'L3MON4D3/LuaSnip' -- Snippets plugin
	use 'nvim-lua/plenary.nvim'
	use 'nvim-lua/popup.nvim'
	use 'mattn/emmet-vim' 
	use 'folke/which-key.nvim'
	use 'kabouzeid/nvim-lspinstall' 
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
end)

vim.g.mapleader = ','
vim.g.user_emmet_leader_key=','
vim.g.completion_matching_strategy_list ='exact,substring,fuzzy'

--Make line numbers default
vim.wo.number = true

--Do not save when switching buffers (note: this is now a default on master)
vim.o.hidden = true

vim.opt.clipboard = vim.opt.clipboard + 'unnamedplus'

--Enable mouse mode
vim.o.mouse = 'a'

--Enable break indent
vim.o.breakindent = true

--Save undo history
vim.opt.undofile = true

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.opt.shortmess = vim.opt.shortmess + 'c'
vim.o.updatetime = 250
vim.wo.signcolumn = 'no'

-- highlight match brackets
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
vim.api.nvim_set_keymap('n','<F2>',':e %:p:h<CR>',{})
vim.api.nvim_set_keymap('n','<C-f>',':Files %:p:h<CR>',{})
vim.api.nvim_set_keymap('n','<C-p>',':Files<CR>',{})


require('lsp')

::eof::
