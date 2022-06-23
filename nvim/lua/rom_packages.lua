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

local use = require'packer'.use
require('packer').startup(function()
	use 'wbthomason/packer.nvim' -- Package manager
	use 'b3nj5m1n/kommentary' -- Comentários
	use { -- Completion
		'hrsh7th/cmp-nvim-lsp',
		'hrsh7th/cmp-buffer',
		'hrsh7th/nvim-cmp',
	}
	use { -- LSP
		'neovim/nvim-lspconfig',
		'williamboman/nvim-lsp-installer',
	}
	use { -- Autopairs
		'windwp/nvim-autopairs',
		config = function()
			require('nvim-autopairs').setup {}
		end
	}
	use { -- Refactoring
		"ThePrimeagen/refactoring.nvim",
		requires = {
			{
				"nvim-treesitter/nvim-treesitter",
				run = ':TSUpdate'
			}
		},
		config = function()
			require('refactoring').setup({})
		end
	}
	use { -- Telescope
		'nvim-telescope/telescope.nvim',
		requires = { 'nvim-lua/plenary.nvim' }
	}
	use 'L3MON4D3/LuaSnip'
end)

