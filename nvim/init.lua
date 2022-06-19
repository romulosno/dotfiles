local vim = vim

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

local map = require("rom_utils").map
map('n','<C-J>','<C-W><C-J>')
map('n','<C-K>','<C-W><C-K>')
map('n','<C-L>','<C-W><C-L>')
map('n','<C-H>','<C-W><C-H>')
map('n','<leader>ff','<cmd>Telescope find_files<CR>')
map('n','<leader>fg','<cmd>Telescope live_grep<cr>')
map('n','<leader>fb','<cmd>Telescope buffers<cr>')
map('n','<leader>fh','<cmd>Telescope help_tags<cr>')
map('n','<F2>',':sp %:p:h<CR>')
map('n','<F3>',':bd <CR>')
map('n','<leader>v', ':vsplit <CR>')
map('n','<leader>s', ':split <CR>')
map('n','<esc>', ':noh<CR>')
map('c','<C-x>','%:p:h')

require("rom_packages")
require("rom_telescope")
require("rom_lsp")
require("rom_cmp")

::eof::
