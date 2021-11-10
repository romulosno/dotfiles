call plug#begin('~/.vim/plugged')
Plug 'neovim/nvim-lspconfig' "LSP
Plug 'mattn/emmet-vim' "HTML tags
Plug 'kabouzeid/nvim-lspinstall' "new lsp installations
Plug 'jiangmiao/auto-pairs' "close brackets
Plug 'hrsh7th/cmp-nvim-lsp' "lsp completion
Plug 'hrsh7th/cmp-buffer' 
Plug 'hrsh7th/nvim-cmp'
Plug 'airblade/vim-gitgutter' "git 
Plug 'jremmen/vim-ripgrep' "ripgrep
Plug 'junegunn/fzf.vim' "fzf
Plug 'folke/lsp-trouble.nvim' "lsp diagnostic
Plug 'kyazdani42/nvim-web-devicons'

let g:gitgutter_max_signs = -1
let g:user_emmet_leader_key=','
call plug#end()

syntax on
set nocompatible
set encoding=utf-8
set showmatch
set ruler
set number
set ignorecase
set smartcase
set hidden
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=number
set completeopt=menuone,noinsert,noselect
set nobackup
set nowritebackup
set wildmode=longest,list,full
set wildmenu
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set tabstop=2
set softtabstop=2
set shiftwidth=2
set splitbelow
set splitright

let mapleader=","
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']

au BufEnter * silent! lcd %:p:h

" == MAPS ==
nmap <C-J> <C-W><C-J>
nmap <C-K> <C-W><C-K>
nmap <C-L> <C-W><C-L>
nmap <C-H> <C-W><C-H>
nmap <C-p> :Rg 
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)

" == FILETYPE CONF ==
au BufNewFile,BufRead *.py
			\set tabstop=4
			\set softtabstop=4
			\set shiftwidth=4
			\set textwidth=79
			\set expandtab
			\set autoindent
			\set fileformat=unix

au BufNewFile,BufRead *.js, *.html, *.css
			\set tabstop=2
			\set softtabstop=2
			\set shiftwidth=2

lua << EOF
local nvim_lsp = require('lspconfig')
local cmp = require('cmp')
cmp.setup({
	snippet = {
		expand = function(args)
		vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` user.
	end,
	},
	mapping = {
		['<C-d>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-e>'] = cmp.mapping.close(),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
		},
	sources = {
		{ name = 'nvim_lsp' },
		{ name = 'vsnip' },
		{ name = 'buffer' },
		}
	})
  
	local on_attach = function(client, bufnr)
	local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
	local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

	buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

	local opts = { noremap=true, silent=true }

	-- See `:help vim.lsp.*` for documentation on any of the below functions
	buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
	buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
	buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
	buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
	buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
--	buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<cr>', opts)
--	buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
--	buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
	buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
	buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
	buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
	buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
	buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
	buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
	buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
	buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
	buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

end

require'lspinstall'.setup()
local servers = require('lspinstall').installed_servers()
for _, lsp in ipairs(servers) do
	nvim_lsp[lsp].setup {
		on_attach = on_attach,
		flags = {
			debounce_text_changes = 150,
			},
		capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
		}
end
EOF
