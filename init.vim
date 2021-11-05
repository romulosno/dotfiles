call plug#begin('~/.vim/plugged')
Plug 'neovim/nvim-lspconfig'
Plug 'mattn/emmet-vim'
Plug 'kabouzeid/nvim-lspinstall'
Plug 'jiangmiao/auto-pairs'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'folke/lsp-trouble.nvim'
Plug 'airblade/vim-gitgutter'
call plug#end()

set nocompatible
set encoding=utf-8
set showmatch
set cursorline
set number
set ignorecase
set smartcase
set hidden
set title
let python_highlight_all = 1 
syntax on

set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=number
set completeopt=menuone,noinsert,noselect

" Explore root folder as current one
set autochdir

" No backup
set nobackup
set nowritebackup

" Wildmode
set wildmode=longest,list,full
set wildmenu

" Wildignore
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux

" Tab size
set tabstop=2
set softtabstop=2
set shiftwidth=2

let mapleader=","
let g:user_emmet_leader_key=','
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
let g:gitgutter_max_signs = -1

" Split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright


let g:completion_confirm_key = ""
imap <expr> <cr>  pumvisible() ? complete_info()["selected"] != "-1" ?
			\ "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<CR>"

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
vim.api.nvim_set_keymap("n", "<leader>xx", "<cmd>LspTroubleToggle<cr>",
  {silent = true, noremap = true}
)
vim.api.nvim_set_keymap("n", "<leader>xw", "<cmd>LspTroubleToggle lsp_workspace_diagnostics<cr>",
  {silent = true, noremap = true}
)
vim.api.nvim_set_keymap("n", "<leader>xd", "<cmd>LspTroubleToggle lsp_document_diagnostics<cr>",
  {silent = true, noremap = true}
)
vim.api.nvim_set_keymap("n", "<leader>xl", "<cmd>LspTroubleToggle loclist<cr>",
  {silent = true, noremap = true}
)
vim.api.nvim_set_keymap("n", "<leader>xq", "<cmd>LspTroubleToggle quickfix<cr>",
  {silent = true, noremap = true}
)
vim.api.nvim_set_keymap("n", "gR", "<cmd>LspTrouble lsp_references<cr>",
  {silent = true, noremap = true}
)

local nvim_lsp = require('lspconfig')

local cmp = require('cmp')
cmp.setup({
	snippet = {
		expand = function(args)
		-- For `vsnip` user.
		vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` user.
	
		-- For `luasnip` user.
		-- require('luasnip').lsp_expand(args.body)
	
		-- For `ultisnips` user.
		-- vim.fn["vsnip#anonymous"](args.body)
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

		-- For vsnip user.
		{ name = 'vsnip' },

		-- For luasnip user.
		-- { name = 'luasnip' },

		-- For ultisnips user.
		-- { name = 'ultisnips' },

		{ name = 'buffer' },
		}
	})
	
  require("trouble").setup {
    fold_open = "v", -- icon used for open folds
    fold_closed = ">", -- icon used for closed folds
    indent_lines = false, -- add an indent guide below the fold icons
    signs = {
        -- icons / text used for a diagnostic
        error = "error",
        warning = "warn",
        hint = "hint",
        information = "info"
    },
    use_lsp_diagnostic_signs = false -- enabling this will use the signs defined in your lsp client
		}
  
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
	buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
	buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
	buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
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

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
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
