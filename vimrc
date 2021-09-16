call plug#begin('~/.vim/plugged')
	Plug 'mattn/emmet-vim'
	Plug 'kien/ctrlp.vim'
call plug#end()

set nocompatible
set encoding=utf-8
set showmatch
set cursorline
set number
set ignorecase
set smartcase
set title
let python_highlight_all = 1 
syntax on

set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=number

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

" Enable folding
set foldmethod=indent
set foldlevel=99

" Tab size
set tabstop=2
set softtabstop=2
set shiftwidth=2

" == KEYBINDINGS ==

let mapleader=","
let g:user_emmet_leader_key=','
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'


" Split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright

" Enable folding with the spacebar
nnoremap <space> za

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

