set nocompatible
filetype off

" -------------------------------- Plugins ----------------------------------- "
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
call vundle#rc("~/.config/nvim")

Plugin 'VundleVim/Vundle.vim'
Plugin 'init.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree'
Plugin 'sheerun/vim-polyglot'
Plugin 'cespare/vim-toml'
Plugin 'yggdroot/indentLine'
Plugin 'lervag/vimtex'
Plugin 'altercation/vim-colors-solarized.git'
Plugin 'tmsvg/pear-tree'
Plugin 'chrisbra/csv.vim'
Plugin 'guns/vim-sexp'
Plugin 'luochen1990/rainbow'

" tpope utilities
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'

call vundle#end()
filetype plugin indent on

" -------------------------------- Globals ----------------------------------- "
set mouse=a
set noswapfile
set encoding=utf-8

" Find
set path+=**
set wildmenu

" Columns
set number
set relativenumber
set textwidth=80
set cc=+0,+40

" Rows
set cursorline
set laststatus=2
set scrolloff=4

" Solarized Colors
syntax on
colorscheme solarized
autocmd Colorscheme * highlight! link SignColumn LineNr
let g:solarized_termtrans=1
call togglebg#map("<F5>")

" Spell check - use `set spell` in buffer to check
set spelllang=en,de

" Tags
set tags^=.git/tags;~

" ---------------------------- User Key Mappings ----------------------------- "
let mapleader=" "

" Buffers
set hidden
nnoremap <leader>bn :bnext<CR>
nnoremap <leader>bp :bprev<CR>
nnoremap <leader>bk :bdelete<CR>

" ------------------------------- FileTypes ---------------------------------- "
autocmd FileType help 
  \ setlocal number |
  \ setlocal relativenumber

" Python
au BufNewFile,BufRead *.py
  \ set tabstop=4 |
  \ set softtabstop=4 |
  \ set shiftwidth=4 |
  \ set textwidth=72 |
  \ set expandtab |
  \ set autoindent |
  \ set fileformat=unix

highlight BadWhitespace ctermbg=red guibg=red

" TeX
map <leader>cc :w! \| !compile "<c-r>%"<CR>
map <leader>cp :!opout <c-r>%<CR><CR>
autocmd VimLeave *.tex !texclear %

" dwmblocks
autocmd BufWritePost ~/.local/suckless/dwmblocks/config.h
  \ !cd ~/.local/suckless/dwmblocks;
  \ sudo make install && { killall -q dwmblocks; setsid -f dwmblocks }

" Diffs
if &diff
  highlight! link DiffText MatchParen
endif

" -------------------------------- Plugins ----------------------------------- "
" Vundle
let g:vundle_default_git_proto = 'git'

" Airline
" let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#displayed_head_limit = 20

" indentLine
let g:indentLine_bufTypeExclude = ['help', 'terminal']
let g:indentLine_fileTypeExclude = ['clj', 'cljs']

" NERDTree
nnoremap <F3> :NERDTreeToggle<CR>
let NERDTreeShowLineNumbers=1
let NERDTreeShowHidden=1

" Rainbow
let g:rainbow_active = 1
