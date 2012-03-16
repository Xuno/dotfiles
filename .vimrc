set undolevels=9999
set history=9999
set wildchar=<tab>
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set number
set showmatch
set backspace=indent,eol,start
set nostartofline
set display+=uhex

set autowrite
set wildmenu
command ConvertToHTML so $VIMRUNTIME/syntax/2html.vim

call pathogen#infect()

syntax on
filetype plugin on
colorscheme herald
set t_Co=256

set autoindent
imap <F1> <ESC>

autocmd BufEnter *.hsc :set filetype=haskell
autocmd BufEnter *.hsc :map <F5> :!hsc2hs %;ghc --make -O2 -threaded -rtsopts %<.hs<CR>
autocmd FileType cpp,c,java :set cindent
autocmd FileType cpp,c :map <F5> :make %<<CR>
autocmd FileType cpp,c :map <F6> :!./%<<CR>
autocmd FileType java set makeprg=javac\ % errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
autocmd FileType java :map <F5> :make<CR>
autocmd FileType java :map <F6> :!java -ea %<<CR>
autocmd FileType tex :map <F5> :!xelatex %<CR>
autocmd FileType tex :map <F6> :!evince %<.pdf<CR>
autocmd FileType haskell,lhaskell :set omnifunc=necoghc#omnifunc
autocmd FileType haskell,lhaskell :set softtabstop=2
autocmd FileType haskell,lhaskell :map <F5> :!ghc --make -O2 -threaded -rtsopts %<CR>
autocmd FileType haskell,lhaskell :map <F6> :!./%<<CR>
autocmd FileType haskell,lhaskell :map <F3> :w<CR>:!quickCheck %<CR>
autocmd FileType scala :map <F4> :!fsc -shutdown<CR>
autocmd FileType scala :map <F5> :make<CR>
autocmd FileType scala :map <F6> :!scala Main<CR>
autocmd FileType scala set errorformat=%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,%-G%.%#
autocmd FileType scala set makeprg=fsc\ %

map <c-a> ggVG
map <c-c> "+y

imap <F9> #include <vector> <CR>#include <list> <CR>#include <map> <CR>#include <set> <CR>#include <deque> <CR>#include <queue> <CR>#include <stack> <CR>#include <bitset> <CR>#include <algorithm> <CR>#include <functional> <CR>#include <numeric> <CR>#include <utility> <CR>#include <complex> <CR>#include <sstream> <CR>#include <iostream> <CR>#include <iomanip> <CR>#include <cstdio> <CR>#include <cmath> <CR>#include <cstdlib> <CR>#include <cstring> <CR>#include <ctime> <CR>#include <cassert> <CR>using namespace std;<CR>
