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

syntax on
filetype plugin on
colorscheme herald
set t_Co=256

"haskell-mode
au BufEnter *.hs compiler ghc
au BufEnter *.lhs compiler ghc
let g:haddock_browser="/usr/bin/firefox"

" target neocomplcache version : 6.2, install ghc-mode binary first
let g:neocomplcache_enable_at_startup = 1 

" ghcmod-vim
autocmd FileType haskell,lhaskell :map <F4> :GhcModLint<CR>
autocmd FileType haskell,lhaskell :map <c-s-t> :GhcModType<CR>
autocmd FileType haskell,lhaskell :map <c-s-e> :GhcModTypeClear<CR>

set autoindent
imap <F1> <ESC>

autocmd FileType cpp,c,java :set cindent
autocmd FileType cpp,c :map <F5> :make %<<CR>
autocmd FileType cpp,c :map <F6> :!./%<<CR>
autocmd FileType java :map <F5> :make<CR>
autocmd FileType java :map <F6> :!java -ea %<<CR>
autocmd FileType tex :map <F4> :!texclear<CR>
autocmd FileType tex :map <F5> :!xelatex %<CR>
autocmd FileType tex :map <F6> :!evince %<.pdf<CR>
autocmd FileType go :map <F5> :!8g %;8l -o %< %<.8<CR>
autocmd FileType go :map <F6> :!./%<<CR>
autocmd FileType haskell,lhaskell :set softtabstop=2
autocmd FileType haskell,lhaskell :map <F5> :!ghc --make -O2 -threaded -rtsopts %<CR>
autocmd FileType haskell,lhaskell :map <F6> :!./%<<CR>
autocmd FileType haskell,lhaskell :map <F3> :w<CR>:!quickCheck %<CR>
autocmd FileType lhaskell :map <F7> :set syntax=pdc<CR>
autocmd FileType lhaskell :map <F8> :set syntax=lhaskell<CR>
autocmd FileType java set makeprg=javac\ % errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
autocmd Filetype java setlocal omnifunc=javacomplete#Complete
autocmd Filetype java setlocal completefunc=javacomplete#CompleteParamsInfo
autocmd FileType scala :map <F4> :!fsc -shutdown<CR>
autocmd FileType scala :map <F5> :make<CR>
autocmd FileType scala :map <F6> :!scala Main<CR>
autocmd FileType scala set errorformat=%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,%-G%.%#
autocmd FileType scala set makeprg=fsc\ %
autocmd BufEnter *.md set syntax=pdc

map <c-a> ggVG
map <c-c> "+y

ab #i #include
ab #d #define

imap <F9> #include <vector> <CR>#include <list> <CR>#include <map> <CR>#include <set> <CR>#include <deque> <CR>#include <queue> <CR>#include <stack> <CR>#include <bitset> <CR>#include <algorithm> <CR>#include <functional> <CR>#include <numeric> <CR>#include <utility> <CR>#include <complex> <CR>#include <sstream> <CR>#include <iostream> <CR>#include <iomanip> <CR>#include <cstdio> <CR>#include <cmath> <CR>#include <cstdlib> <CR>#include <cstring> <CR>#include <ctime> <CR>#include <cassert> <CR>using namespace std;<CR>

"pandoc
au BufNewFile,BufRead *.md,*.markdown setf pdc
au Syntax pdc        so ~/.vim/syntax/pdc.vim

"jflex
au BufNewFile,BufRead *.flex,*.jflex setf jflex
au Syntax jflex      so ~/.vim/syntax/jflex.vim

"java cup
au BufNewFile,BufRead *.cup setf cup
au Syntax cup        so ~/.vim/syntax/cup.vim

"tiger
au BufNewFile,BufRead *.tig setf tiger
au Syntax tiger      so ~/.vim/syntax/tiger.vim
