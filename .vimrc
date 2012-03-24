set nocompatible

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
filetype plugin indent on

if has("gui_running") || &t_Co == 256
  colorscheme herald
else
  colorscheme jellybeans
endif

set hlsearch
set fileencodings=utf-8,gbk,cp936
set fileformats=unix,dos
set textwidth=78

set switchbuf=useopen,split
set autoindent
set foldmethod=marker
imap <F1> <ESC>
nnoremap \f *``
nnoremap \c :nohl<CR>

"tagbar
nmap <F8> :TagbarToggle<CR>

"haskell related
let g:ghcmod_ghc_options=['-w']
autocmd BufWritePost *.hs,*.lhs GhcModCheckAsync
autocmd BufEnter *.hsc :set filetype=haskell
autocmd BufEnter *.hsc :nmap <F5> :!hsc2hs %;ghc --make %<.hs<CR>

autocmd FileType cpp,c,java :set cindent
autocmd FileType cpp,c,java :let b:surround_{char2nr("c")} = "/* \r */"

autocmd FileType cpp,c :nmap <F5> :make %<<CR>
autocmd FileType cpp,c :nmap <F6> :!./%<<CR>

autocmd FileType java :set makeprg=javac\ % errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
autocmd FileType java :nmap <F5> :make<CR>
autocmd FileType java :nmap <F6> :!java -ea %<<CR>
autocmd FileType tex :nmap <F5> :!xelatex %<CR>
autocmd FileType tex :nmap <F6> :!evince %<.pdf<CR>

autocmd FileType haskell,lhaskell :set omnifunc=necoghc#omnifunc
autocmd FileType haskell,lhaskell :set softtabstop=2
autocmd FileType haskell,lhaskell :set makeprg=ghc\ --make\ %
autocmd FileType haskell,lhaskell :set errorformat=
                                      \%-Z\ %#,
                                      \%W%f:%l:%c:\ Warning:\ %m,
                                      \%E%f:%l:%c:\ %m,
                                      \%E%>%f:%l:%c:,
                                      \%+C\ \ %#%m,
                                      \%W%>%f:%l:%c:,
                                      \%+C\ \ %#%tarning:\ %m,
autocmd FileType haskell,lhaskell :nmap <F4> :GhcModLintAsync<CR>
autocmd FileType haskell,lhaskell :nmap <F5> :make<CR>
autocmd FileType haskell,lhaskell :nmap <F6> :!./%<<CR>
autocmd FileType haskell,lhaskell :nmap <F7> :GhcModExpand<CR>
autocmd FileType haskell,lhaskell :nmap <Tab> :GhcModType<CR>
autocmd FileType haskell,lhaskell :nmap <s-Tab> :GhcModTypeClear<CR>
autocmd FileType haskell,lhaskell :let b:surround_{char2nr("c")} = "{- \r -}"

autocmd FileType scala :nmap <F4> :!fsc -shutdown<CR>
autocmd FileType scala :nmap <F5> :make<CR>
autocmd FileType scala :nmap <F6> :!scala Main<CR>
autocmd FileType scala :set makeprg=fsc\ % errorformat=%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

autocmd FileType lhaskell :nmap <F7> :set filetype=pandoc<CR>
autocmd FileType pandoc :nmap <F7> :set filetype=lhaskell<CR>

autocmd FileType pandoc :nnoremap \- yyp<c-v>$r-
autocmd FileType pandoc :nnoremap \= yyp<c-v>$r=
autocmd FileType pandoc :let b:surround_{char2nr("i")} = "_\r_"
autocmd FileType pandoc :let b:surround_{char2nr("b")} = "**\r**"

map <c-a> ggVG
map <c-c> "+y

nnoremap qj :cnext<CR>
nnoremap qk :cprev<CR>
nnoremap qq :cc<CR>
nnoremap qo :copen<CR>
nnoremap qc :cclose<CR>
nnoremap qm :make<CR>
nnoremap qM :make<Space>

highlight WhitespaceEOL ctermbg=grey guibg=grey
autocmd FileType c,cpp,haskell,java match WhitespaceEOL /\s\+$/
highlight OverLength ctermbg=grey guibg=grey
autocmd FileType c,cpp,haskell,java 2match OverLength /\%80v.*/

cmap w!! w !sudo tee % >/dev/null

autocmd FileType cpp :imap <F9> #include <vector><CR>#include <list><CR>#include <map><CR>#include <set><CR>#include <deque><CR>#include <queue><CR>#include <stack><CR>#include <bitset><CR>#include <algorithm><CR>#include <functional><CR>#include <numeric><CR>#include <utility><CR>#include <complex><CR>#include <sstream><CR>#include <iostream><CR>#include <iomanip><CR>#include <cstdio><CR>#include <cmath><CR>#include <cstdlib><CR>#include <cstring><CR>#include <ctime><CR>#include <cassert><CR>using namespace std;<CR>
