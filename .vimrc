
set nocompatible

runtime bundle/unbundle/unbundle.vim

syntax on
filetype off
filetype plugin indent on

" {{{ basics preference

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set number
set showmatch

set hlsearch
set incsearch
set ignorecase smartcase

set switchbuf=useopen,split
set foldmethod=marker

cabbr w!! w !sudo tee % >/dev/null

" }}}

" {{{ quick (and dirty) setting for single file C/C++, Java & Haskell hacking

let g:map_leader = '\'

augroup SingleFileHacking

    autocmd!

    autocmd FileType cpp,c :nmap <silent> <Leader>c :make %<<CR>
    autocmd FileType cpp,c :nmap <silent> <Leader>r :!./%<<CR>

    autocmd FileType java :set makeprg=javac\ % errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
    autocmd FileType java :nmap <silent> <Leader>c :make<CR>
    autocmd FileType java :nmap <silent> <Leader>r :!java -ea %<<CR>

    autocmd FileType haskell :set makeprg=ghc\ --make\ %
    autocmd FileType haskell :set errorformat=
                                 \%-Z\ %#,
                                 \%W%f:%l:%c:\ Warning:\ %m,
                                 \%E%f:%l:%c:\ %m,
                                 \%E%>%f:%l:%c:,
                                 \%+C\ \ %#%m,
                                 \%W%>%f:%l:%c:,
                                 \%+C\ \ %#%tarning:\ %m,
    autocmd FileType haskell :nmap <silent> <Leader>c :make<CR>
    autocmd FileType haskell :nmap <silent> <Leader>r :!./%<<CR>

    autocmd FileType cpp :imap <F9> #include <vector><CR>#include <list><CR>#include <map><CR>#include <set><CR>#include <deque><CR>#include <queue><CR>#include <stack><CR>#include <bitset><CR>#include <algorithm><CR>#include <functional><CR>#include <numeric><CR>#include <utility><CR>#include <complex><CR>#include <sstream><CR>#include <iostream><CR>#include <iomanip><CR>#include <cstdio><CR>#include <cmath><CR>#include <cstdlib><CR>#include <cstring><CR>#include <ctime><CR>#include <cassert><CR>using namespace std;<CR><CR>#define FOR(it,a) for (__typeof((a).begin()) it = (a).begin(); it != (a).end(); ++it)<CR><CR>

augroup END

" }}}

runtime! config/**/*.vim
