
set nocompatible

set undolevels=9999
set history=9999
set wildchar=<tab>
set wildignore=*.o,*.bak,*~,hi,*.git
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

let mapleader = ","

set background=dark

if has("gui_running") || &t_Co == 256
  colorscheme herald
else
  colorscheme default
endif

set hlsearch
set fileencodings=utf-8,gbk,cp936
set fileformats=unix,dos
set textwidth=78

set switchbuf=useopen,split
set autoindent
set foldmethod=marker

syntax on
filetype plugin indent on

command ConvertToHTML so $VIMRUNTIME/syntax/2html.vim
call pathogen#infect()

" ThinkPad sucks
imap <F1> <ESC>

"tagbar
nmap <silent> <F8> :TagbarToggle<CR>

"haskell related
let g:ghcmod_ghc_options=['-w']

autocmd BufNewFile *.sh 0put=\"#!/bin/bash\<nl>\"
autocmd BufNewFile *.rb 0put=\"#!/usr/bin/env ruby\<nl>\"
autocmd BufNewFile *.py 0put=\"#!/usr/bin/env python\<nl>\"

autocmd BufWritePost *.hs,*.lhs GhcModCheckAsync
autocmd BufEnter *.hsc :setlocal filetype=haskell
autocmd BufEnter *.hsc :nmap <silent> <F5> :!hsc2hs %;ghc --make %<.hs<CR>

autocmd FileType cpp,c,java :let b:surround_{char2nr("c")} = "/* \r */"
autocmd FileType cpp,c,java :setlocal commentstring=//\ %s

autocmd FileType cpp,c :nmap <silent> <F5> :make %<<CR>
autocmd FileType cpp,c :nmap <silent> <F6> :!./%<<CR>

autocmd FileType java :setlocal makeprg=javac\ % errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
autocmd FileType java :nmap <silent> <F5> :make<CR>
autocmd FileType java :nmap <silent> <F6> :!java -ea %<<CR>
autocmd FileType tex :nmap <silent> <F5> :!xelatex %<CR>
autocmd FileType tex :nmap <silent> <F6> :!evince %<.pdf<CR>

function! s:SetHaskellCompiler()
  if glob("*.cabal") != ''
    compiler cabal
    nmap <silent> <F5> :make build<CR>
  else
    compiler ghc
    nmap <silent> <F5> :make<CR>
  endif
endfunction

autocmd FileType haskell,lhaskell :setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell,lhaskell :setlocal softtabstop=2
autocmd FileType haskell,lhaskell :call s:SetHaskellCompiler()
autocmd FileType haskell,lhaskell :nmap <F4> :GhcModLintAsync<CR>
autocmd FileType haskell,lhaskell :nmap <silent> <F6> :!./%<<CR>
autocmd FileType haskell,lhaskell :nmap <S-Tab> :GhcModType<CR>
autocmd FileType haskell,lhaskell :cmap tc<CR> GhcModTypeClear<CR>
autocmd FileType haskell,lhaskell :cmap exp<CR> GhcModExpand<CR>
autocmd FileType haskell,lhaskell :let b:surround_{char2nr("c")} = "{- \r -}"
autocmd FileType haskell,lhaskell :setlocal commentstring=--\ %s

autocmd FileType scala :nmap <F4> :!fsc -shutdown<CR>
autocmd FileType scala :nmap <silent> <F5> :make<CR>
autocmd FileType scala :nmap <silent> <F6> :!scala Main<CR>
autocmd FileType scala :setlocal makeprg=fsc\ % errorformat=%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

autocmd FileType lhaskell :nmap <silent> <F7> :setlocal filetype=pandoc<CR>
autocmd FileType pandoc :nmap <silent> <F7> :setlocal filetype=lhaskell<CR>

autocmd FileType pandoc :nnoremap <buffer> \- yyp<c-v>$r-
autocmd FileType pandoc :nnoremap <buffer> \= yyp<c-v>$r=
autocmd FileType pandoc :let b:surround_{char2nr("i")} = "_\r_"
autocmd FileType pandoc :let b:surround_{char2nr("b")} = "**\r**"

autocmd FileType pandoc,text :setlocal dict=/usr/share/dict/words

nmap <buffer> <c-a> ggVG
imap <buffer> <c-a> <ESC>ggVG
vmap <buffer> <c-c> "+y

nnoremap <silent> qj :cnext<CR>
nnoremap <silent> qk :cprev<CR>
nnoremap <silent> qq :cc<CR>
nnoremap <silent> qo :copen<CR>
nnoremap <silent> qc :cclose<CR>
nnoremap <silent> qm :make<CR>
nnoremap qM :make<Space>

nnoremap <silent> tj :tabprevious<CR>
nnoremap <silent> tk :tabnext<CR>
nnoremap <silent> to :tabnew<CR>
nnoremap <silent> tc :tabclose<CR>

highlight WhitespaceEOL ctermbg=DarkGrey guibg=DarkGrey
highlight OverLength ctermbg=DarkGrey guibg=DarkGrey
autocmd FileType c,cpp,haskell,java match WhitespaceEOL /\s\+$/
autocmd FileType c,cpp,haskell,java 2match OverLength /\%80v.*/

cmap w!! w !sudo tee % >/dev/null

autocmd FileType cpp :imap <F9> #include <vector><CR>#include <list><CR>#include <map><CR>#include <set><CR>#include <deque><CR>#include <queue><CR>#include <stack><CR>#include <bitset><CR>#include <algorithm><CR>#include <functional><CR>#include <numeric><CR>#include <utility><CR>#include <complex><CR>#include <sstream><CR>#include <iostream><CR>#include <iomanip><CR>#include <cstdio><CR>#include <cmath><CR>#include <cstdlib><CR>#include <cstring><CR>#include <ctime><CR>#include <cassert><CR>using namespace std;<CR>
