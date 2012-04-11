
set nocompatible

syntax on
filetype plugin indent on

set undolevels=9999
set history=9999
set wildchar=<tab>
set wildignore=*.o,*.bak,*~,*.hi,*.git
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

if has("gui_running") && executable('ibus-disable')
  autocmd InsertLeave * call IBusDisable()
  function! IBusDisable()
    let res = system('ibus-disable')
  endfunction
endif

set laststatus=2

set hlsearch
set fileencodings=utf-8,gbk,cp936
set fileformats=unix,dos
set textwidth=78

set switchbuf=useopen,split
set autoindent
set foldmethod=marker

call pathogen#infect()

command ConvertToHTML so $VIMRUNTIME/syntax/2html.vim

" ThinkPad sucks
autocmd BufEnter * imap <F1> <ESC>

"tagbar
autocmd BufEnter * nmap <silent> <F8> :TagbarToggle<CR>

"nerdtree
autocmd BufEnter * nmap <silent> <F2> :NERDTreeToggle<CR>

"ultisnips
let g:UltiSnipsEditSplit = 'vertical'
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

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
autocmd FileType haskell,lhaskell :nnoremap <silent> <buffer> \= :Tabularize /^[^=]*\zs=/<CR>
autocmd FileType haskell,lhaskell :nnoremap <silent> <buffer> \- :Tabularize / -> /l0r0<CR>
autocmd FileType haskell,lhaskell :nnoremap \l :GhciLoad<CR>
autocmd FileType haskell,lhaskell :nnoremap \s :GhciSend<Space>
autocmd FileType haskell,lhaskell :nnoremap \B :GhciSend<Space>:break<Space>
autocmd FileType haskell,lhaskell :nnoremap \S :GhciSend<Space>:step<CR>
autocmd FileType haskell,lhaskell :nnoremap \m :GhciModule<Space>
autocmd FileType haskell,lhaskell :nnoremap <silent> <buffer> \t :GhciType<CR>
autocmd FileType haskell,lhaskell :nnoremap <silent> <buffer> \i :GhciInfo<CR>
autocmd FileType haskell,lhaskell :nnoremap \q :GhciQuit<CR>

autocmd FileType scala :nmap <F4> :!fsc -shutdown<CR>
autocmd FileType scala :nmap <silent> <F5> :make<CR>
autocmd FileType scala :nmap <silent> <F6> :!scala Main<CR>
autocmd FileType scala :setlocal makeprg=fsc\ % errorformat=%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

let g:vimclojure#HighlightBuiltins=1
let g:vimclojure#ParenRainbow=1
let g:vimclojure#DynamicHighlighting=1
"let vimclojure#NailgunClient = "/usr/bin/ng"
"let vimclojure#WantNailgun=1

autocmd FileType lhaskell :nmap <silent> <F7> :setlocal filetype=pandoc<CR>
autocmd FileType pandoc :nmap <silent> <F7> :setlocal filetype=lhaskell<CR>

autocmd FileType pandoc :nnoremap <buffer> \- yyp<c-v>$r-
autocmd FileType pandoc :nnoremap <buffer> \= yyp<c-v>$r=
autocmd FileType pandoc :let b:surround_{char2nr("i")} = "_\r_"
autocmd FileType pandoc :let b:surround_{char2nr("b")} = "**\r**"

autocmd FileType pandoc,text :setlocal dict=/usr/share/dict/words

autocmd BufEnter * nmap <buffer> <c-a> ggVG
autocmd BufEnter * imap <buffer> <c-a> <ESC>ggVG
autocmd BufEnter * vmap <buffer> <c-c> "+y

autocmd BufEnter * nnoremap <silent> qj :cnext<CR>
autocmd BufEnter * nnoremap <silent> qk :cprev<CR>
autocmd BufEnter * nnoremap <silent> qq :cc<CR>
autocmd BufEnter * nnoremap <silent> qo :copen<CR>
autocmd BufEnter * nnoremap <silent> qc :cclose<CR>
autocmd BufEnter * nnoremap <silent> qm :make<CR>
autocmd BufEnter * nnoremap qM :make<Space>

autocmd BufEnter * nnoremap <silent> tj :tabprevious<CR>
autocmd BufEnter * nnoremap <silent> th :tabprevious<CR>
autocmd BufEnter * nnoremap <silent> tk :tabnext<CR>
autocmd BufEnter * nnoremap <silent> tl :tabnext<CR>
autocmd BufEnter * nnoremap <silent> to :tabnew<CR>
autocmd BufEnter * nnoremap <silent> tc :tabclose<CR>

highlight WhitespaceEOL ctermbg=DarkGrey guibg=DarkGrey
highlight OverLength ctermbg=DarkGrey guibg=DarkGrey
autocmd FileType c,cpp,haskell,java match WhitespaceEOL /\s\+$/
autocmd FileType c,cpp,haskell,java 2match OverLength /\%80v.*/

autocmd BufEnter * cmap <silent> ccc match WhitespaceEOL /\s\+$/<CR>:2match OverLength /\%80v.*/<CR>

autocmd BufEnter * cmap w!! w !sudo tee % >/dev/null

imap <F9> headers<Tab>
