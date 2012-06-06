
set nocompatible

syntax on
filetype plugin indent on

call pathogen#infect()
call pathogen#helptags()

set undolevels=1024
set history=1024
set wildchar=<tab>
set wildignore=*.o,*.bak,*~,*.hi,*.git,*.swp
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

if has("gui_running")
  set vb t_vb=
  set guifont=CtrlD\ 16
  set guioptions=aciML
  map <S-F1> :set guifont=CtrlD\ 16<CR>
  map <S-F2> :set guifont=Monaco\ For\ Powerline\ 10<CR>
endif

set background=dark
if has("gui_running") || &t_Co == 256
  "colorscheme herald
  let mycolors = ['badwolf', 'herald', 'molokai', 'xoria256']
  if has("gui_running")
    let mycolors += ['lucius', 'github', 'railscasts', 'solarized', 'solarized | set background=light']
  else
  endif
  exe 'colorscheme ' . mycolors[localtime() % len(mycolors)]
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
set incsearch
set ignorecase
set smartcase
set fileencodings=utf-8,gbk,cp936
set fileformats=unix,dos

set wrap
set textwidth=78
set formatoptions=qrn1

set switchbuf=useopen,split
set autoindent
set foldmethod=marker

set modelines=0
set noerrorbells

"tagbar
autocmd BufEnter * nmap <silent> <Leader>t :TagbarToggle<CR>
let g:tagbar_iconchars = ['▸', '▾']

"nerdtree
autocmd BufEnter * nmap <silent> <Leader>n :NERDTreeToggle<CR>

"ultisnips
let g:UltiSnipsEditSplit = 'vertical'
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:UltiSnipsListSnippets = "<C-E>"

"powerline
if has("gui_running") || $COLORTERM == "rxvt-xpm"
  "Only use fancy symbols with font patched urxvt
  let g:Powerline_symbols = 'fancy'
endif
let g:Powerline_stl_path_style = 'short'

"haskell related
let g:ghcmod_ghc_options=['-w']

autocmd BufNewFile *.sh 0put=\"#!/bin/bash\<nl>\"
autocmd BufNewFile *.rb 0put=\"#!/usr/bin/env ruby\<nl>\"
autocmd BufNewFile *.py 0put=\"#!/usr/bin/env python\<nl>\"
autocmd BufNewFile *.zsh 0put=\"#!/usr/bin/env zsh\<nl>\"

autocmd BufEnter COMMIT_EDITMSG set ft=gitcommit

autocmd FileType java,scale,less :nmap <silent> <F5> :make<CR>

autocmd BufWritePost *.hs,*.lhs GhcModCheckAsync
autocmd BufEnter *.hsc :setlocal filetype=haskell
autocmd BufEnter *.hsc :nmap <silent> <F5> :!hsc2hs %;ghc --make %<.hs<CR>

autocmd FileType cpp,c,java :let b:surround_{char2nr("c")} = "/* \r */"
autocmd FileType cpp,c,java :setlocal commentstring=//\ %s

autocmd FileType cpp,c :nmap <silent> <F5> :make %<<CR>
autocmd FileType cpp,c :nmap <silent> <F6> :!./%<<CR>

autocmd FileType java :setlocal makeprg=javac\ % errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
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
autocmd FileType scala :nmap <silent> <F6> :!scala Main<CR>
autocmd FileType scala :setlocal makeprg=fsc\ % errorformat=%f:%l:\ error:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

let g:vimclojure#HighlightBuiltins=1
let g:vimclojure#ParenRainbow=1
let g:vimclojure#DynamicHighlighting=1
"let vimclojure#NailgunClient = "/usr/bin/ng"
"let vimclojure#WantNailgun=1

autocmd FileType lhaskell :nmap <silent> <F7> :setlocal filetype=pandoc<CR>
autocmd FileType pandoc :nmap <silent> <F7> :setlocal filetype=lhaskell<CR>
autocmd FileType pandoc,lhaskell :UltiSnipsAddFiletypes pandoc.mkd

autocmd FileType pandoc :nnoremap <buffer> \- yyp<c-v>$r-
autocmd FileType pandoc :nnoremap <buffer> \= yyp<c-v>$r=
autocmd FileType pandoc :let b:surround_{char2nr("i")} = "_\r_"
autocmd FileType pandoc :let b:surround_{char2nr("b")} = "**\r**"

autocmd FileType pandoc,text :setlocal dict=/usr/share/dict/words

autocmd FileType css :UltiSnipsAddFiletypes css.css3
autocmd FileType less :UltiSnipsAddFiletypes css.css3.less-elements
autocmd FileType less compiler lessc

autocmd FileType vim set sw=2 sts=2
autocmd FileType haskell set sts=2

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
nnoremap <silent> th :tabprevious<CR>
nnoremap <silent> tk :tabnext<CR>
nnoremap <silent> tl :tabnext<CR>
nnoremap <silent> to :tabnew<CR>
nnoremap <silent> tc :tabclose<CR>

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

cmap W<CR> w !sudo tee % >/dev/null<CR>
autocmd FocusLost * :wa

"inoremap jj <ESC>

imap <F9> headers<Tab>
