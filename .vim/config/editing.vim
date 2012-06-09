
set undolevels=1024
set backspace=indent,eol,start

set complete-=u
set completeopt=longest,menuone

nnoremap <buffer> <c-a> ggVG
inoremap <buffer> <c-a> <ESC>ggVG
vnoremap <buffer> <c-c> "+y
