
set autoindent

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set textwidth=78

set formatoptions=croqB

autocmd FileType pandoc,text setlocal formatoptions+=t
autocmd FileType c,cpp,java,haskell setlocal formatoptions+=lj

autocmd FileType vim setlocal ts=2 sts=2 sw=2
autocmd FileType haskell setlocal sts=2
autocmd FileType make setlocal noet

autocmd FileType haskell setlocal commentstring=--%s comments=s1:{-,mb:-,ex:-},:--
autocmd FileType c,cpp,java setlocal commentstring=//%s

autocmd FileType gitcommit set tw=68 spell

autocmd FileType haskell setlocal
    \ include=^import\\s*\\(qualified\\)\\?\\s*
    \ includeexpr=substitute(v:fname,'\\.','/','g')
    \ suffixesadd=.hs,.lhs,.hsc


set modeline
set modelines=5
