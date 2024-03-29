
set autoindent

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set textwidth=78

set formatoptions=croqB

autocmd FileType pandoc,text setlocal formatoptions+=t
autocmd FileType c,cpp,java,haskell setlocal formatoptions+=l

autocmd FileType c,cpp setlocal formatoptions+=l ts=2 sts=2 sw=2 cino=g1h1+4
autocmd FileType vim setlocal ts=2 sts=2 sw=2
autocmd FileType python setlocal ts=2 sts=2 sw=2
autocmd FileType haskell setlocal sts=2
autocmd FileType make,snippets setlocal noet

autocmd FileType haskell setlocal commentstring=--%s comments=s1:{-,mb:-,ex:-},:--
autocmd FileType c,cpp,java setlocal commentstring=//%s

autocmd FileType gitcommit set tw=68 spell

autocmd FileType haskell setlocal
    \ include=^import\\s*\\(qualified\\)\\?\\s*
    \ includeexpr=substitute(v:fname,'\\.','/','g')
    \ suffixesadd=.hs,.lhs,.hsc
    \ iskeyword=@,48-57,_,',192-255


set modeline
set modelines=5
