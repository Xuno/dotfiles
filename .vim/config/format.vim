
set autoindent

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set textwidth=78

set formatoptions=croq

autocmd FileType pandoc,text set formatoptions+=tB
autocmd FileType c,cpp,java,haskell set formatoptions+=j

autocmd FileType vim set ts=2 sts=2 sw=2
autocmd FileType haskell set sts=2

set modeline
set modelines=5
