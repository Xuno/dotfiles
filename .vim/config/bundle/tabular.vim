
autocmd FileType haskell nnoremap <silent> <buffer> \= :Tabularize /[^=$/><]\zs=\ze[^=$]/<CR>
autocmd FileType haskell nnoremap <silent> <buffer> \- :Tabularize / -> /l0r0<CR>
