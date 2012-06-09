
autocmd FileType haskell nnoremap <silent> <buffer> \= :Tabularize /^[^=]*\zs=/<CR>
autocmd FileType haskell nnoremap <silent> <buffer> \- :Tabularize / -> /l0r0<CR>
