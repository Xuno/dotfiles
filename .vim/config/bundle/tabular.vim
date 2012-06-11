
augroup HaskellTabular
  autocmd!
  autocmd FileType haskell nnoremap <silent> <buffer> \= :Tabularize /[^=$/><]\zs=\ze[^=$]/<CR>
  autocmd FileType haskell nnoremap <silent> <buffer> \-> :Tabularize / -> /l0r0<CR>
  autocmd FileType haskell nnoremap <silent> <buffer> \<- :Tabularize / <- /l0r0<CR>
  autocmd FileType haskell nnoremap <silent> <buffer> \:: :Tabularize / :: /l0r0<CR>
  autocmd FileType haskell nnoremap <silent> <buffer> \-- :Tabularize /--/<CR>
augroup END
