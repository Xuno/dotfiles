
augroup inline-formating
  autocmd!
  autocmd FileType cpp,c,objc,haskell noremap <C-K> <C-K>
  autocmd FileType cpp,c,objc,haskell inoremap <C-K> <C-K>
  if filereadable('/usr/lib/clang-format/clang-format.py')
    autocmd FileType cpp,c,objc noremap <C-K> :pyf /usr/lib/clang-format/clang-format.py<CR>
    autocmd FileType cpp,c,objc inoremap <C-K> <C-O>:pyf /usr/lib/clang-format/clang-format.py<CR>
  elseif filereadable('/usr/share/clang/clang-format.py')
    autocmd FileType cpp,c,objc noremap <C-K> :pyf /usr/share/clang/clang-format.py<CR>
    autocmd FileType cpp,c,objc inoremap <C-K> <C-O>:pyf /usr/share/clang/clang-format.py<CR>
  endif
  autocmd FileType haskell noremap <C-K> :%!stylish-haskell<CR>
  autocmd FileType haskell inoremap <C-K> <C-O>:%!stylish-haskell<CR>
augroup END
