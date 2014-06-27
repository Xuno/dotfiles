
if filereadable('/usr/lib/clang-format/clang-format.py')
  noremap <C-K> :pyf /usr/lib/clang-format/clang-format.py<CR>
  inoremap <C-K> <C-O>:pyf /usr/lib/clang-format/clang-format.py<CR>
elseif filereadable('/usr/share/clang/clang-format.py')
  noremap <C-K> :pyf /usr/share/clang/clang-format.py<CR>
  inoremap <C-K> <C-O>:pyf /usr/share/clang/clang-format.py<CR>
endif
