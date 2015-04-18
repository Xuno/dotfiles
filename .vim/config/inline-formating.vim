
augroup inline-formating
  autocmd!
  autocmd FileType cpp,c,objc,haskell noremap <C-K> <C-K>
  autocmd FileType cpp,c,objc,haskell inoremap <C-K> <C-K>

  " Find the script in ugly way.
  let s:scripts = [
        \ '/usr/lib/clang-format/clang-format.py',
        \ '/usr/share/clang/clang-format.py',
        \ getcwd() . '/src/buildtools/clang_format/script/clang-format.py',
        \ getcwd() . '/buildtools/clang_format/script/clang-format.py',
        \ getcwd() . '/../buildtools/clang_format/script/clang-format.py',
        \ getcwd() . '/../../buildtools/clang_format/script/clang-format.py',
        \ getcwd() . '/../../../buildtools/clang_format/script/clang-format.py',
        \ ]

  for script in s:scripts
    if filereadable(script)
      execute "autocmd FileType cpp,c,objc noremap <C-K> :pyf " . script . "<CR>"
      execute "autocmd FileType cpp,c,objc inoremap <C-K> <C-O>:pyf " . script . "<CR>"
      break
    endif
  endfor

  autocmd FileType haskell noremap <C-K> :%!stylish-haskell<CR>
  autocmd FileType haskell inoremap <C-K> <C-O>:%!stylish-haskell<CR>
  autocmd FileType python noremap <C-K> :0,$!yapf --style='{based_on_style: google, indent_width: 2}'<CR>
augroup END
