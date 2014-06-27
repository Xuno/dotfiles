
let g:clang_complete_copen=1
let g:clang_periodic_quickfix=0
let g:clang_complete_auto=1
let g:clang_complete_macros=1
let g:clang_snippets=0

let g:clang_use_library=1
if isdirectory('/usr/lib/llvm-3.3/lib')
  let g:clang_library_path='/usr/lib/llvm-3.3/lib'
else
  let g:clang_library_path='/usr/lib'
endif

autocmd FileType c,cpp nnoremap <Leader>s :call g:ClangUpdateQuickFix()<CR>
