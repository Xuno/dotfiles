
let g:clang_complete_copen=1
let g:clang_periodic_quickfix=1

autocmd FileType c,cpp nnoremap <Leader>s :call g:ClangUpdateQuickFix()<CR>
