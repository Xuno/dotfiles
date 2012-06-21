
let g:clang_complete_copen=1
let g:clang_periodic_quickfix=0
let g:clang_complete_auto=0

autocmd FileType c,cpp nnoremap <Leader>s :call g:ClangUpdateQuickFix()<CR>
