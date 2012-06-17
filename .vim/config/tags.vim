
function! UPDATE_TAGS()
  let _f_ = vimproc#shellescape(expand("%:p"))
  let _update_tags = vimproc#get_command_name("update-tags")
  let _resp = vimproc#system(_update_tags . ' ' . _f_)
endfunction

if executable("update-tags")
  autocmd BufWritePost,FileWritePost *.c,*.h,*.cpp,*.java,*.hs,*.lhs
    \ call UPDATE_TAGS()
endif
