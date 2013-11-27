
" chromium development
if filereadable("out/Debug/build.ninja")
  autocmd FileType cpp :set makeprg=ninja\ -C\ out/Debug ts=2 sts=2 sw=2
endif
