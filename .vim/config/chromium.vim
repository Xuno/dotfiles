
" chromium development

if filereadable("out/Debug/build.ninja")
  set makeprg=ninja\ -C\ out/Debug
endif

if filereadable("tools/vim/chromium.ycm_extra_conf.py")
  let g:ycm_global_ycm_extra_conf = 'tools/vim/chromium.ycm_extra_conf.py'
endif

if filereadable("tools/vim/filetypes.vim")
  so tools/vim/filetypes.vim
endif

if filereadable("tools/vim/ninja-build.vim")
  so tools/vim/ninja-build.vim
  map <Leader>C :CrCompileFile<cr>
endif
