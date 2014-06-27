
" chromium development
if filereadable("out/Debug/build.ninja")
  let g:clang_compilation_database = 'out/Debug'
  let g:chromium_compdb = g:clang_compilation_database.'/compile_commands.json'
  let g:chromium_build_compdb_cmd = "ninja -C ".g:clang_compilation_database.
      \" -t compdb cxx objc objcxx > ".g:chromium_compdb
  if !filereadable(g:chromium_compdb)
    execute "!".g:chromium_build_compdb_cmd
  endif
  set makeprg=ninja\ -C\ out/Debug
endif
