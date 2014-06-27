
nnoremap <leader>s :YcmForceCompileAndDiagnostics<CR>
nnoremap <C-j> :YcmCompleter GoTo<CR>

if !exists("g:ycm_global_ycm_extra_conf")
  let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/ycm/third_party/ycmd/examples/.ycm_extra_conf.py'
endif

let g:ycm_always_populate_location_list = 1
let g:ycm_key_invoke_completion = '<C-L>'
let g:ycm_semantic_triggers =  {
  \   'c' : ['->', '.'],
  \   'objc' : ['->', '.'],
  \   'ocaml' : ['.', '#'],
  \   'cpp,objcpp' : ['->', '.', '::'],
  \   'perl' : ['->'],
  \   'php' : ['->', '::'],
  \   'cs,java,javascript,d,vim,python,perl6,scala,vb,elixir,go,haskell' : ['.'],
  \   'ruby' : ['.', '::'],
  \   'lua' : ['.', ':'],
  \   'erlang' : [':'],
  \ }
