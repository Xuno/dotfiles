
nnoremap <leader>s :YcmForceCompileAndDiagnostics<CR>
nnoremap <C-j> :YcmCompleter GoTo<CR>

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
