
autocmd FileType cpp,c,java let b:surround_{char2nr("c")} = "/* \r */"
autocmd FileType haskell let b:surround_{char2nr("c")} = "{- \r -}"
