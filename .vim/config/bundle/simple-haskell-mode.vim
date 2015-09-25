
function! s:SetHaskellCompiler()
  if glob("*.cabal") != ''
    compiler cabal
  else
    compiler ghc
  endif
endfunction

autocmd FileType haskell call s:SetHaskellCompiler()
