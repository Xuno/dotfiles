
let g:ghcmod_ghc_options=['-Wall']
let g:ghcmod_hlint_options=['--ignore=Use camelCase']

augroup GhcModVim
  autocmd!
  if filereadable("xmonad.hs") && isdirectory("lib")
    let b:ghcmod_ghc_options = ['-ilib']
  endif
  autocmd FileType haskell nnoremap <S-Tab> :GhcModType<CR>
  autocmd FileType haskell nnoremap <Tab> :GhcModInfo<CR>
  autocmd FileType haskell nnoremap <Leader>T :GhcModTypeInsert<CR>
  autocmd FileType haskell cabbr htc GhcModTypeClear
  autocmd FileType haskell cabbr hexp GhcModExpand
  autocmd FileType haskell nnoremap <Leader>s :GhcModCheckAsync<CR>
  autocmd FileType haskell nnoremap <Leader>S :GhcModLintAsync<CR>
augroup END
