
let g:ghcmod_ghc_options=['-w']
let g:ghcmod_ghc_options+=['-fwarn-duplicate-exports']
let g:ghcmod_ghc_options+=['-fwarn-incomplete-patterns']
let g:ghcmod_ghc_options+=['-fwarn-incomplete-uni-patterns']
let g:ghcmod_ghc_options+=['-fwarn-missing-fields']
let g:ghcmod_ghc_options+=['-fwarn-overlapping-patterns']
let g:ghcmod_ghc_options+=['-fwarn-name-shadowing']
let g:ghcmod_ghc_options+=['-fwarn-unused-imports']
let g:ghcmod_ghc_options+=['-fwarn-unused-matches']
let g:ghcmod_hlint_options=['--ignore=Use camelCase']

augroup GhcModVim
  autocmd!
  autocmd FileType haskell nnoremap <S-Tab> :GhcModType<CR>
  autocmd FileType haskell cabbr htc GhcModTypeClear
  autocmd FileType haskell cabbr hexp GhcModExpand
  autocmd FileType haskell nnoremap <Leader>s :GhcModCheckAsync<CR>
  autocmd FileType haskell nnoremap <Leader>S :GhcModLintAsync<CR>
augroup END
