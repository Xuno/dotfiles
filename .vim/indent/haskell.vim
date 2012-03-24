
if exists('b:did_indent') && b:did_indent
  finish
  endif
let b:did_indent = 1

function! haskell:indentexpr(lnum)
  let l:line = getline(a:lnum - 1)

  if l:line =~# '^\s*--'
    return -1
  endif

  if l:line =~# '^data\>.*=.\+$'
    return match(l:line, '=')
  endif

  if l:line =~# '^data\>[^=]\+$'
    return &shiftwidth
  endif

  if l:line =~# '^\(instance\|class\).*\&.*where$'
    return &shiftwidth
  endif

  if l:line =~# '\[\s[^\]]*$'
    return match(l:line, '\[')
  endif

  if l:line =~# '\s=$'
    return &shiftwidth
  endif

  if l:line =~# '\<do$'
    return &shiftwidth
  endif

  let s = match(line, '\<do\s\+\zs[^{]\|\<where\s\+\zs\w\|\<let\s\+\zs\S\|^\s*\zs|\s')
  if s > 0
    return s
  endif

  return -1
endfunction

setlocal indentexpr=haskell:indentexpr(v:lnum)
setlocal indentkeys=!^F,o,O
