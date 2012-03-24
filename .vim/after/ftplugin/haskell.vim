
setlocal comments=s1fl:{-,mb:-,ex:-},:-- commentstring=--\ %s
setlocal formatoptions-=t formatoptions+=croql

function! haskell:indentexpr(lnum)
  let l:line = getline(a:lnum - 1)

  if l:line =~# '^data\>.*=.\+$'
    return match(l:line, '=')
  endif

  if l:line =~# '^data\>[^=]\+$'
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

  return -1
endfunction

setlocal indentexpr=haskell:indentexpr(v:lnum)
setlocal indentkeys=!^F,o,O
