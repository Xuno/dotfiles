
if exists('b:did_indent') && b:did_indent
  finish
  endif
let b:did_indent = 1

function! haskell#indent()
  let l:prev_lnum = line('.')-1
  let l:prev_line = getline(l:prev_lnum)
  let l:current_lnum = line('.')
  let l:current_line = getline('.')

  if l:current_line =~# '^\s*$'
    return s:on_newline()
  endif

  " data Person = 
  "     { age :: Int
  "     ...
  if l:current_line =~# '^\s*{' && l:prev_line =~# '^\<data\>'
    return &shiftwidth
  endif

  " a :: b
  "   -> c
  if l:current_line =~# '^\s*\(->\|=>\)' && l:prev_line =~# '::'
    return match(l:prev_line, '::')
  endif

  " let xxx
  "     xxx
  " in
  "
  " if ...
  " then ...
  " else ...
  if l:current_line =~# '^\s*\(in\>\|then\>\|else\)'
    if l:current_line =~# '^\s*in\>'
      let l:expect = '\<let\>'
    elseif l:current_line =~# '^\s*then\>'
      let l:expect = '\<if\>'
    else
      let l:expect = '\<then\>'
    endif
    let l:lnum = l:prev_lnum
    let l:min_indent = 999
    while l:lnum >= 1 && l:lnum >= l:prev_lnum - 99 && l:min_indent > 0
      let l:cline = getline(l:lnum)
      let l:expect_pos = match(l:cline, l:expect)
      if l:expect_pos >= 0 && l:expect_pos < l:min_indent
        return l:expect_pos
      end
      let l:cindent = match(l:cline, '^\s*\zs\S')
      if l:cindent >= 0 && l:cindent < l:min_indent
        let l:min_indent = l:cindent
      end
      let l:lnum -= 1
    endwhile
  endif

  " data A = B
  "        | C
  "        | D
  "
  "
  "      [ i
  "      | i <- [1..10]
  "      , even i
  "      ]
  "
  " funcName n
  "     | even n    = ...
  "     | otherwise = ...
  "
  if l:current_line =~# '^\s*|'
    if l:prev_line =~# '^\s*|'
      return indent(l:prev_lnum)
    elseif l:prev_line =~# '^\<data\>'
      return match(l:prev_line, '=')
    elseif l:prev_line =~# '\[\s[^\]]*$'
      return match(l:prev_line, '\[')
    elseif l:prev_line =~# '|'
      return match(l:prev_line, '|')
    else
      return indent(l:prev_lnum) + &shiftwidth
    endif
  endif

  let s = match(l:prev_line, '\<do\s\+\zs[^{]\|\<where\s\+\zs\w\|\<let\s\+\zs\S')
  if s > 0
    return s
  endif

  return indent(l:current_lnum)
endfunction

function! s:on_newline()
  let l:lnum = line('.')-1
  let l:line = getline(l:lnum)

  " ignore comments
  if l:line =~# '^\s*--'
    return -1
  endif

  " data xxx =
  "          ^
  if l:line =~# '^data\>.*=.\+$'
    return match(l:line, '=')
  endif

  " data xxx
  "     ^
  if l:line =~# '^data\>[^=]\+$'
    return &shiftwidth
  endif

  " instance/class xxx whee
  "     ^
  if l:line =~# '^\(instance\|class\).*\&.*where$'
    return &shiftwidth
  endif

  " ..... = [ ...
  "         ^
  if l:line =~# '\[\s[^\]]*$'
    return match(l:line, '\[')
  endif

  " ....... =
  "     ^
  if l:line =~# '\s=\s*$'
    return indent(l:lnum) + &shiftwidth
  endif

  " <space>where/else/then
  "          ^
  let s = match(l:line, '^\s*\zs\(\<\where\|\<then\|\<else\)$')
  if s > 0
    return s + &softtabstop
  endif

  " ....... do/of/then/else
  "     ^
  if l:line =~# '\<do$\|\<of$\|\<then$\|\<else$'
    return indent(l:lnum) + &shiftwidth
  endif

  " xxxxxx let
  "            ^
  let s = match(l:line, '\zs\<let$')
  if s > 0
    return s + &shiftwidth
  endif

  " case or lambda
  if l:line =~# '->\s*$'
    return indent(l:lnum) + &shiftwidth
  endif

  " deriving
  if l:line =~# '^\s*\<deriving\>'
    return 0
  endif

  return -1
endfunction

setlocal indentexpr=haskell#indent()
setlocal indentkeys=!^F,o,O,0{,0=in,0=then,e,0=\|,0=\-\>,0=\=\>
