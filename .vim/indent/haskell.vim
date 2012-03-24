
if exists('b:did_indent') && b:did_indent
  finish
  endif
let b:did_indent = 1

function! haskell#indent()
  let l:prev_lnum = line('.')-1
  let l:prev_line = getline(l:prev_lnum)
  let l:current_lnum = line('.')
  let l:current_line = getline('.')

  if current_line =~# '^\s*$'
    return s:on_newline()
  endif

  if l:current_line =~# '^\s*{' && l:prev_line =~# '^\<data\>'
    return &shiftwidth
  endif

  if l:current_line =~# '^\s*in'
    let l:lnum = l:prev_lnum
    while l:lnum >= 1
      if getline(l:lnum) =~# '\<let\>'
        let l:pos = match(getline(l:lnum), '\<let\>')
        return l:pos
      end
      let l:lnum -= 1
    endwhile
    return indent(l:current_lnum) - &shiftwidth
  endif

  if l:current_line =~# '^\s*|'
    if l:prev_line =~# '^\s*|'
      return indent(l:prev_lnum)
    elseif l:prev_line =~# '^\<data\>'
      return match(l:prev_line, '=')
    elseif l:prev_line =~# '\[\s[^\]]*$'
      return match(l:prev_line, '\[')
    else
      return indent(l:prev_lnum) + &shiftwidth
    endif
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

  " ....... do
  "     ^
  if l:line =~# '\<do$'
    return indent(l:lnum) + &shiftwidth
  endif

  " the first token after do/where/let begins a new block by indent in haskell
  " syntax
  let s = match(l:line, '\<do\s\+\zs[^{]\|\<where\s\+\zs\w\|\<let\s\+\zs\S\|^\s*\zs|\s')
  if s > 0
    return s
  endif

  " <space>where
  "          ^
  let s = match(l:line, '^\s*\zs\<\where$')
  if s > 0
    return s + 2
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

  return indent(l:lnum)
endfunction

setlocal indentexpr=haskell#indent()
setlocal indentkeys=!^F,o,O,0{,0=in,0=\|
