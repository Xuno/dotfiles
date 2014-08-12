
if has("gui_running")
  set vb t_vb=
  set guifont=CtrlD\ 12
  set guioptions=aciML
  nmap <S-F1> :set guifont=CtrlD\ 7<CR>
  nmap <S-F2> :set guifont=CtrlD\ 10<CR>
  nmap <S-F3> :set guifont=CtrlD\ 12<CR>
  nmap <S-F4> :set guifont=Source\ Code\ Pro\ for\ Powerline\ 10<CR>
  nmap <S-F5> :set guifont=Anonymous\ Pro\ for\ Powerline\ 12<CR>

  let s:pattern = '^\(.* \)\([1-9][0-9]*\)$'
  let s:minfontsize = 6
  let s:maxfontsize = 30
  function! AdjustFontSize(amount)
    let fontname = substitute(&guifont, s:pattern, '\1', '')
    let cursize = substitute(&guifont, s:pattern, '\2', '')
    let newsize = cursize + a:amount
    if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
      let newfont = fontname . newsize
      let &guifont = newfont
    endif
  endfunction

  function! LargerFont()
    call AdjustFontSize(1)
  endfunction
  command! LargerFont call LargerFont()

  function! SmallerFont()
    call AdjustFontSize(-1)
  endfunction
  command! SmallerFont call SmallerFont()

  nmap <silent> <C-Up> :LargerFont<CR>:redraw<CR>
  nmap <silent> <C-Down> :SmallerFont<CR>:redraw<CR>
endif

