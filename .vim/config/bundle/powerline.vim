
set laststatus=2

if has("gui_running") || $COLORTERM == "rxvt-xpm"
  let g:Powerline_symbols = 'fancy'
endif

let g:Powerline_stl_path_style = 'short'
call Pl#Theme#InsertSegment('ws_marker', 'after', 'lineinfo')
