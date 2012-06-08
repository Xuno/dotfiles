
set background=dark
if has("gui_running") || &t_Co == 256
  let mycolors = ['badwolf', 'herald', 'molokai', 'xoria256']
  if has("gui_running")
    let mycolors += ['lucius', 'github', 'railscasts', 'solarized', 'solarized | set background=light']
  else
  endif
  exe 'colorscheme ' . mycolors[localtime() % len(mycolors)]
else
  colorscheme default
endif
