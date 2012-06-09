
set backup
set writebackup

set backupdir=~/.vim/backup

set backupext=.bak

if !isdirectory(&backupdir)
  call mkdir(&backupdir, 'p')
endif

if has('persistent_undo')
  set undodir=~/.vim/undo
  if !isdirectory(&undodir)
    call mkdir(&undodir, 'p')
  endif
  set undofile
endif
