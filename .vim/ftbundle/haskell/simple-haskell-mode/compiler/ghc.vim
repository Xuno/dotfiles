
if exists("current_compiler")
  finish
endif
let current_compiler = "ghc"

setlocal makeprg=ghc\ --make\ -rtsopts\ -threaded\ %

setlocal errorformat=
                    \%-Z\ %#,
                    \%W%f:%l:%c:\ Warning:\ %m,
                    \%E%f:%l:%c:\ %m,
                    \%E%>%f:%l:%c:,
                    \%+C\ \ %#%m,
                    \%W%>%f:%l:%c:,
                    \%+C\ \ %#%tarning:\ %m,

setlocal shellpipe=2>
