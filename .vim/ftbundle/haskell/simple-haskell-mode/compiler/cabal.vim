if exists("current_compiler")
  finish
endif
let current_compiler = "cabal"
if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=cabal
CompilerSet errorformat=
      \%-Gcleaning...,
      \%-GLinking\ %s,
      \%-GResolving\ dependencies...,
      \%-GConfiguring\ %s,
      \%-GPreprocessing\ executables\ for\ %s,
      \%-GBuilding\ %s,
      \%-G[%*\\d\ of\ %*\\d]\ Compiling\ %s,
      \%-G,
      \%-Z\ %#,
      \%W%f:%l:%c:\ Warning:\ %m,
      \%E%f:%l:%c:\ %m,
      \%E%>%f:%l:%c:,
      \%+C\ \ %#%m,
      \%W%>%f:%l:%c:,
      \%+C\ \ %#%tarning:\ %m,

