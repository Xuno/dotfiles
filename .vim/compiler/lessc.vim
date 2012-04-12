
if exists("current_compiler")
  finish
endif
let current_compiler = "lessc"

setlocal makeprg=lessc\ --no-color\ %\ /dev/null

setlocal errorformat=%m\ in\ %f:%l:%c
