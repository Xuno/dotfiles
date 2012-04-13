
if exists("current_compiler")
  finish
endif
let current_compiler = "lessc"

setlocal makeprg=lessc\ --no-color\ --x\ %\ %<.css

setlocal errorformat=%m\ in\ %f:%l:%c
