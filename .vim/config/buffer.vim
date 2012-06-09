
set switchbuf=useopen,split

set hidden

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

inoremap <c-j> <ESC><c-w>j
inoremap <c-k> <ESC><c-w>k
inoremap <c-h> <ESC><c-w>h
inoremap <c-l> <ESC><c-w>l

nnoremap <silent> <c-p> :bprev<CR>
nnoremap <silent> <c-n> :bnext<CR>

nnoremap <silent> <Leader>q :tabprevious<CR>
nnoremap <silent> <Leader>e :tabnext<CR>
nnoremap <silent> <Leader>T :tabnew<CR>
nnoremap <silent> <Leader>Q :tabclose<CR>
