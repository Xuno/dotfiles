
set switchbuf=useopen,split

set hidden

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <silent> <Leader>t :tabnew<CR>
nnoremap <silent> <Leader>T :tabclose<CR>

autocmd VimResized * wincmd =
