
set switchbuf=useopen,split

set hidden

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <silent> <Leader>w :tabnew<CR>
nnoremap <silent> <Leader>W :tabclose<CR>
nnoremap <silent> [w :tabprev<CR>
nnoremap <silent> ]w :tabnext<CR>
nnoremap <silent> [W :tabfirst<CR>
nnoremap <silent> ]W :tablast<CR>

autocmd VimResized * wincmd =
