
set switchbuf=useopen,split

set hidden

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <silent> [b :bprev<CR>
nnoremap <silent> ]b :bnext<CR>

nnoremap <silent> [t :tabprev<CR>
nnoremap <silent> ]t :tabnext<CR>

nnoremap <silent> <Leader>to :tabnew<CR>
nnoremap <silent> <Leader>tc :tabclose<CR>
nnoremap <silent> <Leader>tf :tabfirst<CR>
nnoremap <silent> <Leader>tl :tablast<CR>

autocmd VimResized * wincmd =
