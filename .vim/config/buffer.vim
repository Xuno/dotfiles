
set switchbuf=usetab,split

set hidden

nnoremap <silent> <Leader>w :tabnew<CR>
nnoremap <silent> <Leader>W :tabclose<CR>
nnoremap <silent> [w :tabprev<CR>
nnoremap <silent> ]w :tabnext<CR>
nnoremap <silent> [W :tabfirst<CR>
nnoremap <silent> ]W :tablast<CR>

autocmd VimResized * wincmd =
