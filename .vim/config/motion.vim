
set hlsearch
set incsearch
set ignorecase smartcase

autocmd BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

nnoremap H ^
nnoremap L g_

nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
