
Here is my configure files, use `ln -s` to put them under `$HOME`

Install Script
--------------

1. `make build` : build binary for `vim-proc` plugin (required by `ghc-mod`)
2. `make restore`: initialize/revert-back git submodules (mostly vim plugins)
3. `make update`: update git submodules

Required Binary
---------------

1. [xclip](http://sourceforge.net/projects/xclip/), [wmctrl](http://tomas.styblo.name/wmctrl/) for urxvt plugin
2. [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) for Haskell plugin (omnicomplete, compile check and hlint)
3. (optional) [nailgun](http://sourceforge.net/projects/nailgun/) for Clojure plugin (REPL environment, omnicomplete)
4. (optional) [pandoc](http://johnmacfarlane.net/pandoc/) for vim-pandoc plugin (document generetion)
