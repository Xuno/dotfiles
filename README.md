
Here is my configure files, use `ln -s` to put them under `$HOME`

Install Script
--------------

* `make build` : build binary for `vim-proc` plugin (required by `ghc-mod`)
* `make init`: initialize git submodules
* `make restore`: revert-back git submodules
* `make update`: update git submodules

Required Binary
---------------

* [xclip](http://sourceforge.net/projects/xclip/) and [wmctrl](http://tomas.styblo.name/wmctrl/) for urxvt plugin
* [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) for Haskell plugin (omnicomplete, compile check and hlint)
* [ctags](http://ctags.sourceforge.net/) and [lushtags](https://github.com/bitc/lushtags) for tagbar vim plugin
* (optional) [nailgun](http://sourceforge.net/projects/nailgun/) for Clojure plugin (REPL environment, omnicomplete)
* (optional) [pandoc](http://johnmacfarlane.net/pandoc/) for vim-pandoc plugin (document generetion)

TODO
----

* tags and omnicompl suppport for C++/Java
* tmux/screen config
* xmonad config
