
Here is my configure files, use `ln -s` to put them under `$HOME`

Install Script
--------------

* `make build` : build binary for `vim-proc` plugin and `ibus-disable`
* `make restore`: revert-back git submodules
* `make fetch`: fetch git submodules
* `make update`: update git submodules to HEAD of fetched content

Required Binary
---------------

* [dmenu](http://tools.suckless.org/dmenu/) [dzen](https://sites.google.com/site/gotmor/dzen) and 
  [yeganesh](http://dmwit.com/yeganesh/) for xmonad
* [xclip](http://sourceforge.net/projects/xclip/) and [wmctrl](http://tomas.styblo.name/wmctrl/) for urxvt plugin
* [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) for Haskell plugin (omnicomplete, compile check and hlint)
* [ctags](http://ctags.sourceforge.net/) and [lushtags](https://github.com/bitc/lushtags) for tagbar vim plugin
* (optional) [nailgun](http://sourceforge.net/projects/nailgun/) for Clojure plugin (REPL environment, omnicomplete)
* (optional) [pandoc](http://johnmacfarlane.net/pandoc/) for vim-pandoc plugin (document generetion)
* [WenQuanYi Micro Hei (Mono) font](http://wenq.org/index.cgi?MicroHei) for Chinese display

TODO
----

* ctags config
* omnicompl for Jave
* integrate gdb, cscope, ack with vim
* try emacs, mutt, uzbl, darcs
* try nix/nixos
