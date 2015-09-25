
Here is my configure files, use `ln -s` to put them under `$HOME`

Install Script
--------------

* `make build` : build binaries for vim plugins like `vim-proc` and
  `YouCompleteMe`
* `make restore`: revert-back git submodules
* `make fetch`: fetch git submodules
* `make update`: update git submodules to HEAD of fetched content

Required Binary
---------------

* [dmenu](http://tools.suckless.org/dmenu/) [dzen](https://sites.google.com/site/gotmor/dzen) and 
  [yeganesh](http://dmwit.com/yeganesh/) for xmonad
* [xclip](http://sourceforge.net/projects/xclip/) and [wmctrl](http://tomas.styblo.name/wmctrl/) for urxvt plugin
* [ctags](http://ctags.sourceforge.net/) and [hasktags](https://github.com/chrisdone/hasktags)
  for tagbar plugin and tags file generation.
* [clang](http://clang.llvm.org/) for C/C++ omnicomplete and syntax check
* [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) for Haskell omnicomplete & syntax check

Licence
-------

All code which copyrighted by me are licensed under [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0),
for those I copied from other place, see the corresponding copyright claim for details.
Other works are licensed under [CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/).
