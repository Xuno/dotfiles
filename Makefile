
all: build_ghcmod build_vimproc build_necoghc build_neocompl

update:
	git submodule sync
	git submodule update
	git submodule foreach 'git fetch origin'
	git submodule foreach 'git merge origin/master'

build_ghcmod: update
	cp ghcmod-vim/autoload/ghcmod.vim .vim/autoload/
	cp ghcmod-vim/plugin/ghcmod.vim .vim/plugin/

build_vimproc: update
	cd vimproc; make -f make_gcc.mak all
	cp -r vimproc/autoload/* .vim/autoload/
	cp vimproc/doc/vimproc.txt .vim/doc/
	cp vimproc/plugin/vimproc.vim .vim/plugin/

build_necoghc: update
	cp -r neco-ghc/autoload/* .vim/autoload/

build_neocompl: update
	cp -r neocomplcache/autoload/* .vim/autoload/
	cp neocomplcache/doc/neocomplcache.txt .vim/doc/
	cp neocomplcache/plugin/neocomplcache.vim .vim/plugin/
	
