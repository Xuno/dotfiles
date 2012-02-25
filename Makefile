
all: build_ghcmod build_vimproc build_necoghc

build_ghcmod:
	cp ghcmod-vim/autoload/ghcmod.vim .vim/autoload/
	cp ghcmod-vim/plugin/ghcmod.vim .vim/plugin/

build_vimproc:
	cd vimproc; make -f make_gcc.mak all
	cp -r vimproc/autoload/* .vim/autoload/
	cp vimproc/doc/vimproc.txt .vim/doc/
	cp vimproc/plugin/vimproc.vim .vim/plugin/
build_necoghc:
	cp -r neco-ghc/autoload/* .vim/autoload
