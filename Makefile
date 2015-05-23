
build: bin/update-tags vim-plugins

restore:
	git submodule sync
	git submodule update --init --recursive
	git submodule foreach --recursive 'git clean -f -f -d'

fetch:
	git submodule foreach 'git fetch origin'

update:
	git submodule foreach --recursive 'git clean -f -f -d'
	git submodule foreach 'git reset --hard origin/master && git submodule sync && git submodule update --init --recursive'

summary:
	@git submodule summary | sed 's/  </  <<<<<<<<<<<</'

vim-plugins:
	cd .vim/bundle/vimproc; make -f make_unix.mak
	cd .vim/bundle/ycm; ./install.sh --clang-completer --system-libclang --gocode-completer

bin/update-tags: bin/update-tags.hs
	ghc --make $< -o $@ -O2

clean:
	rm -f .vim/bundle/vimproc/autoload/vimproc_*.so
	rm -f bin/update-tags
