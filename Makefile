
build: .vim/bundle/vimproc/autoload/vimproc_unix.so bin/ibus-disable bin/update-tags

restore:
	git submodule sync
	git submodule update --init

fetch:
	git submodule foreach 'git fetch origin'

update:
	git submodule foreach 'git checkout origin/master'

summary:
	@git submodule summary | sed 's/  </  <<<<<<<<<<<</'

.vim/bundle/vimproc/autoload/vimproc_unix.so: .vim/bundle/vimproc/autoload/proc.c
	cd .vim/bundle/vimproc; make -f make_unix.mak

bin/ibus-disable: bin/ibus-disable.c
	cc $< -o $@ $$(pkg-config --cflags --libs ibus-1.0)

bin/update-tags: bin/update-tags.hs
	ghc --make $< -o $@ -O2

clean:
	rm -f .vim/bundle/vimproc/autoload/vimproc_unix.so
	rm -f bin/ibus-disable
	rm -f bin/update-tags
