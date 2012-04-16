
build: .vim/bundle/vimproc/autoload/vimproc_unix.so bin/ibus-disable

init:
	git submodule init

restore:
	git submodule sync
	git submodule update

update:
	git submodule foreach 'git pull origin master'

.vim/bundle/vimproc/autoload/vimproc_unix.so: .vim/bundle/vimproc/autoload/proc.c
	cd .vim/bundle/vimproc; make -f make_unix.mak

bin/ibus-disable: bin/ibus-disable.c
	cc $< -o $@ $$(pkg-config --cflags --libs ibus-1.0)

clean:
	rm -f .vim/bundle/vimproc/autoload/vimproc_unix.so
	rm -f bin/ibus-disable
