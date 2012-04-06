
build: .vim/bundle/vimproc/autoload/vimproc_unix.so .vim/binary/ibus-disable

restore:
	git submodule sync
	git submodule update

update:
	git submodule foreach 'git fetch origin'
	git submodule foreach 'git merge origin/master'

.vim/bundle/vimproc/autoload/vimproc_unix.so: .vim/bundle/vimproc/autoload/proc.c
	cd .vim/bundle/vimproc; make -f make_unix.mak

.vim/binary/ibus-disable: .vim/binary/ibus-disable.c
	cd .vim/binary; make ibus-disable
