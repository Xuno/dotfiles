
build: .vim/bundle/vimproc/autoload/vimproc_unix.so

update:
	git submodule sync
	git submodule update
	git submodule foreach 'git fetch origin'
	git submodule foreach 'git merge origin/master'

.vim/bundle/vimproc/autoload/vimproc_unix.so: .vim/bundle/vimproc/autoload/proc.c
	cd .vim/bundle/vimproc; make -f make_unix.mak
