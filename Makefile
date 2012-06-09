
build: bin/ibus-disable

restore:
	git submodule sync
	git submodule update --init

fetch:
	git submodule foreach 'git fetch origin'

update:
	git submodule foreach 'git checkout origin/master'
	cd .vim/bundle/vim-powerline; git checkout origin/develop

summary:
	@git submodule summary | sed 's/  </  <<<<<<<<<<<</'

bin/ibus-disable: bin/ibus-disable.c
	cc $< -o $@ $$(pkg-config --cflags --libs ibus-1.0)

clean:
	rm -f bin/ibus-disable
