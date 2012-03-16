
update:
	git submodule sync
	git submodule update
	git submodule foreach 'git fetch origin'
	git submodule foreach 'git merge origin/master'
