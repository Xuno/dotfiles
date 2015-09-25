
if [ -e $HOME/.profile ]; then
  source $HOME/.profile
fi

PS1='`a=$?;if [ $a -ne 0 ]; then a="  "$a; echo -ne "\[\e[s\e[1A\e[$((COLUMNS-2))G\e[31m\e[1;41m${a:(-3)}\e[u\]\[\e[0m\e[7m\e[2m\]"; fi`\[\e[1;32m\]\u@\h:\[\e[0m\e[1;34m\]\W\[\e[1;34m\]\$ \[\e[0m\]'
PS1="\[\033[G\]$PS1"

alias rm="rm -i"
alias ls='ls --color=auto'
alias split2flac='shntool split -o flac -t "%n.%t"'
alias voicerec='rec -c 2 -r 44100 -s -t wav - | oggenc -q 5 --raw --raw-chan=2 --raw-rate=44100 --raw-bits=16 -o out.ogg -'
alias screenrec='recordmydesktop --no-sound --on-the-fly-encoding --fps 3'
