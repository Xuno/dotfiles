txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset
txtbld='\e[1m'    # Text Bold

# Check for an interactive session
[ -z "$PS1" ] && return

PROMPT_COMMAND='RET=$?;'
RET_COLOR='$(if [[ $RET == 0 ]]; then echo -ne "$txtgrn"; else echo -ne "$txtred"; fi;)'
#PS1="[\u@\h \W]$RET_COLOR\$$txtrst "
#PS1='[\u@\h \W]\$ '
PS1='`a=$?;if [ $a -ne 0 ]; then a="  "$a; echo -ne "\[\e[s\e[1A\e[$((COLUMNS-2))G\e[31m\e[1;41m${a:(-3)}\e[u\]\[\e[0m\e[7m\e[2m\]"; fi`\[\e[1;32m\]\u@\h:\[\e[0m\e[1;34m\]\W\[\e[1;34m\]\$ \[\e[0m\]'
AWT_TOOLKIT=MToolkit
export PATH=$PATH:/opt/android-sdk/platform-tools
alias rm="rm -i"
alias ls='ls --color=auto'
alias split2flac='shntool split -o flac -t "%n.%t"'
alias mplayer-fb='mplayer -vo fbdev2'
alias mencoder-android='mencoder -vf dsize=800:480:2,scale=-8:-8,harddup -oac faac -faacopts mpeg=4:object=2:raw:br=128 -of lavf -lavfopts format=mp4 -ovc x264 -sws 9 -x264encopts nocabac:level_idc=30:bframes=0:bitrate=1024:threads=auto:turbo=1:global_header:threads=auto:subq=5:frameref=6:partitions=all:trellis=1:chroma_me:me=umh'


vman () {
	export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
				  vim -R -c 'set ft=man nomod nolist' -c 'map q :q<CR>' \
				  -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
				  -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""
	man $1
	unset PAGER
}

#dos2unix() { awk "{ sub(\"\r\$\", \"\"); print }" $1 > $2; }
unix2dos() { awk "sub(\"\$\", \"\r\")" $1 > $2; }

extract () {
  if [ -f $1 ] ; then
      case $1 in
          *.tar.bz2)   tar xvjf $1    ;;
          *.tar.gz)    tar xvzf $1    ;;
          *.tar.xz)    tar xvJf $1    ;;
          *.bz2)       bunzip2 $1     ;;
          *.rar)       unrar x $1     ;;
          *.gz)        gunzip $1      ;;
          *.tar)       tar xvf $1     ;;
          *.tbz2)      tar xvjf $1    ;;
          *.tgz)       tar xvzf $1    ;;
          *.zip)       unzip $1       ;;
          *.Z)         uncompress $1  ;;
          *.7z)        7z x $1        ;;
          *.xz)        unxz $1        ;;
          *.exe)       cabextract $1  ;;
          *)           echo "\`$1': unrecognized file compression" ;;
      esac
  else
      echo "\`$1' is not a valid file"
  fi
}

if [ "$TERM" = "linux__" ]; then
	echo -en "\e]P0444444"
	echo -en "\e]P1ee6363"
	echo -en "\e]P273ba3e"
	echo -en "\e]P3baa33e"
	echo -en "\e]P43e73ba"
	echo -en "\e]P5ac3885"
	echo -en "\e]P63ebaba"
	echo -en "\e]P7dddddd"
	echo -en "\e]P8333333"
	echo -en "\e]P9cc4444"
	echo -en "\e]PA4d8728"
	echo -en "\e]PB877d28"
	echo -en "\e]PC284d87"
	echo -en "\e]PD812973"
	echo -en "\e]PE288787"
	echo -en "\e]PFaaaaaa"
	clear
fi
export PATH=$PATH:~/bin:/home/crazyb0y/src/arm-code-sourcey/arm-2010.09/bin:/opt/go/bin:/home/crazyb0y/.cabal/bin:/opt/jhc/usr/bin
export JHC_LIBRARY_PATH=/opt/jhc/usr/share/jhc-0.7
export DOWNLOAD_PATH=$HOME/Desktop

gcj () {

    [ -e $DOWNLOAD_PATH/*.in ] || return

    file=`ls $DOWNLOAD_PATH/*.in | head -n1`
    echo "Filename $file"

    problem=`basename $file | sed 's/^\([A-Z]\)-.*\.in/\1/g'`
    testcase=`basename $file | sed 's/^\(.*\)\.in/\1/g'`
    echo "Problem $problem"

    read -p $'Is these information okay? (y/n)\n' -n 1
    [ "$REPLY" == "y" ] || return

    echo

    mv "$file" .

    echo "./$problem $RTS < $testcase.in > $testcase.out"
    time ./$problem $RTS < $testcase.in > $testcase.out
}
