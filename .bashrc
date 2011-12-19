PS1='`a=$?;if [ $a -ne 0 ]; then a="  "$a; echo -ne "\[\e[s\e[1A\e[$((COLUMNS-2))G\e[31m\e[1;41m${a:(-3)}\e[u\]\[\e[0m\e[7m\e[2m\]"; fi`\[\e[1;32m\]\u@\h:\[\e[0m\e[1;34m\]\W\[\e[1;34m\]\$ \[\e[0m\]'
AWT_TOOLKIT=MToolkit
alias rm="rm -i"
alias ls='ls --color=auto'
alias split2flac='shntool split -o flac -t "%n.%t"'
alias mplayer-fb='mplayer -vo fbdev2'
alias mencoder-android='mencoder -vf dsize=800:480:2,scale=-8:-8,harddup -oac faac -faacopts mpeg=4:object=2:raw:br=128 -of lavf -lavfopts format=mp4 -ovc x264 -sws 9 -x264encopts nocabac:level_idc=30:bframes=0:bitrate=1024:threads=auto:turbo=1:global_header:threads=auto:subq=5:frameref=6:partitions=all:trellis=1:chroma_me:me=umh'
alias voicerec='rec -c 2 -r 44100 -s -t wav - | oggenc -q 5 --raw --raw-chan=2 --raw-rate=44100 --raw-bits=16 -'


vman () {
	export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
				  vim -R -c 'set ft=man nomod nolist' -c 'map q :q<CR>' \
				  -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
				  -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""
	man $1
	unset PAGER
}

#dos2unix() { awk "{ sub(\"\r\$\", \"\"); print }" $1 > $2; }
#unix2dos() { awk "sub(\"\$\", \"\r\")" $1 > $2; }

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

if [ "$TERM" = "linux" ]; then
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

if [ "$TERM" = "xterm" ]; then
    export TERM=xterm-256color
fi

export PATH=$PATH:~/bin:$HOME/src/arm-code-sourcey/arm-2010.09/bin:$HOME/.cabal/bin:/opt/android-sdk/platform-tools:/usr/share/java/apache-ant/bin
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

    echo "./$problem $RTS < $testcase.in | tee $testcase.out"
    time ./$problem $RTS < $testcase.in | tee $testcase.out
}

export RTS="+RTS -K512m -A8m -N2"

T () {
    for inp in *.in; do
        cas=${inp%.in}
        echo Test Case \#$cas
        $@ < $cas.in > output
        if [ $? == 0 ]; then
            diff -b output $cas.out > /dev/null
            if [ $? == 0 ]; then
                echo -e "\e[0;32mPassed\e[0m"
            else
                echo -en "\e[0;33m"
                cat $cas.in
                echo -en "\e[0;34m"
                cat $cas.out
                echo -en "\e[0;36m"
                cat output
                echo -e "\e[0;31mNot Match\e[0m"
            fi
        else
            echo -e "\e[0;31mFailed\e[0m"
        fi
    done
}
