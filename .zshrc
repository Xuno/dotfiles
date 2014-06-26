#!/usr/bin/env zsh

export ZSH=$HOME/.zsh/oh-my-zsh

fpath=($HOME/.zsh/zsh-completions/src $fpath)

fpath=($HOME/.zsh/custom-completions $fpath)

plugins=(archlinux extract vi-mode git)

# grep -Ril \? . | xargs grep -RilE "(git|branch)" | wc -l
ZSH_THEMES=(simonoff dst dieter)
ZSH_THEME=$ZSH_THEMES[$RANDOM%$#ZSH_THEMES+1]
# echo Loading $ZSH_THEME

source $ZSH/oh-my-zsh.sh

for conf ($HOME/.zsh/*.zsh); do
    source $conf
done

source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh

for keycode in '[' '0'; do
    bindkey "^[${keycode}A" history-substring-search-up
    bindkey "^[${keycode}B" history-substring-search-down
done
unset keycode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

