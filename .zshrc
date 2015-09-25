#!/usr/bin/env zsh

export ZSH=$HOME/.zsh/oh-my-zsh

fpath=($HOME/.zsh/zsh-completions/src $fpath)

fpath=($HOME/.zsh/custom-completions $fpath)

plugins=(archlinux extract vi-mode git docker cabal jsontools sudo systemd history-substring-search)

# grep -Ril \? . | xargs grep -RilE "(git|branch)" | wc -l
ZSH_THEMES=(simonoff dst dieter)
ZSH_THEME=$ZSH_THEMES[$RANDOM%$#ZSH_THEMES+1]
# echo Loading $ZSH_THEME

DISABLE_AUTO_UPDATE="true"
source $ZSH/oh-my-zsh.sh
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

for conf ($HOME/.zsh/*.zsh); do
    source $conf
done
