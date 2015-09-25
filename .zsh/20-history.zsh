#!/usr/bin/env zsh

export HISTSIZE=20000
export SAVEHIST=$HISTSIZE

setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS

for keycode in '[' '0'; do
    bindkey "^[${keycode}A" history-substring-search-up
    bindkey "^[${keycode}B" history-substring-search-down
done
unset keycode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
