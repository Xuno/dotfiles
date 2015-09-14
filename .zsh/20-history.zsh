#!/usr/bin/env zsh

export HISTSIZE=20000
export SAVEHIST=$HISTSIZE

setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
