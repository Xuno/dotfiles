#!/usr/bin/env zsh

OHMYZSH=$HOME/.zsh/oh-my-zsh

for lib ($OHMYZSH/lib/*.zsh); do
    source $lib
done

for plugin in archlinux extract vi-mode; do
    source $OHMYZSH/plugins/$plugin/$plugin.plugin.zsh
done

for conf ($HOME/.zsh/*.zsh); do
    source $conf
done

source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source $OHMYZSH/themes/re5et.zsh-theme

autoload -U compinit
compinit -i
