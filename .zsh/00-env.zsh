#!/usr/bin/env zsh

if [ -e $HOME/.profile ]; then
  source $HOME/.profile
fi

if [ -e $HOME/.local-profile ]; then
  source $HOME/.local-profile
fi
