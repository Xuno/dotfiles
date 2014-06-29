#!/usr/bin/env zsh

if [ -e $HOME/.default-profile ]; then
  source $HOME/.default-profile
fi

if [ -e $HOME/.profile ]; then
  source $HOME/.profile
fi

