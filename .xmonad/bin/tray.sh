#!/bin/sh

# set the width of trayer to 64
# the screen width is 1280, left 95% is xmobar

trayer --edge top --align right --margin 0 --width 64 --widthtype pixel --height 12 \
        --padding 1 --tint 0x000000 --transparent true &

