#!/bin/sh

xrdb -merge .Xdefaults
xset -b

case $1 in
    xfce4)
        exec startxfce4
        ;;
    *)
        xmonad
        ;;
esac
