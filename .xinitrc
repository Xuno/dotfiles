#!/bin/sh

xrdb -load ~/.Xdefaults
xset -b
xsetroot -cursor_name left_ptr

case $1 in
    xfce4)
        exec startxfce4
        ;;
    *)
        nitrogen --restore &
        ~/.xmonad/bin/ibus.sh
        ~/.xmonad/bin/tray.sh
        xmonad
        ;;
esac
