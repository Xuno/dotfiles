#!/bin/sh

xrdb -load ~/.Xdefaults
xset +fp ~/.fonts
xset +fp ~/.fonts/ctrld
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b
xsetroot -cursor_name left_ptr

export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export XMODIFIERS="@im=fcitx"
fcitx

case $1 in
    xfce4)
        exec startxfce4
        ;;
    *)
        nitrogen --restore &
        killall xcompmgr
        xcompmgr &
        xscreensaver &
        exec xmonad
        ;;
esac
