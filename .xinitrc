#!/bin/sh

xrdb -load ~/.Xdefaults
xset +fp ~/.fonts
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
    e17)
        exec enlightenment_start
        ;;
    *)
        killall compton nitrogen xscreensaver
        nitrogen --restore &
        compton --backend glx --vsync opengl-swc --paint-on-overlay --glx-no-stencil &
        xscreensaver &
        exec xmonad
        ;;
esac
